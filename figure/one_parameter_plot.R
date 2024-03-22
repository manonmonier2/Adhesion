rm(list = ls())

library("config")
library("ggplot2")
library("dplyr")
library("rcompanion")
library("agricolae")
library("FSA")
library("ggpubr")
library("DescTools")
library("ggtext")
library("doBy")
library("ggrepel")
library("reshape2")
library("gridExtra")
library("pivottabler") ### doc : http://www.pivottabler.org.uk/articles/v04-regularlayout.html
library("extrafont")
# font_import()
loadfonts(device = "win")

#### FUNCTIONS ####

n_fun <- function(data){
  y_pos = min(data) + (max(data) - min(data)) * 0.1
  return(data.frame(y = y_pos, label = paste0("n = ",length(data))))
}


make_stat = function(data, factor_name, parameter){
  
  data[[factor_name]] = as.factor(data[[factor_name]])
  test_stat = c()
  
  # shapiro
  res_shapiro_global = shapiro.test(data[, which(colnames(data) == parameter)])
  shapiro_global_handler = res_shapiro_global$p.value >= 0.01
  test_stat = c(test_stat, shapiro_global_handler)
  
  
  # bartlett
  res_bartlett = bartlett.test(x = data[, which(colnames(data) == parameter)], 
                               g = data[, which(colnames(data) == factor_name)], 
                               data = data)
  bartlett_reject = res_bartlett$p.value >= 0.01 #si p value superieure a 0.01 on accepte H0 donc variance egales entre protocoles
  test_stat = c(test_stat, bartlett_reject)
  
  # anova
  aov_res = aov(data[[parameter]] ~ data[[factor_name]])
  anova_res = anova(aov_res)
  anova_handler = anova_res$`Pr(>F)`[1] >= 0.01
  
  test_stat = c(test_stat, anova_handler)
  
  # kruskal wallis
  kruskal_res = kruskal.test(data[[parameter]] ~ data[[factor_name]])
  kruskal_handler = kruskal_res$p.value >= 0.01
  
  test_stat = c(test_stat, anova_handler)
  
  names(test_stat) = c("shapiro", "bartlett", "anova", "kruskal-wallis")
  
  # Tukey
  tukey_res = TukeyHSD(aov_res, conf.level = 0.99)
  HSD_res = HSD.test(aov_res, "data[[factor_name]]", group = T)
  tukey_group = HSD_res$groups
  tukey_group = cbind(rownames(tukey_group), tukey_group[, -1])#on extrait les noms de ligne et on les place dans une nouvelle colonne à gauche avec cbind
  #puis on append le tableau de resultats tukey_group auquel on retire les moyennes en colonne 1
  colnames(tukey_group) = c(factor_name, "groups")
  tukey_group = as.data.frame(tukey_group)
  
  
  # Dunn
  dunn_res = dunnTest(data[[parameter]] ~ data[[factor_name]])
  
  dunn_group = cldList(P.adj ~ Comparison, threshold = 0.01, data = dunn_res$res, 
                       remove.zero = F,
                       remove.space = T)
  dunn_group = dunn_group[, -3]
  colnames(dunn_group) = c(factor_name, "groups")
  # correction of dunn group name (add space)
  for(true_name in unique(data[[factor_name]])){
    dunn_name = gsub(" ", "", true_name)
    dunn_group[[factor_name]][which(dunn_group[[factor_name]] == dunn_name)] = true_name
  }
  dunn_group = dunn_group[order(dunn_group$groups), ]#order donne la position des valeurs non ordonnees apres ordre alphabetique
  #order est donne pour lignes car on veut ordonner lignes
  ###
  
  used_test = NA
  if (test_stat["shapiro"] & test_stat["bartlett"]){
    data_test = tukey_group
    used_test = "Tukey"
  } else {
    data_test = dunn_group
    used_test = "Dunn"
  }
  return(data_test)
}


reorder_by_factor = function(data, factor_name, fun, parameter) {
  order_data = data %>%
    group_by(!!as.symbol(factor_name)) %>% 
    filter(!is.na(!!as.symbol(parameter))) %>%
    filter(is.finite(!!as.symbol(parameter))) %>%
    summarise(fun = 
                do.call(fun, list(x = (!!as.symbol(parameter)))))
  order_data = as.data.frame(order_data[order(order_data$fun), ])
  
  return(order_data[[factor_name]])
}


format_label = function(factor_name, factor_labels, stat_group = NULL, n_data = NULL) {
  if (factor_name == "Species"){
    
    a_col = levels(factor_labels)
    a_col = gsub("_", " ", a_col, fixed = T)
    a_col = gsub("Drosophila", "D.", a_col, fixed = T)
    a_col = gsub("Megaselia", "M.", a_col, fixed = T)
    a_col = gsub("Scaptodrosophila", "S.", a_col, fixed = T)
    a_col = gsub("Zaprionus", "Z.", a_col, fixed = T)
    a_col = substr(a_col, 1, 8)
    #a_col = paste0("*", a_col, "*")
    a_col = StrAlign(a_col, sep = "\\l")
  } else {
    a_col = StrAlign(levels(factor_labels), sep = "\\r")
  }
  
  if (length(stat_group) > 0){
    ordered_groups = unlist(lapply(levels(stat_group[[factor_name]]),
                                   function(x) {
                                     stat_group[stat_group[[factor_name]] == x, ]$groups
                                   }))
    group_diversity = sort(unique(unlist(lapply(ordered_groups, function(x) {
      return(base::unlist(strsplit(x, split = "")))
    }))))
    
    edited_groups = unlist(lapply(ordered_groups, function(g) {
      base::paste(unlist(lapply(group_diversity, function(x) {
        if (grepl(x, g)) {
          return(x)
        } else {
          return(" ")
        }
      })), collapse = "")
    }))
    
    
    b_col = edited_groups
  } else {
    b_col = ""
  }
  
  if (length(n_data) > 0){
    
    n_count = unlist(lapply(levels(n_data[[factor_name]]), 
                            function(x) {
                              sum(n_data[[factor_name]] == x)
                            }))
    c_col = StrAlign(paste0(n_count), sep = "\\r")
  } else {
    c_col = ""
  }
  
  labels =  paste(a_col,
                  b_col,
                  c_col,
                  sep = "   ")
  return(labels)
}

####

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "portable")

# retrieve parameters
# Input
path_index = opt$index_path
path_batch_by_id = opt$batch_by_id
plot_path = opt$plot_path
path_integral = opt$integral_path

parameter_list = gsub(" +$", "", 
                      gsub("^ +", "", unlist(strsplit(opt$parameter_list, ","))))
lab_list = gsub(" +$", "", 
                gsub("^ +", "", unlist(strsplit(opt$lab_list, ","))))
unit_list = gsub(" +$", "",
                 gsub("^ +", "", unlist(strsplit(opt$unit_list, ","))))
stat_list = gsub(" +$", "", 
                 gsub("^ +", "", unlist(strsplit(opt$stat_list, ","))))

comment_list = gsub(" +$", "", 
                    gsub("^ +", "", unlist(strsplit(opt$comment_list, ","))))

comment_lab_list = gsub(" +$", "", 
                        gsub("^ +", "", unlist(strsplit(opt$comment_lab_list, ","))))

# read data figure
gg_data = read.table(paste0(plot_path, "/data_figure.csv"), 
                     stringsAsFactors = F,
                     sep = "\t",
                     header = T)

species_list = unique(gg_data$Species)
stock_list = unique(gg_data$Stock)
protocol_list = unique(gg_data$Protocol)


# one parameter plot
## D. melanogaster protocols

plot_path_one_parameter_by_protocol_and_species = paste0(plot_path, "/one_parameter/by_protocol_and_species/")
dir.create(plot_path_one_parameter_by_protocol_and_species, showWarnings = FALSE, recursive = T)


manual_order = ordered(c( 
  "no tape", "1 tape ; detached", 
  "2 tapes ; detached", "3 d", "1 strong tape",
  "speed /3", "speed x3" , "5 min", 
  "0 s", "0.25 N" , "standard"))

list_plot = list()


for (i in 1:length(parameter_list)){
  temp_data = gg_data %>% 
    filter(Comment == "ok" & 
             Species == "Drosophila_melanogaster" & 
             Protocol != "water" & 
             Protocol != "1 tape ; detached ; speed x3") %>%
    filter(!is.na(!!as.symbol(parameter_list[[i]]))) %>%
    filter(is.finite(!!as.symbol(parameter_list[[i]])))
  
  temp_data_all_comment = gg_data %>% 
    filter(Species == "Drosophila_melanogaster" & 
             Stock == "cantonS" & Protocol != "water" & 
             Protocol != "1 tape ; detached ; speed x3") %>%
    filter(Comment == "ok" | Comment == "cuticle_broke" | 
             Comment == "not_detached") %>%
    filter(!is.na(!!as.symbol(parameter_list[[i]])))
  
  
  gg_data_test = make_stat(data = temp_data, 
                           factor_name = "Protocol", 
                           parameter = parameter_list[i])
  
  # reorder all the data in the same way
  # protocol_order = reorder_by_factor(data = temp_data,
  #                                    factor_name = "Protocol",
  #                                    fun = "median",
  #                                    parameter = parameter_list[1])
  
  protocol_order = manual_order
  
  gg_data_test$Protocol =
    factor(gg_data_test$Protocol, levels = protocol_order, ordered = T)
  temp_data$Protocol =
    factor(temp_data$Protocol, levels = protocol_order,  ordered = T)
  temp_data_all_comment$Protocol =
    factor(temp_data_all_comment$Protocol, levels = protocol_order,
           ordered = T)
  
  
  
  x_labels = format_label(factor_name = "Protocol",
                          factor_labels = gg_data_test[["Protocol"]],
                          stat_group = gg_data_test,
                          n_data = temp_data_all_comment)
  
  
  p = ggplot(temp_data,
             aes_string(x = "Protocol", y = parameter_list[i])) +
    geom_point(colour = "black", shape = 20, size = 2, stroke = 1) +
    geom_boxplot(width= 0.4, colour= "black", outlier.colour = "grey", fill = NA) +
    theme_bw(base_size = 22) +
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5), 
          axis.text.x = element_text(family = "Courier New"), 
          axis.text.y= element_text(family = "Courier New"),
          axis.title.y = element_blank()) +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    scale_x_discrete(labels = x_labels) +
    coord_flip()
  
  ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/", parameter_list[i], "_Drosophila_melanogaster", ".pdf"),
         plot=p, width=16, height=8, device = cairo_pdf)
  
  if (! grepl("^log10_", parameter_list[i])){
    list_plot[[parameter_list[i]]] = p
  }
  
}

p1 = ggarrange(list_plot[[5]], list_plot[[6]], list_plot[[4]], 
               nrow = 3, 
               common.legend = T, 
               align = c("v"), 
               labels = c("A", "B", "C"), 
               font.label=list(color="black",size=30))

p2 = ggarrange(list_plot[[1]], list_plot[[3]], list_plot[[2]], 
               nrow = 3, 
               common.legend = T, 
               align = c("v"), 
               labels = c("D", "E", "F"), 
               font.label=list(color="black",size=30))

p3 = ggarrange(list_plot[[3]], list_plot[[2]], 
               nrow = 2,
               common.legend = T, 
               align = c("v"), 
               labels = c("A", "B"), 
               font.label=list(color="black",size=30))

p4 = ggarrange(list_plot[[4]], list_plot[[5]],
               nrow = 2,
               common.legend = T, 
               align = c("v"), 
               labels = c("A", "B"), 
               font.label=list(color="black",size=30))

p = ggarrange(p1, p2, ncol = 2, common.legend = T, align = c("v"))

ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/all_parameters_Drosophila_melanogaster", ".pdf"), 
       plot=p, width=40, height=30, device = cairo_pdf)

ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/position_Drosophila_melanogaster", ".pdf"), 
       plot=p1, width=20, height=30, device = cairo_pdf)

ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/force_energy_Drosophila_melanogaster", ".pdf"), 
       plot=p2, width=20, height=30, device = cairo_pdf)

ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/two_energies_Drosophila_melanogaster", ".pdf"), 
       plot=p3, width=17, height=15, device = cairo_pdf)

ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/rigidity_dr_Drosophila_melanogaster", ".pdf"), 
       plot=p4, width=20, height=30, device = cairo_pdf)


## by species
plot_path_one_parameter_by_species = paste0(plot_path, "/one_parameter/by_species/")
dir.create(plot_path_one_parameter_by_species, showWarnings = FALSE, recursive = T)

list_plot = list()
list_plot_log = list()
for (i in 1:length(parameter_list)){
  
  temp_gg_data = gg_data  %>%
    filter(Species != "Megaselia_abdita") %>%
    filter(Species != "Megaselia_scalaris") %>%
    filter(Species != "Drosophila_elegans") %>%
    filter(Species != "Drosophila_quadraria") %>%
    filter(
      
      ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
         (Species == "Drosophila_hydei" & Protocol == "1 strong tape ; 0.25 N") |
         (Species == "Drosophila_suzukii" & Stock == "WT3") |
         (Species == "Drosophila_biarmipes" & Stock == "G224") |
         (Species == "Drosophila_simulans" & Stock == "simulans_vincennes"))
      |
        (! Species %in% c("Drosophila_melanogaster", 
                          "Drosophila_suzukii", 
                          "Drosophila_biarmipes", 
                          "Drosophila_simulans", 
                          "Drosophila_hydei"))
    )
  
  if (parameter_list[i] %in% c("Glue_area", "log10_glue_area", "detachment_force_div_glue_area", "glue_area_div_pupa_area",
                               "log10_glue_area_mm", "log10_detachment_force_div_glue_area", "log10_na(glue_area_div_pupa_area)", 
                               "log10_glue_area_div_pupa_area")) {
    temp_data_species = temp_gg_data %>%
      filter(Comment == "ok") %>%
      filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
      filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
      group_by(Species) %>%
      filter(length(!!as.symbol(parameter_list[i])) > 1)
    
    temp_data_all_comment = temp_gg_data %>%
      filter(Comment == "ok" | Comment == "cuticle_broke" | Comment == "not_detached") %>%
      filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
      filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
      group_by(Species) %>%
      filter(length(!!as.symbol(parameter_list[i])) > 1)
    
  } else {
    
    temp_data_species = temp_gg_data %>%
      filter(Comment == "ok") %>%
      filter((Protocol == "1 strong tape ; 0.25 N" | Protocol == "standard")) %>%
      filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
      group_by(Species) %>%
      filter(length(!!as.symbol(parameter_list[i])) > 1)
    
    temp_data_all_comment = temp_gg_data %>%
      filter(Comment == "ok" | Comment == "cuticle_broke" | Comment == "not_detached") %>%
      filter((Protocol == "1 strong tape ; 0.25 N" | Protocol == "standard")) %>%
      filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
      group_by(Species) %>%
      filter(length(!!as.symbol(parameter_list[i])) > 1)
  }
  
  temp_data_species = as.data.frame(temp_data_species)
  
  gg_data_test_species = make_stat(data = temp_data_species,
                                   factor_name = "Species",
                                   parameter = parameter_list[i])
  
  temp_data_species %>%
    filter(Species == "Drosophila_rhopaloa") %>%
    nrow()
  
  temp_data_all_comment %>%
    filter(Species == "Drosophila_rhopaloa") %>%
    nrow()
  
  # reorder species for the plot
  species_order = reorder_by_factor(data = temp_data_species, 
                                    factor_name = "Species", 
                                    fun = "median", 
                                    parameter = parameter_list[1])
  
  temp_data_species$Species = factor(temp_data_species$Species,
                                     levels = species_order,
                                     ordered = T)
  
  temp_data_all_comment$Species = factor(temp_data_all_comment$Species,
                                         levels = species_order,
                                         ordered = T)
  
  gg_data_test_species$Species = factor(gg_data_test_species$Species,
                                        levels = species_order,
                                        ordered = T)
  
  x_labels = format_label(factor_name = "Species",
                          factor_labels = gg_data_test_species[["Species"]],
                          stat_group = gg_data_test_species,
                          n_data = temp_data_all_comment)
  
  #plot
  p = ggplot(temp_data_species,
             aes_string(x = "Species", y = parameter_list[i])) +
    geom_boxplot(width= 0.4, colour= "black", outlier.colour = "grey") + 
    geom_jitter(position=position_dodge(0.5)) +
    scale_shape_manual(values = c(3, 4)) +
    scale_color_manual(values = rep(1, 8)) +
    theme_bw(base_size = 18) +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    xlab("Species") +
    coord_flip() + 
    scale_x_discrete(labels = x_labels) +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_text(family = "Courier New"),
          axis.text.y = element_text(family = "Courier New"))
  
  # recuperation de stat ggplot avec ggplot_build()
  df_res = ggplot_build(p)$data[[1]]
  
  ggsave(file = paste0(plot_path_one_parameter_by_species, "/", parameter_list[i], ".pdf"), 
         plot=p, width=16, height=8, device = cairo_pdf)
  
  if (! grepl("^log10_", parameter_list[i])){
    list_plot[[parameter_list[i]]] = p
  }
  #list_plot_log[[parameter_list[i]]] = p
}


#recuperation mediane detachment force par especes
stat = aggregate(gg_data$detachment_force ~ gg_data$Species, data = gg_data, median)
stat = as.data.frame(stat)
write.csv(stat, "/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_22_03_23/data/stat_by_species.csv", row.names=FALSE)

p1 = ggarrange(list_plot[[5]], list_plot[[6]], list_plot[[4]], 
               nrow = 3, 
               common.legend = T, 
               align = c("v"), 
               labels = c("A", "B", "C"), 
               font.label=list(color="black",size=30))

p2 = ggarrange(list_plot[[3]], list_plot[[2]], 
               nrow = 3, 
               common.legend = T, 
               align = c("v"), 
               labels = c("A", "B"), 
               font.label=list(color="black",size=30))


p = ggarrange(p2, p1, ncol = 2, common.legend = T, align = c("v"))

ggsave(file = paste0(plot_path_one_parameter_by_species, "/all_parameters_all_species", ".pdf"), 
       plot=p, width=30, height=20, device = cairo_pdf)

ggsave(file = paste0(plot_path_one_parameter_by_species, "/position_all_species", ".pdf"), 
       plot=p1, width=20, height=30, device = cairo_pdf)

ggsave(file = paste0(plot_path_one_parameter_by_species, "/all_energy_all_species", ".pdf"), 
       plot=p2, width=20, height=30, device = cairo_pdf)


p3 = ggarrange(plotlist = list_plot_log[10:12], nrow = 3, common.legend = T, align = c("v"), labels = c("A", "B", "C"))
ggsave(file = paste0(plot_path_one_parameter_by_species, "/log_force_energy_all_species", ".pdf"), 
       plot=p3, width=30, height=10, device = cairo_pdf)

## by stock
plot_path_one_parameter_by_stock = paste0(plot_path, "/one_parameter/by_stock/")
dir.create(plot_path_one_parameter_by_stock, showWarnings = FALSE, recursive = T)

list_plot = list()
for (i in 1:length(parameter_list)){
  list_plot[[parameter_list[i]]] = list()
  for(focus in c("Drosophila_suzukii", "Drosophila_biarmipes", 
                 "Drosophila_simulans")) {
    focus_lab = gsub("_", " ", focus, fixed = T)
    focus_lab = gsub("Drosophila", "D.", focus_lab, fixed = T)
    focus_lab = gsub("Megaselia", "M.", focus_lab, fixed = T)
    focus_lab = gsub("Scaptodrosophila", "S.", focus_lab, fixed = T)
    focus_lab = gsub("Zaprionus", "Z.", focus_lab, fixed = T)
    focus_lab = StrAlign(focus_lab, sep = "\\l")
    focus_lab = substr(focus_lab, 1, 8)
    if (parameter_list[i] %in% c("Glue_area", "log10_glue_area", "detachment_force_div_glue_area", "glue_area_div_pupa_area",
                                 "log10_glue_area_mm", "log10_detachment_force_div_glue_area", "log10_na(glue_area_div_pupa_area)")) {
      temp_data_stock = gg_data %>%
        filter(Species == focus) %>%
        filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
        filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
        filter(Stock != "Japan") %>%
        group_by(Stock) %>%
        filter(length(!!as.symbol(parameter_list[i])) > 1)
      
      temp_data_all_comment = temp_data_stock
      
    } else {
      temp_data_stock = gg_data %>%
        filter(Comment == "ok") %>%
        filter(Protocol == "standard") %>%
        filter(Species == focus) %>%
        filter(Stock != "Japan") %>%
        filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
        filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
        group_by(Stock) %>%
        filter(length(!!as.symbol(parameter_list[i])) > 1)
      
      temp_data_all_comment = gg_data %>%
        filter(Comment == "ok" | Comment == "cuticle_broke" | 
                 Comment == "not_detached") %>%
        filter(Protocol == "standard") %>%
        filter(Stock != "Japan") %>%
        filter(Species == focus) %>%
        filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
        filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
        group_by(Stock) %>%
        filter(length(!!as.symbol(parameter_list[i])) > 1)
    }
    
    temp_data_stock = as.data.frame(temp_data_stock)
    
    # no data for this species for this parameter
    if (nrow(temp_data_stock) == 0) {
      p = ggplot(temp_data_stock,
                 aes_string(x = "Stock", y = parameter_list[i])) +
        geom_boxplot(width= 0.4, colour= "black", outlier.colour = "grey") + 
        geom_jitter(position=position_dodge(0.5)) +
        scale_shape_manual(values = c(3, 4)) +
        scale_color_manual(values = rep(1, 8)) +
        theme_bw(base_size = 18) +
        ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
        xlab(focus_lab) +
        coord_flip() +
        theme(axis.text.x = element_text(family = "Courier New"),
              axis.text.y= element_text(family = "Courier New"))
      
      list_plot[[parameter_list[i]]][[focus]] = p
      next
    }
    
    # 1 or 2 stocks for this species for this parameter
    if (length(unique(temp_data_stock$Stock)) <= 2){
      gg_data_test_stock = data.frame("Stock" = unique(temp_data_stock$Stock),
                                      "Groups" = rep("", length(unique(temp_data_stock$Stock))))
    } else {
      gg_data_test_stock = make_stat(data = temp_data_stock,
                                     factor_name = "Stock",
                                     parameter = parameter_list[i])
    }
    
    # reorder species for the plot
    stock_order = reorder_by_factor(data = temp_data_stock, 
                                    factor_name = "Stock", 
                                    fun = "median", 
                                    parameter = parameter_list[i])
    
    temp_data_stock$Stock = factor(temp_data_stock$Stock,
                                   levels = stock_order,
                                   ordered = T)
    
    temp_data_all_comment$Stock = factor(temp_data_all_comment$Stock,
                                         levels = stock_order,
                                         ordered = T)
    
    gg_data_test_stock$Stock = factor(gg_data_test_stock$Stock,
                                      levels = stock_order,
                                      ordered = T)
    
    x_labels = format_label(factor_name = "Stock",
                            factor_labels = gg_data_test_stock[["Stock"]],
                            stat_group = gg_data_test_stock,
                            n_data = temp_data_all_comment)
    
    #plot
    p = ggplot(temp_data_stock,
               aes_string(x = "Stock", y = parameter_list[i])) +
      geom_boxplot(width= 0.4, colour= "black", outlier.colour = "grey") + 
      geom_jitter(position=position_dodge(0.5)) +
      scale_shape_manual(values = c(3, 4)) +
      scale_color_manual(values = rep(1, 8)) +
      theme_bw(base_size = 18) +
      ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
      xlab(focus_lab) +
      coord_flip() + 
      scale_x_discrete(labels = x_labels) +
      theme(axis.text.x = element_text(family = "Courier New"),
            axis.text.y= element_text(family = "Courier New"))
    
    list_plot[[parameter_list[i]]][[focus]] = p
  }
  p = ggarrange(plotlist = list_plot[[parameter_list[i]]],
                nrow = 3, 
                common.legend = T,
                align = c("v"), 
                labels = c("A", "B", "C"))
  
  df_res = ggplot_build(p)$data[[1]]
  
  ggsave(file = paste0(plot_path_one_parameter_by_stock, "/", parameter_list[i], 
                       "_stock", ".pdf"), 
         plot=p, 
         width=15, 
         height=20, 
         device = cairo_pdf)
}

### réduction du graph force divided by glue area
plot_path_one_parameter_normalisation = paste0(plot_path, "/one_parameter/normalisation/")
dir.create(plot_path_one_parameter_normalisation, showWarnings = FALSE, recursive = T)

list_plot = list()
list_plot_log = list()

i = 12

  temp_gg_data = gg_data  %>%
    filter(Comment == "ok") %>%
    filter(Species != "Megaselia_abdita") %>%
    filter(Species != "Megaselia_scalaris") %>%
    filter(Species != "Drosophila_elegans") %>%
    filter(Species != "Drosophila_quadraria") %>%
    filter(   
      ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
         (Species == "Drosophila_hydei" & Protocol == "1 strong tape ; 0.25 N") |
         (Species == "Drosophila_suzukii" & Stock == "WT3") |
         (Species == "Drosophila_biarmipes" & Stock == "G224") |
         (Species == "Drosophila_simulans" & Stock == "simulans_vincennes"))
      |
        (! Species %in% c("Drosophila_melanogaster", 
                          "Drosophila_suzukii", 
                          "Drosophila_biarmipes", 
                          "Drosophila_simulans", 
                          "Drosophila_hydei"))
    ) %>%
  filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
    filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
    group_by(Species) 

  
  temp_gg_data = as.data.frame(temp_gg_data)
  
  
  temp_data_all_comment = gg_data  %>%
    filter(Comment == "ok" | Comment == "cuticle_broke" | Comment == "not_detached") %>%
    filter(Species != "Megaselia_abdita") %>%
    filter(Species != "Megaselia_scalaris") %>%
    filter(Species != "Drosophila_elegans") %>%
    filter(Species != "Drosophila_quadraria") %>%
    filter(
      ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
         (Species == "Drosophila_hydei" & Protocol == "1 strong tape ; 0.25 N") |
         (Species == "Drosophila_suzukii" & Stock == "WT3") |
         (Species == "Drosophila_biarmipes" & Stock == "G224") |
         (Species == "Drosophila_simulans" & Stock == "simulans_vincennes"))
      |
        (! Species %in% c("Drosophila_melanogaster", 
                          "Drosophila_suzukii", 
                          "Drosophila_biarmipes", 
                          "Drosophila_simulans", 
                          "Drosophila_hydei"))
    ) %>%
    filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
    filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
    group_by(Species) 
  
  temp_data_all_comment = as.data.frame(temp_data_all_comment)
  
  gg_data_test_species = make_stat(data = temp_gg_data,
                                   factor_name = "Species",
                                   parameter = parameter_list[i])
  
  
  # reorder species for the plot
  species_order = reorder_by_factor(data = temp_gg_data, 
                                    factor_name = "Species", 
                                    fun = "median", 
                                    parameter = parameter_list[1])
  
  temp_gg_data$Species = factor(temp_gg_data$Species,
                                     levels = species_order,
                                     ordered = T)
  
  temp_data_all_comment$Species = factor(temp_data_all_comment$Species,
                                         levels = species_order,
                                         ordered = T)
  
  gg_data_test_species$Species = factor(gg_data_test_species$Species,
                                        levels = species_order,
                                        ordered = T)
  

  #plot
  threshold = quantile(temp_gg_data[["detachment_force_div_glue_area"]], probs = seq(0, 1, 0.05))[20]
  
  sub <- subset(temp_gg_data, detachment_force_div_glue_area < threshold)
  
  x_labels_sub = format_label(factor_name = "Species",
                              factor_labels = gg_data_test_species[["Species"]],
                              stat_group = gg_data_test_species,
                              n_data = temp_data_all_comment)
  
  p_sub = ggplot(sub,
                 aes_string(x = "Species", y = "detachment_force_div_glue_area")) +
    geom_boxplot(width= 0.4, colour= "black", outlier.colour = "grey") + 
    geom_jitter(position=position_dodge(0.5)) +
    scale_shape_manual(values = c(3, 4)) +
    scale_color_manual(values = rep(1, 8)) +
    theme_bw(base_size = 18) +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    xlab("Species") +
    coord_flip() + 
    scale_x_discrete(labels = x_labels_sub) +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_text(family = "Courier New"),
          axis.text.y= element_text(family = "Courier New"))
  
  ggsave(file = paste0(plot_path_one_parameter_normalisation, "/", parameter_list[i], "_sub_95.pdf"), 
         plot=p_sub, width=16, height=8, device = cairo_pdf)
  
  

## normalisation by glue area - other comments
plot_path_one_parameter_normalisation = paste0(plot_path, "/one_parameter/normalisation_other_comments/")
dir.create(plot_path_one_parameter_normalisation, showWarnings = FALSE, recursive = T)

list_plot = list()
list_plot_log = list()

i = 12
  
temp_gg_data = gg_data  %>%
  filter(Comment == "cuticle_broke" | Comment == "not_detached") %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_elegans") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_funebris") %>% #because only one pupa
  filter(Species != "Zaprionus_indianus") %>% #because only one pupa
  filter(Species != "Drosophila_malerkotliana") %>% #because only two pupa
  
  filter(
    
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_hydei" & Protocol == "1 strong tape ; 0.25 N") |
       (Species == "Drosophila_suzukii" & Stock == "WT3") |
       (Species == "Drosophila_biarmipes" & Stock == "G224") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes"))
    |
      (! Species %in% c("Drosophila_melanogaster", 
                        "Drosophila_suzukii", 
                        "Drosophila_biarmipes", 
                        "Drosophila_simulans", 
                        "Drosophila_hydei"))
  ) %>%
  filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
  filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%

  group_by(Species)

temp_gg_data = temp_gg_data %>%
filter(! is.na(parameter_list[[i]])) %>%
  filter(is.finite(!!as.symbol(parameter_list[[i]])))

temp_gg_data = as.data.frame(temp_gg_data)


gg_data_test_species = make_stat(data = temp_gg_data,
                                 factor_name = "Species",
                                 parameter = parameter_list[i])


# reorder species for the plot
species_order = reorder_by_factor(data = temp_gg_data, 
                                  factor_name = "Species", 
                                  fun = "median", 
                                  parameter = parameter_list[1])

temp_gg_data$Species = factor(temp_gg_data$Species,
                              levels = species_order,
                              ordered = T)

gg_data_test_species$Species = factor(gg_data_test_species$Species,
                                      levels = species_order,
                                      ordered = T)



#plot
threshold = quantile(temp_gg_data[["detachment_force_div_glue_area"]], probs = seq(0, 1, 0.05))[20]

sub <- subset(temp_gg_data, detachment_force_div_glue_area < threshold)

x_labels_sub = format_label(factor_name = "Species",
                            factor_labels = gg_data_test_species[["Species"]],
                            stat_group = gg_data_test_species,
                            n_data = sub)


p = ggplot(sub,
               aes_string(x = "Species", y = "detachment_force_div_glue_area")) +
  geom_boxplot(width= 0.4, colour= "black", outlier.colour = "grey") + 
  geom_jitter(position=position_dodge(0.5)) +
  scale_shape_manual(values = c(3, 4)) +
  scale_color_manual(values = rep(1, 8)) +
  theme_bw(base_size = 18) +
  ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
  xlab("Species") +
  coord_flip() + 
  scale_x_discrete(labels = x_labels_sub) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Courier New"),
        axis.text.y= element_text(family = "Courier New"))

ggsave(file = paste0(plot_path_one_parameter_normalisation, "/", parameter_list[i], "cuticle_broke_not_detached.pdf"), 
       plot=p, width=16, height=8, device = cairo_pdf)


#recuperation mediane detachment force par especes
stat = aggregate(temp_data_species$div_glue_area ~ temp_data_species$Species, data = temp_data_species, median)
stat = as.data.frame(stat)
write.csv(stat, "/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_22_03_23/data/stat_all_comment_normalisation_by_species.csv", row.names=FALSE)


ggsave(file = paste0(plot_path_one_parameter_normalisation, "/all_parameters_all_species", ".pdf"), 
       plot=p, width=30, height=20, device = cairo_pdf)





### STATS

plot_path_one_parameter_by_species = paste0(plot_path, "/one_parameter/by_species/")
dir.create(plot_path_one_parameter_by_species, showWarnings = FALSE, recursive = T)

# graph scatter plot number of ok and not ok
comment_stats_ok = gg_data %>%
  filter(
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes") |
       (Species == "Drosophila_suzukii" & Stock == "suzukii_Vincennes") |
       (Species == "Drosophila_biarmipes" & Stock == "G224")) |
      (! Species %in% c("Drosophila_melanogaster", "Drosophila_simulans", "Drosophila_suzukii", "Drosophila_biarmipes"))) %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_elegans") %>%
  
  filter(Comment == "ok" | Comment == "not_detached" | Comment == "cuticle_broke") %>%
  
  filter(Protocol == "standard") %>%
  
  select(Species, Comment) %>%
  group_by(Species) %>% 
  summarise(ok = sum(Comment == 'ok'))

comment_stats_ok = as.data.frame(comment_stats_ok)


comment_stats_cuticle_broke = gg_data %>%
  filter(
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes") |
       (Species == "Drosophila_suzukii" & Stock == "suzukii_Vincennes") |
       (Species == "Drosophila_biarmipes" & Stock == "G224")) |
      (! Species %in% c("Drosophila_melanogaster", "Drosophila_simulans", "Drosophila_suzukii", "Drosophila_biarmipes"))) %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_elegans") %>%
  
  filter(Comment == "ok" | Comment == "not_detached" | Comment == "cuticle_broke") %>%
  
  filter(Protocol == "standard") %>%
  
  select(Species, Comment) %>%
  group_by(Species) %>% 
  summarise(cuticle_broke = sum(Comment == 'cuticle_broke'))

comment_stats_cuticle_broke = as.data.frame(comment_stats_cuticle_broke)


comment_stats_not_detached = gg_data %>%
  filter(
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes") |
       (Species == "Drosophila_suzukii" & Stock == "suzukii_Vincennes") |
       (Species == "Drosophila_biarmipes" & Stock == "G224")) |
      (! Species %in% c("Drosophila_melanogaster", "Drosophila_simulans", "Drosophila_suzukii", "Drosophila_biarmipes"))) %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_elegans") %>%
  
  filter(Comment == "ok" | Comment == "not_detached" | Comment == "cuticle_broke") %>%
  
  filter(Protocol == "standard") %>%
  
  select(Species, Comment) %>%
  group_by(Species) %>% 
  summarise(not_detached = sum(Comment == 'not_detached'))

comment_stats_not_detached = as.data.frame(comment_stats_not_detached)


a = base::merge(comment_stats_ok, comment_stats_cuticle_broke, by = 'Species')
b = base::merge(a, comment_stats_not_detached, by = 'Species')

b$not_ok = b$not_detached + b$cuticle_broke
b$Total = b$not_detached + b$cuticle_broke + b$ok
b$percent_cuticle_broke = b$cuticle_broke*100/b$Total
b$percent_not_detached = b$not_detached*100/b$Total
b$percent_ok = b$ok*100/b$Total
b$percent_not_ok = b$not_ok*100/b$Total

x_labels = format_label(factor_name = "Species", factor_labels = as.factor(b[["Species"]]))
x_labels = gsub(" *$", "", x_labels)

pretty_comment = comment_lab_list
names(pretty_comment) = comment_list

legend_labels = pretty_comment[levels(d[["Comment"]])]


p_total_detached = ggplot(b, aes(x=Total, y=ok)) + 
  geom_point() + theme_bw(base_size = 18) + labs(x = "Total number of tested pupae", y = "Number of detached and not broken pupae") +
  geom_text_repel(aes(label = x_labels),
                  max.overlaps = 50,
                  size = 4,
                  segment.color = 'grey50') +
  theme(axis.text.x = element_text(family = "Courier New"),
        axis.text.y= element_text(family = "Courier New")) +
  geom_abline(slope=1, intercept = 0) +
  geom_abline(slope=0.8, intercept = 0, linetype="dotted") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 150)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 65))
p_total_detached #augmenter police


#barplot comments percentage standard protocol
comment_stats_ok = gg_data %>%
  filter(
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes") |
       (Species == "Drosophila_suzukii" & Stock == "suzukii_Vincennes") |
       (Species == "Drosophila_biarmipes" & Stock == "G224")) |
      (! Species %in% c("Drosophila_melanogaster", "Drosophila_simulans", "Drosophila_suzukii", "Drosophila_biarmipes"))) %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_elegans") %>%
  
  filter(Comment == "ok" | Comment == "not_detached" | Comment == "cuticle_broke") %>%
  
  filter(Protocol == "standard") %>%
  
  select(Species, Comment) %>%
  group_by(Species) %>% 
  summarise(ok = sum(Comment == 'ok'))

comment_stats_ok = as.data.frame(comment_stats_ok)


comment_stats_cuticle_broke = gg_data %>%
  filter(
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes") |
       (Species == "Drosophila_suzukii" & Stock == "suzukii_Vincennes") |
       (Species == "Drosophila_biarmipes" & Stock == "G224")) |
      (! Species %in% c("Drosophila_melanogaster", "Drosophila_simulans", "Drosophila_suzukii", "Drosophila_biarmipes"))) %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_elegans") %>%
  
  filter(Comment == "ok" | Comment == "not_detached" | Comment == "cuticle_broke") %>%
  
  filter(Protocol == "standard") %>%
  
  select(Species, Comment) %>%
  group_by(Species) %>% 
  summarise(cuticle_broke = sum(Comment == 'cuticle_broke'))

comment_stats_cuticle_broke = as.data.frame(comment_stats_cuticle_broke)


comment_stats_not_detached = gg_data %>%
  filter(
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes") |
       (Species == "Drosophila_suzukii" & Stock == "suzukii_Vincennes") |
       (Species == "Drosophila_biarmipes" & Stock == "G224")) |
      (! Species %in% c("Drosophila_melanogaster", "Drosophila_simulans", "Drosophila_suzukii", "Drosophila_biarmipes"))) %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_elegans") %>%
  
  filter(Comment == "ok" | Comment == "not_detached" | Comment == "cuticle_broke") %>%
  
  filter(Protocol == "standard") %>%
  
  select(Species, Comment) %>%
  group_by(Species) %>% 
  summarise(not_detached = sum(Comment == 'not_detached'))

comment_stats_not_detached = as.data.frame(comment_stats_not_detached)


c = base::merge(comment_stats_ok, comment_stats_cuticle_broke, by = 'Species')
d = base::merge(c, comment_stats_not_detached, by = 'Species')

d$not_ok = d$not_detached + d$cuticle_broke
d$Total = d$not_detached + d$cuticle_broke + d$ok
d$percent_cuticle_broke = d$cuticle_broke*100/d$Total
d$percent_not_detached = d$not_detached*100/d$Total
d$percent_ok = d$ok*100/d$Total

mdat_standard = melt(d, id.vars=c("Species"),
                     measure.vars=c("percent_ok", "percent_not_detached", "percent_cuticle_broke"))

mdat_standard = as.data.frame(mdat_standard)

pretty_comment = comment_lab_list
names(pretty_comment) = comment_list

legend_labels = pretty_comment[levels(d[["Comment"]])]

#order variables
mdat_standard$variable <- factor(mdat_standard$variable, levels=c('percent_not_detached', 'percent_cuticle_broke', 'percent_ok'))

# Change species ordering manually
mdat_standard$Species <- factor(mdat_standard$Species,
                                levels = c("Drosophila_hydei", "Drosophila_pachea",
                                           "Drosophila_nannoptera", "Drosophila_virilis",
                                           "Drosophila_immigrans", "Zaprionus_lachaisei",
                                           "Drosophila_simulans", "Drosophila_yakuba",
                                           "Drosophila_eugracilis", "Drosophila_malerkotliana",
                                           "Zaprionus_indianus", "Drosophila_ananassae",
                                           "Drosophila_funebris",
                                           "Drosophila_biarmipes", "Drosophila_suzukii",
                                           "Drosophila_kurseongensis", "Drosophila_mauritiana", 
                                           "Drosophila_melanogaster", "Drosophila_prostipennis", 
                                           "Drosophila_pseudoobscura", "Drosophila_rhopaloa", 
                                           "Drosophila_takahashii", "Drosophila_tropicalis", 
                                           "Scaptodrosophila_lebanonensis"))

mdat_standard = as.data.frame(mdat_standard)

x_labels = format_label(factor_name = "Species", factor_labels = as.factor(mdat_standard[["Species"]]))
x_labels = gsub(" *$", "", x_labels)

p_standard = ggplot(data = mdat_standard,
                    aes(x = Species, y = value, fill = variable)) + coord_flip() +
  geom_bar(position="stack", stat = "identity", colour="black", width = 0.8) + theme_bw(base_size = 18) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Courier New"),
        axis.text.y= element_text(family = "Courier New")) +
  scale_x_discrete(labels = x_labels) +
  scale_fill_manual(name = "Pupa state after detachment", labels = c("Not detached", "Cuticle broke", "Detached"), 
                    values=c('white', 'grey', 'black')) +
  ylab("Percentage of pupae after standard adhesion assay")

ggsave(file = paste0(plot_path_one_parameter_by_species, "/bar_plot_standard", ".pdf"), 
       plot=p_strong_025N, width=16, height=8, device = cairo_pdf)


#barplot comments percentage 1 strong tape ; glue ; 0.25 N protocol
comment_stats_ok = gg_data %>%
  filter(
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes") |
       (Species == "Drosophila_suzukii" & Stock == "suzukii_Vincennes") |
       (Species == "Drosophila_biarmipes" & Stock == "G224")) |
      (! Species %in% c("Drosophila_melanogaster", "Drosophila_simulans", "Drosophila_suzukii", "Drosophila_biarmipes"))) %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_elegans") %>%
  
  filter(Comment == "ok" | Comment == "not_detached" | Comment == "cuticle_broke") %>%
  
  filter(Protocol == "1 strong tape ; 0.25 N") %>%
  
  select(Species, Comment) %>%
  group_by(Species) %>% 
  summarise(ok = sum(Comment == 'ok')) 

comment_stats_ok = as.data.frame(comment_stats_ok)



comment_stats_cuticle_broke = gg_data %>%
  filter(
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes") |
       (Species == "Drosophila_suzukii" & Stock == "suzukii_Vincennes") |
       (Species == "Drosophila_biarmipes" & Stock == "G224")) |
      (! Species %in% c("Drosophila_melanogaster", "Drosophila_simulans", "Drosophila_suzukii", "Drosophila_biarmipes"))) %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_elegans") %>%
  
  filter(Comment == "ok" | Comment == "not_detached" | Comment == "cuticle_broke") %>%
  
  filter(Protocol == "1 strong tape ; 0.25 N") %>%
  
  select(Species, Comment) %>%
  group_by(Species) %>% 
  summarise(cuticle_broke = sum(Comment == 'cuticle_broke'))

comment_stats_cuticle_broke = as.data.frame(comment_stats_cuticle_broke)


comment_stats_not_detached = gg_data %>%
  filter(
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes") |
       (Species == "Drosophila_suzukii" & Stock == "suzukii_Vincennes") |
       (Species == "Drosophila_biarmipes" & Stock == "G224")) |
      (! Species %in% c("Drosophila_melanogaster", "Drosophila_simulans", "Drosophila_suzukii", "Drosophila_biarmipes"))) %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_elegans") %>%
  
  filter(Comment == "ok" | Comment == "not_detached" | Comment == "cuticle_broke") %>%
  
  filter(Protocol == "1 strong tape ; 0.25 N") %>%
  
  select(Species, Comment) %>%
  group_by(Species) %>% 
  summarise(not_detached = sum(Comment == 'not_detached'))

comment_stats_not_detached = as.data.frame(comment_stats_not_detached)


c = base::merge(comment_stats_ok, comment_stats_cuticle_broke, by = 'Species')
d = base::merge(c, comment_stats_not_detached, by = 'Species')

d$not_ok = d$not_detached + d$cuticle_broke
d$Total = d$not_detached + d$cuticle_broke + d$ok
d$percent_cuticle_broke = d$cuticle_broke*100/d$Total
d$percent_not_detached = d$not_detached*100/d$Total
d$percent_ok = d$ok*100/d$Total

mdat_strong_025N = melt(d, id.vars=c("Species"),
                        measure.vars=c("percent_ok", "percent_not_detached", "percent_cuticle_broke"))

mdat_strong_025N = as.data.frame(mdat_strong_025N)

pretty_comment = comment_lab_list
names(pretty_comment) = comment_list

legend_labels = pretty_comment[levels(d[["Comment"]])]

#order variables
mdat_strong_025N$variable <- factor(mdat_strong_025N$variable, levels=c('percent_not_detached', 'percent_cuticle_broke', 'percent_ok'))

# Change species ordering manually
mdat_strong_025N$Species <- factor(mdat_strong_025N$Species,                                    
                                   levels = c("Drosophila_pachea", "Drosophila_nannoptera",
                                              "Drosophila_littoralis", "Drosophila_virilis",
                                              "Drosophila_hydei", "Drosophila_immigrans",
                                              "Zaprionus_lachaisei"
                                              ))

mdat_strong_025N = as.data.frame(mdat_strong_025N)

x_labels = format_label(factor_name = "Species", factor_labels = as.factor(mdat_strong_025N[["Species"]]))
x_labels = gsub(" *$", "", x_labels)

p_strong_025N = ggplot(data = mdat_strong_025N,
                       aes(x = Species, y = value, fill = variable)) + coord_flip() +
  geom_bar(position="stack", stat = "identity", colour="black", width = 0.8) + theme_bw(base_size = 18) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Courier New"),
        axis.text.y= element_text(family = "Courier New")) +
  scale_x_discrete(labels = x_labels) +
  scale_fill_manual(name = "Pupa state after detachment", labels = c("Not detached", "Cuticle broke", "Detached"), 
                    values=c('white', 'grey', 'black')) +
  ylab("Percentage of pupae after '1 strong tape ; 0.25 N' adhesion assay")
# geom_text(data=subset(mdat, value != 100), aes(label = value), size = 7, position = position_stack(vjust = 0.5))

ggsave(file = paste0(plot_path_one_parameter_by_species, "/bar_plot_strong_025N", ".pdf"), 
       plot=p_strong_025N, width=16, height=8, device = cairo_pdf)


p = ggarrange(p_standard, p_strong_025N, ncol = 1, common.legend = T, align = c("v"))

p1 = ggarrange(p_total_detached, p, ncol = 1, common.legend = T, align = c("v"))


ggsave(file = paste0(plot_path_one_parameter_by_species, "/stat_bar_plot_standard_strong_025N", ".pdf"), 
       plot=p1, width=8, height=12, device = cairo_pdf)


#table melano comments
melano_stats_ok = gg_data %>%
  filter(Species == "Drosophila_melanogaster" & 
           Stock == "cantonS" & Protocol != "water" & 
           Protocol != "1 tape ; detached ; speed x3") %>%
  filter(Comment == "ok") %>%
  select(Protocol, Comment) %>%
  group_by(Protocol) %>% 
  summarise(ok = sum(Comment == 'ok'))

melano_stats_ok = as.data.frame(melano_stats_ok)

melano_stats_not_detached = gg_data %>%
  filter(Species == "Drosophila_melanogaster" & 
           Stock == "cantonS" & Protocol != "water" & 
           Protocol != "1 tape ; detached ; speed x3") %>%
  filter(Comment == "not_detached") %>%
  select(Protocol, Comment) %>%
  group_by(Protocol) %>% 
  summarise(not_detached = sum(Comment == 'not_detached'))

melano_stats_not_detached = as.data.frame(melano_stats_not_detached)

melano_stats_cuticle_broke = gg_data %>%
  filter(Species == "Drosophila_melanogaster" & 
           Stock == "cantonS" & Protocol != "water" & 
           Protocol != "1 tape ; detached ; speed x3") %>%
  filter(Comment == "cuticle_broke") %>%
  select(Protocol, Comment) %>%
  group_by(Protocol) %>% 
  summarise(cuticle_broke = sum(Comment == 'cuticle_broke'))

a = base::merge(melano_stats_ok, melano_stats_not_detached, by = 'Protocol', all = TRUE)
a[is.na(a)] <- 0
b = base::merge(a, melano_stats_cuticle_broke, by = 'Protocol', all = TRUE)
b[is.na(b)] <- 0
b <- data.frame(b)
colnames(b)[2] ="Detached"
colnames(b)[3] ="Not detached"
colnames(b)[4] ="Cuticle broke"

t1 <- ttheme_default(core=list(bg_params = list(fill="white", col = "black")))


grid.table(b, rows = NULL, theme= t1)
dev.off()




######test merge tableau
number_pupae_tested = gg_data %>%
  filter((Species == "Drosophila_simulans" & Stock == "simulans_vincennes") |
           (Species == "Drosophila_suzukii" & Stock == "suzukii_Vincennes") |
           (Species == "Drosophila_biarmipes" & Stock == "G224") |
           (! Species %in% c("Drosophila_simulans", "Drosophila_suzukii", "Drosophila_biarmipes"))) %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Drosophila_melanogaster") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(Species != "Drosophila_elegans") %>%
  filter(Comment == "ok" | Comment == "not_detached" | Comment == "cuticle_broke") %>%
  filter(Protocol != "0.25 N") %>%
  select(Species, Protocol, Comment) %>%
  group_by(Protocol)

# copy filtered data as data.frame
data = as.data.frame(number_pupae_tested)


# format species name
short_name = data$Species
short_name = gsub("_", "", short_name, fixed = T)
short_name = gsub("Drosophila", "D.", short_name, fixed = T)
short_name = gsub("Megaselia", "M.", short_name, fixed = T)
short_name = gsub("Scaptodrosophila", "S.", short_name, fixed = T)
short_name = gsub("Zaprionus", "Z.", short_name, fixed = T)
data$Species = short_name

# format comment name
temp_comment = data$Comment
temp_comment = gsub("^cuticle_broke$", "Cuticle broke", temp_comment)
temp_comment = gsub("^not_detached$", "Not detached", temp_comment)
temp_comment = gsub("^ok$", "Detached", temp_comment)
data$Comment = temp_comment

data$Species = as.factor(data$Species)
data$Protocol = as.factor(data$Protocol)
data$Comment = as.factor(data$Comment)

pt <- PivotTable$new()
pt$addData(data)
pt$addRowDataGroups("Species")
pt$addColumnDataGroups("Protocol")
pt$addColumnDataGroups("Comment")
pt$defineCalculation(calculationName="All", summariseExpression="n()", noDataCaption = "-")
pt$evaluatePivot()
pt$setStyling(rowNumbers = 1:(length(unique(data$Species)) + 1), 
              columnNumbers = 1:pt$columnCount,
              declarations=list("font-weight"="bold", "text-align"="center"))
pt$renderPivot()


### PCA

# library("stats")
# library("ggcorrplot")
# 
# sgs = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_22_03_23/data/species_proteins.csv", 
#                             sep = ",", header = T, check.names = F)
# 
# temp_data_species = temp_data_species %>%
#   group_by(Species) %>%
#   mutate("median_detachment_force" = median((!!sym(parameter_list[1]))))
# 
# 
# pca_data = temp_data_species %>%
#   select(Species, median_detachment_force) %>%
#   distinct()
# 
# pca_data_sgs <- base::merge(sgs, pca_data, by = "Species")
# 
# pca_data_sgs_select <- pca_data_sgs[,2:15]
# 
# corr_matrix <- cor(pca_data_sgs_select)
# 
# ggcorrplot(corr_matrix)
# 
# pca = princomp(pca_data_sgs_select)


