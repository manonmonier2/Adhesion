rm(list = ls())

library("config")
library("ggplot2")
library("dplyr")
library("rcompanion")
library("agricolae")
library("FSA")
library("ggpubr")
library("DescTools")
#library("ggtext")

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


format_label = function(factor_name, factor_labels, stat_group = NA, n_data = NA) {
  if (factor_name == "Species"){
    # a_col = paste0("***",
    #                levels(factor_labels),
    #                "***")
    a_col = levels(factor_labels)
    a_col = gsub("_", " ", a_col, fixed = T)
    a_col = gsub("Drosophila", "D.", a_col, fixed = T)
    a_col = gsub("Megaselia", "M.", a_col, fixed = T)
    a_col = gsub("Scaptodrosophila", "S.", a_col, fixed = T)
    a_col = gsub("Zaprionus", "Z.", a_col, fixed = T)
    a_col = StrAlign(a_col, sep = "\\l")
    a_col = substr(a_col, 1, 8)
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


manual_order = ordered(c( "no tape", "detached pupae", "detached pupae and speed x3",
                          "pupae attached on tesa tape",
                          "0.25 N", "3 days",
                          "0s", "5min", "strong tape",
                          "speed x3", "speed /3", "standard"))

list_plot = list()

for (i in 1:length(parameter_list)){
  temp_data = gg_data %>% 
    filter(Comment == "ok" & 
             Species == "Drosophila_melanogaster" & 
             Protocol != "water") %>%
    filter(!is.na(!!as.symbol(parameter_list[[i]]))) %>%
    filter(is.finite(!!as.symbol(parameter_list[[i]])))
  
  
  temp_data_all_comment = gg_data %>% 
    filter(Species == "Drosophila_melanogaster" & 
             Stock == "cantonS" & Protocol != "water") %>%
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

p1 = ggarrange(plotlist = list_plot[1:3], nrow = 3, common.legend = T, align = c("v"), labels = c("A", "B", "C"))
p2 = ggarrange(plotlist = list_plot[4:6], nrow = 3, common.legend = T, align = c("v"), labels = c("D", "E", "F"))
p = ggarrange(p1, p2, ncol = 2, common.legend = T, align = c("v"))

ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/all_parameters_Drosophila_melanogaster", ".pdf"), 
       plot=p, width=40, height=30, device = cairo_pdf)


## by species
plot_path_one_parameter_by_species = paste0(plot_path, "/one_parameter/by_species/")
dir.create(plot_path_one_parameter_by_species, showWarnings = FALSE, recursive = T)

list_plot = list()
list_plot_log = list()
for (i in 1:length(parameter_list)){
  if (parameter_list[i] %in% c("Glue_area", "log10_glue_area")) {
    temp_data_species = gg_data %>% 
      filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
      filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
      group_by(Species) %>%
      filter(length(!!as.symbol(parameter_list[i])) > 1)
    
    temp_data_all_comment = temp_data_species
    
  } else {
    temp_data_species = gg_data %>%
      filter(Comment == "ok") %>%
      filter((Protocol == "strong tape and 0.25 N" | Protocol == "standard")) %>%
      filter(Species != "Megaselia_abdita") %>%
      filter(Species != "Drosophila_quadraria") %>%
      filter(
        ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
           (Species == "Drosophila_suzukii" & Stock == "WT3") |
           (Species == "Drosophila_biarmipes" & Stock == "G224") |
           (Species == "Drosophila_simulans" & Stock == "simulans_vincennes")) |
          (! Species %in% c("Drosophila_melanogaster", "Drosophila_suzukii", 
                            "Drosophila_biarmipes", "Drosophila_simulans")) 
      ) %>%
      filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
      group_by(Species) %>%
      filter(length(!!as.symbol(parameter_list[i])) > 1)
    
    temp_data_all_comment = gg_data %>%
      filter(Comment == "ok" | Comment == "cuticle_broke" | Comment == "not_detached") %>%
      filter((Protocol == "strong tape and 0.25 N" | Protocol == "standard")) %>%
      filter(Species != "Megaselia_abdita") %>%
      filter(Species != "Drosophila_quadraria") %>%
      filter(
        ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
           (Species == "Drosophila_suzukii" & Stock == "WT3") |
           (Species == "Drosophila_biarmipes" & Stock == "G224") |
           (Species == "Drosophila_simulans" & Stock == "simulans_vincennes")) |
          (! Species %in% c("Drosophila_melanogaster", "Drosophila_suzukii", 
                            "Drosophila_biarmipes", "Drosophila_simulans")) 
      ) %>%
      filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
      group_by(Species) %>%
      filter(length(!!as.symbol(parameter_list[i])) > 1)
  }
  
  temp_data_species = as.data.frame(temp_data_species)
  
  gg_data_test_species = make_stat(data = temp_data_species,
                                   factor_name = "Species",
                                   parameter = parameter_list[i])
  
  
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
          axis.text.y= element_text(family = "Courier New"))
  
  # recuperation de stat ggplot avec ggplot_build()
  # df_res = ggplot_build(p)$data[[1]]
  
  ggsave(file = paste0(plot_path_one_parameter_by_species, "/", parameter_list[i], ".pdf"), 
         plot=p, width=16, height=8, device = cairo_pdf)
  
  if (! grepl("^log10_", parameter_list[i])){
    list_plot[[parameter_list[i]]] = p
  }
  
  #list_plot_log[[parameter_list[i]]] = p
}

p1 = ggarrange(plotlist = list_plot[1:3], nrow = 3, common.legend = T, align = c("v"), labels = c("A", "B", "C"))
p2 = ggarrange(plotlist = list_plot[4:6], nrow = 3, common.legend = T, align = c("v"), labels = c("D", "E", "F"))
p = ggarrange(p1, p2, ncol = 2, common.legend = T, align = c("v"))

ggsave(file = paste0(plot_path_one_parameter_by_species, "/all_parameters_all_species", ".pdf"), 
       plot=p, width=30, height=20, device = cairo_pdf)


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
    if (parameter_list[i] %in% c("Glue_area", "log10_glue_area")) {
      temp_data_stock = gg_data %>%
        filter(Species == focus) %>%
        filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
        filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
        group_by(Stock) %>%
        filter(length(!!as.symbol(parameter_list[i])) > 1)
      
      temp_data_all_comment = temp_data_stock
      
    } else {
      temp_data_stock = gg_data %>%
        filter(Comment == "ok") %>%
        filter(Protocol == "standard") %>%
        filter(Species == focus) %>%
        filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
        filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
        group_by(Stock) %>%
        filter(length(!!as.symbol(parameter_list[i])) > 1)
      
      temp_data_all_comment = gg_data %>%
        filter(Comment == "ok" | Comment == "cuticle_broke" | 
                 Comment == "not_detached") %>%
        filter(Protocol == "standard") %>%
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
  ggsave(file = paste0(plot_path_one_parameter_by_stock, "/", parameter_list[i], 
                       "_stock", ".pdf"), 
         plot=p, 
         width=15, 
         height=20, 
         device = cairo_pdf)
}

# p = ggarrange(plotlist = list_plot, nrow = 3, common.legend = T, align = c("v"), labels = c("A", "B", "C"))
# ggsave(file = paste0(plot_path_one_parameter_by_stock, "/all_parameters_stock", ".pdf"), 
#        plot=p, width=30, height=40, device = cairo_pdf)
