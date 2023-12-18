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
library("Polychrome")
#library("ggpmisc")
library("ggrepel")

library("extrafont")
#font_import()
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
    summarise(fun = 
                do.call(fun, list(x = (!!as.symbol(parameter)))))
  order_data = as.data.frame(order_data[order(order_data$fun), ])
  
  return(order_data)
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
    a_col = StrAlign(a_col, sep = "\\r")
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
    c_col = StrAlign(paste0("n = ", n_count), sep = "\\r")
  } else {
    c_col = ""
  }
  
  labels =  paste(a_col,
                  b_col,
                  c_col,
                  sep = " | ")
  return(labels)
}

####


#
parameter_with_threshold = c("log10_detachment_force", "log10_energy", "log10_negative_energy")

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "manon_acanthoptera")

# retrieve parameters
# Input
path_metadata_file = opt$concatenate_metadata
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

index_table = read.table(path_index, header = T, sep = "\t")

# read data figure
gg_data = read.table(paste0(plot_path, "/data_figure.csv"), 
                     stringsAsFactors = F,
                     sep = "\t",
                     header = T)

species_list = unique(gg_data$Species)
protocol_list = unique(gg_data$Protocol)


## by protocol for Drosophila melanogaster
plot_path_two_parameters_by_protocol_for_drosophila_melanogaster = paste0(plot_path, "/two_parameters/by_protocol_and_species/")
dir.create(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, showWarnings = FALSE, recursive = T)

# get threshold values with "detached pupae" protocol
thr_data = gg_data %>%
  filter(Protocol == "detached pupae") %>%
  summarise("log10_detachment_force" = -1.10,
            "log10_energy" = -2,
            "log10_negative_energy" = -2)

list_plot = list()
list_plot_t = list()

mypal_protocol <- c("red", "#222222", "#f3c300", "#875692", "#f38400", 
                    "#a1caf1", "#8db600", "#a13d2d", "#848482", "#008856", 
                    "#e68fac", "#0067a5")

for (i in 1:length(parameter_list)){
  for (j in 1:length(parameter_list)){
    if (i == j) next
    
    # filter data and computes stats
    temp_data = gg_data %>%
      filter(Species == "Drosophila_melanogaster" & 
               Stock == "cantonS" & Protocol != "water" & Protocol != "1 tape ; no glue ; speed x3") %>%
      filter(Comment == "ok" ) %>%
      group_by(Protocol)
    
    
    # | Comment == "cuticle_broke" | 
    # Comment == "not_detached"
    
    temp_data$Protocol = factor(temp_data$Protocol, 
                                levels = c("standard",
                                           "speed /3",
                                           "speed x3",
                                           "1 strong tape ; glue",
                                           "5 min",
                                           "0 s",
                                           "3 d",
                                           "0.25 N",
                                           "2 tapes ; no glue",
                                           "1 tape ; no glue ; speed x3",
                                           "1 tape ; no glue",
                                           "no tape ; glue"),
                                ordered = T)
    
    names(mypal_protocol) <- levels(temp_data$Protocol)
    
    # complete plot
    temp_data = temp_data %>% 
      mutate("median_x" = median((!!sym(parameter_list[i])), na.rm = T)) %>%
      mutate("sd_x" = sd((!!sym(parameter_list[i])), na.rm = T)) %>%
      mutate("median_y" = median((!!sym(parameter_list[j])), na.rm = T)) %>%
      mutate("sd_y" = sd((!!sym(parameter_list[j])), na.rm = T))
    
    p = ggplot(temp_data,
               aes_string("median_x", y = "median_y", colour = "Protocol")) +
      geom_point(temp_data,
                 mapping = aes_string(x = parameter_list[i], y = parameter_list[j])) +
      geom_point(size = 1) +
      geom_errorbar(xmin = temp_data[["median_x"]] - temp_data[["sd_x"]],
                    xmax = temp_data[["median_x"]] + temp_data[["sd_x"]]) +
      geom_errorbar(ymin = temp_data[["median_y"]] - temp_data[["sd_y"]],
                    ymax = temp_data[["median_y"]] + temp_data[["sd_y"]]) +
      xlab(paste0(lab_list[i], " (", unit_list[i], ")")) +
      ylab(paste0(lab_list[j], " (", unit_list[j], ")")) +
      scale_colour_manual(values = mypal_protocol) +
      theme_bw(base_size = 50) +
      theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5), 
            legend.position = c(0.25, 0.8),
            axis.text.x = element_text(family = "Courier New"), 
            axis.text.y= element_text(family = "Courier New"),
            aspect.ratio=1)

    

    ggsave(file = paste0(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, "/x_", parameter_list[i], "_y_", parameter_list[j], "_Drosophila_melanogaster", ".pdf"), 
           plot=p, width=16, height=8, device = "pdf")
    
    list_plot[[paste0("x_", parameter_list[i], "_y_", parameter_list[j])]] = p
    
    p2 = list_plot[["x_log10_negative_energy_y_log10_energy"]]
    p3 = list_plot[["x_log10_detachment_force_y_log10_energy"]]
    
    
    
    # trimmed plot
    if (parameter_list[i] %in% parameter_with_threshold | 
        parameter_list[j] %in% parameter_with_threshold) {
      if (parameter_list[i] %in% parameter_with_threshold) {
        temp_data = temp_data %>%
          filter(!!sym(parameter_list[i]) > as.numeric(thr_data[parameter_list[i]]))
      }
      if (parameter_list[j] %in% parameter_with_threshold) {
        temp_data = temp_data %>%
          filter(!!sym(parameter_list[j]) > as.numeric(thr_data[parameter_list[j]]))
      }
      
      temp_data = temp_data %>% 
        mutate("median_x" = median((!!sym(parameter_list[i])), na.rm = T)) %>%
        mutate("sd_x" = sd((!!sym(parameter_list[i])), na.rm = T)) %>%
        mutate("median_y" = median((!!sym(parameter_list[j])), na.rm = T)) %>%
        mutate("sd_y" = sd((!!sym(parameter_list[j])), na.rm = T))
      
      t = ggplot(temp_data,
                 aes_string("median_x", y = "median_y", colour = "Protocol")) +
        geom_point(temp_data,
                   mapping = aes_string(x = parameter_list[i], y = parameter_list[j])) +
        stat_cor(cor.coef.name = "r", aes(label = paste(..r.label..)), color = "black",
                 label.y.npc="top", label.x.npc = "left", inherit.aes = TRUE) +
        # geom_smooth(method=lm , color="red", formula = y ~ x, se=FALSE, fullrange = T) +
        # stat_poly_eq(data = temp_data,
        #              color = "red",
        #              inherit.aes = F,
        #              method = lm,
        #              mapping = aes_string("median_x", y = "median_y"),
        #              formula = y ~ x) +
        geom_smooth(data = temp_data,
                    method =lm,
                    mapping = aes_string(x = parameter_list[i],
                                         y = parameter_list[j]),
                    color="black", formula = y ~ x, se = F) +
        # stat_regline_equation(aes(label = ..rr.label..)) +
        #stat_poly_eq avec package ggpmisc ne fonctionne pas sur PC Manon
        # stat_poly_eq(data = temp_data,
        #              method =lm,
        #              mapping = aes_string(x = parameter_list[i],
        #                                   y = parameter_list[j]),
        #              color="blue", formula = y ~ x, label.x = "right") +
        geom_point(size = 1) +
        geom_errorbar(xmin = temp_data[["median_x"]] - temp_data[["sd_x"]],
                      xmax = temp_data[["median_x"]] + temp_data[["sd_x"]]) +
        geom_errorbar(ymin = temp_data[["median_y"]] - temp_data[["sd_y"]],
                      ymax = temp_data[["median_y"]] + temp_data[["sd_y"]]) +
        xlab(paste0(lab_list[i], " (", unit_list[i], ")")) +
        ylab(paste0(lab_list[j], " (", unit_list[j], ")")) +
        scale_colour_manual(values = mypal_protocol) +
        theme_bw(base_size = 22)
      
      ggsave(file = paste0(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, "/x_", parameter_list[i], "_y_", parameter_list[j], "_Drosophila_melanogaster_trimmed", ".pdf"), 
             plot=t, width=16, height=8, device = "pdf")
      
      list_plot_t[[paste0("x_", parameter_list[i], "_y_", parameter_list[j])]] = t
      t2 = list_plot_t[["x_log10_negative_energy_y_log10_energy"]]
      t3 = list_plot_t[["x_log10_energy_y_log10_detachment_force"]]
      
      
      # f= p2 + annotation_custom(ggplotGrob(t2), xmin = -6, xmax = -4,
      #                           ymin = -2, ymax = 0)
      
    }
  }
}


## all previous plot are stored in list_plot
# names(list_plot)
# list_plot[["x_detachment_force_y_energy"]]

#plot parameter and speed for melano (comment == "ok")
plot_path_two_parameters_by_protocol_for_drosophila_melanogaster = 
  paste0(plot_path, "/two_parameters/by_protocol_and_species/")
dir.create(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, 
           showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){
  
  p = gg_data %>%
    filter(Species == "Drosophila_melanogaster") %>%
    filter(Comment == "ok") %>%
    filter(! is.na(Speed)) %>%
    filter(Speed %in% ordered(c("1/3", "1", "3"))) %>%
    ggplot(aes_string(x = "Speed", 
                      y = parameter_list[i], 
                      color = "Protocol")) +
    geom_boxplot() +
    geom_point(mapping = aes_string(x = "Speed",
                                    y = parameter_list[i],
                                    fill = "Protocol"),
               position=position_jitterdodge(),
               alpha = 0.3,
               show.legend = F) +
    xlab("Speed") +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    theme_bw(base_size = 22) 
  
  ggsave(file = paste0(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, "/x_", parameter_list[i], "_y_speed_protocol", ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
}


# ## by species
plot_path_two_parameters_by_species = paste0(plot_path, "/two_parameters/by_species/")
dir.create(plot_path_two_parameters_by_species, showWarnings = FALSE, recursive = T)

c27 <- c("dodgerblue2", "#E31A1C", "red",
         "green4","#6A3D9A", "purple",
         "#FF7F00", "orange","black", 
         "gold1","skyblue2", "#FB9A99", 
         "#ffb6c1","palegreen2","#CAB2D6",
         "#CBC3E3","#FDBF6F", "#FFD580",
         "gray70", "khaki2","maroon", 
         "orchid1", "deeppink1", "blue1", 
         "steelblue4","darkturquoise", "green1")

for (i in 1:length(parameter_list)){
  for (j in 1:length(parameter_list)){
    if (i == j) next
    
    temp_data_species = gg_data
    if (parameter_list[i] %in% c("Glue_area", "log10_glue_area") | parameter_list[j] %in% c("Glue_area", "log10_glue_area")) {
      if (parameter_list[i] %in% c("Glue_area", "log10_glue_area")) {
        temp_data_species = temp_data_species %>%
          filter(Species != "Megaselia_abdita") %>%
          filter(Species != "Drosophila_elegans") %>%
          filter((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
                   (Species != "Drosophila_melanogaster")) %>%
          filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
          filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
          group_by(Species) %>%
          filter(length(!!as.symbol(parameter_list[i])) > 1)
      }
      if (parameter_list[j] %in% c("Glue_area", "log10_glue_area")){
        temp_data_species = temp_data_species %>%
          filter(Species != "Megaselia_abdita") %>%
          filter(Species != "Drosophila_elegans") %>%
          filter((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
                   (Species != "Drosophila_melanogaster")) %>%
          filter(! is.na(!!as.symbol(parameter_list[j]))) %>%
          filter(is.finite(!!as.symbol(parameter_list[[j]]))) %>%
          group_by(Species) %>%
          filter(length(!!as.symbol(parameter_list[j])) > 1)
      }
    }
    
    
    if ( ! parameter_list[i] %in% c("Glue_area", "log10_glue_area")) {
      temp_data_species = temp_data_species %>%
        filter(Comment == "ok") %>%
        filter((Protocol == "1 strong tape ; glue ; 0.25 N" | Protocol == "standard")) %>%
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
        filter(is.finite(!!as.symbol(parameter_list[[i]]))) %>%
        group_by(Species) %>%
        filter(length(!!as.symbol(parameter_list[i])) > 1)
    }
    
    if ( ! parameter_list[j] %in% c("Glue_area", "log10_glue_area")) {
      temp_data_species = temp_data_species %>%
        filter(Comment == "ok") %>%
        filter((Protocol == "1 strong tape ; glue ; 0.25 N" | Protocol == "standard")) %>%
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
        filter(! is.na(!!as.symbol(parameter_list[j]))) %>%
        filter(is.finite(!!as.symbol(parameter_list[[j]]))) %>%
        group_by(Species) %>%
        filter(length(!!as.symbol(parameter_list[j])) > 1)
    }
    
    temp_data_species$Species = factor(temp_data_species$Species, 
                                       levels = c("Drosophila_kurseongensis",
                                                  "Drosophila_biarmipes",
                                                  "Drosophila_melanogaster",
                                                  "Drosophila_suzukii",
                                                  "Drosophila_mauritiana",
                                                  "Drosophila_simulans",
                                                  "Drosophila_yakuba",
                                                  "Drosophila_takahashii",        
                                                  "Drosophila_ananassae",
                                                  "Drosophila_prostipennis",
                                                  "Drosophila_eugracilis",
                                                  "Drosophila_rhopaloa",
                                                  "Drosophila_elegans",
                                                  "Drosophila_funebris",
                                                  "Drosophila_immigrans",
                                                  "Drosophila_virilis",
                                                  "Drosophila_tropicalis",
                                                  "Scaptodrosophila_lebanonensis",
                                                  "Drosophila_nannoptera",        
                                                  "Drosophila_pachea",
                                                  "Drosophila_malerkotliana",
                                                  "Zaprionus_indianus",
                                                  "Zaprionus_lachaisei",          
                                                  "Megaselia_scalaris",
                                                  "Drosophila_hydei",
                                                  "Drosophila_littoralis",        
                                                  "Drosophila_pseudoobscura"),
                                       ordered = T)
    
    names(c27) <- levels(temp_data_species$Species)
    
    
    # add stats
    temp_data_species = temp_data_species %>%
      group_by(Species) %>%
      mutate("median_x" = median((!!sym(parameter_list[i])))) %>%
      mutate("sd_x" = sd((!!sym(parameter_list[i])))) %>%
      mutate("median_y" = median((!!sym(parameter_list[j])))) %>%
      mutate("sd_y" = sd((!!sym(parameter_list[j]))))
    
    # construct data for the geom_text_repel function
    gg_repel_data = temp_data_species %>%
      select(Species, median_x, median_y) %>%
      distinct() 
    
    short_name = gg_repel_data$Species
    short_name = gsub("_", " ", short_name, fixed = T)
    short_name = gsub("Drosophila", "D.", short_name, fixed = T)
    short_name = gsub("Megaselia", "M.", short_name, fixed = T)
    short_name = gsub("Scaptodrosophila", "S.", short_name, fixed = T)
    short_name = gsub("Zaprionus", "Z.", short_name, fixed = T)
    short_name = substr(short_name, 1, 8)
    
    gg_repel_data = cbind(gg_repel_data, 
                          data.frame("species_number" = 1:nrow(gg_repel_data),
                                     "species_short" = short_name))
    
    #plot
    
    p = ggplot(temp_data_species,
               aes(x = median_x,
                   y = median_y,
                   color = Species)) +
      geom_point(temp_data_species,
                 mapping = aes_string(x = parameter_list[i], y = parameter_list[j]), alpha = 0.3) +
      geom_smooth(temp_data_species, 
                  mapping = aes_string(x = parameter_list[i], y = parameter_list[j]), 
                  inherit.aes = F, method = 'lm', formula = y ~ x) +
      stat_regline_equation(data = temp_data_species,
                            mapping = aes_string(x = parameter_list[i], y = parameter_list[j], label = "..eq.label.."),
                            label.x.npc = "left",
                            label.y.npc = "top",
                            formula = y ~ x,
                            inherit.aes = F) +
      # stat_regline_equation(mapping = aes_string(x = parameter_list[i], y = parameter_list[j]),
      #                       temp_data_species, aes(label = ..rr.label..), inherit.aes = F) +
      geom_point(size = 5, shape = 3) + 
      geom_errorbar(xmin = temp_data_species$median_x - temp_data_species$sd_x,
                    xmax = temp_data_species$median_x + temp_data_species$sd_x) +
      geom_errorbar(ymin = temp_data_species$median_y - temp_data_species$sd_y,
                    ymax = temp_data_species$median_y + temp_data_species$sd_y) +
      # xlim(min(temp_data_species[[parameter_list[i]]], na.rm = T),
      #      max(temp_data_species[[parameter_list[i]]], na.rm = T)) +
      # ylim(min(temp_data_species[[parameter_list[j]]], na.rm = T),
      #      max(temp_data_species[[parameter_list[j]]], na.rm = T)) +

      xlim(c(4.5, 6.75)) +
      ylim(c(-4, 0.5)) +
      
      xlab(paste0(lab_list[i], " (", unit_list[i], ")")) +
      ylab(paste0(lab_list[j], " (", unit_list[j], ")")) +
      geom_text_repel(data = gg_repel_data,
                      aes(x = median_x,
                          y = median_y,
                          label = species_short),
                      # nudge_y = max(gg_repel_data$median_y),
                      segment.linetype = "solid",
                      segment.color = "grey",
                      force = 50,
                      # direction = "y",
                      color = "black") +
      scale_colour_manual(values = c27) +
      theme_bw(base_size = 22) +
      theme(legend.position = "none")
    
    ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_", parameter_list[i], "_y_", parameter_list[j], ".pdf"),
           plot=p, width=10, height=8, device = "pdf")
  }
}

### superposition ####

temp_data = gg_data %>%
  filter(Species == "Drosophila_melanogaster" & Stock == "cantonS") %>%
  filter(Protocol == "standard" | Protocol == "0.25 N") %>%
  filter(Comment == "ok" | Comment == "cuticle_broke" | 
           Comment == "not_detached") %>%
  group_by(Protocol)


# f(extension) = load entre index 1 et 2

temp_gg_data = data.frame()



for (id in temp_data$Sample_ID){
  # id = "2022050403"
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
  
  current_id = which(index_table$id == id)
  index1 = index_table[current_id, "index_1"]
  index2 = index_table[current_id, "index_2"]
  
  temp_gg_data = rbind(temp_gg_data, 
                       data.frame("time" = sample$time[index1 : index2],
                                  "load" = sample$load[index1 : index2],
                                  "extension" = sample$extension[index1 : index2] - sample$extension[index2], 
                                  "protocol" = gg_data$Protocol[which(gg_data$Sample_ID == id)], 
                                  "id" = rep(id, length(index1 : index2))))
}

legend_title <- "OMG My Title"

p_el = ggplot(data = temp_gg_data, aes(x = extension, y = load, color = protocol, group = id)) +
  #group permet de separer les jeux de données par id, evite les courbes liees entre elles
  geom_path() + xlab("Captor position (mm)") + ylab("Force (N)") +
  scale_color_manual(values = c("black", "grey")) +
  scale_fill_manual(legend_title) +
  theme_bw(base_size = 22) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        legend.position = c(0.25, 0.8),
        axis.text.x = element_text(family = "Courier New"), 
        axis.text.y= element_text(family = "Courier New"),
        aspect.ratio=1)
p_el

ggsave(file = paste0(plot_path_two_parameters_by_species, "extension_load_superposition_025N_standard", ".pdf"),
       plot=p_el, width=16, height=8, device = "pdf")


#plot species protocol standard and strong force 0,25N
plot_path_two_parameters_detachment_protocol = paste0(plot_path, "/two_parameters/by_protocol_and_species/")
dir.create(plot_path_two_parameters_detachment_protocol, showWarnings = FALSE, recursive = T)

species_to_keep =
  unique(gg_data$Species[which(gg_data$Protocol ==
                                 "1 strong tape ; glue ; 0.25 N")])[
                                   unique(gg_data$Species[
                                     which(gg_data$Protocol ==
                                             "1 strong tape ; glue ; 0.25 N")]) %in%
                                     unique(gg_data$Species[
                                       which(gg_data$Protocol == "standard")])]

temp_data = gg_data %>%
  filter(Comment == "ok" | Comment == "cuticle_broke" | Comment == "not_detached") %>%
  filter(Species %in% species_to_keep) %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Protocol == "1 strong tape ; glue ; 0.25 N" | Protocol == "standard") %>%
  group_by(Species, Protocol) %>%
  summarise(median = median(detachment_force),
            sd = sd(detachment_force))


index_standard =which(temp_data$Protocol == "standard")
index_strong_tape_and_0.25_N =
  which(temp_data$Protocol == "1 strong tape ; glue ; 0.25 N")

temp_gg_data = data.frame(Species = temp_data$Species[index_standard],
                          median_standard = temp_data$median[index_standard],
                          median_strong_tape_and_0.25_N =
                            temp_data$median[index_strong_tape_and_0.25_N],
                          sd_standard = temp_data$sd[index_standard],
                          sd_strong_tape_and_0.25_N =
                            temp_data$sd[index_strong_tape_and_0.25_N])

c5 <- c("dodgerblue2", "orange", "red",
         "green4","#6A3D9A")

# "purple",
#          "#FF7F00", "black", 
#          "gold1","skyblue2", "#FB9A99", 
#          "#ffb6c1","palegreen2","#CAB2D6",
#          "#CBC3E3","#FDBF6F", "#FFD580",
#          "gray70", "khaki2","maroon", 
#          "orchid1", "deeppink1", "blue1", 
#          "steelblue4","darkturquoise", "green1")

names(c5) <- levels(temp_gg_data$Species)

p = ggplot(temp_gg_data, aes(x = median_standard,
                             y = median_strong_tape_and_0.25_N,
                             color = Species)) +
  geom_errorbar(xmin = temp_gg_data$median_standard -
                  temp_gg_data$sd_standard,
                xmax = temp_gg_data$median_standard +
                  temp_gg_data$sd_standard) +
  geom_errorbar(ymin = temp_gg_data$median_strong_tape_and_0.25_N -
                  temp_gg_data$sd_strong_tape_and_0.25_N,
                ymax = temp_gg_data$median_strong_tape_and_0.25_N +
                  temp_gg_data$sd_strong_tape_and_0.25_N) +
  geom_point() +  scale_colour_manual(values = c5) +
  stat_cor(cor.coef.name = "r", aes(label = paste(..r.label..)), color = "black",
           label.y.npc="top", label.x.npc = "left", inherit.aes = TRUE) +
  xlim(c(0,
         max(temp_gg_data$median_standard +
               temp_gg_data$sd_standard))) +
  ylim(c(0,
         max(temp_gg_data$median_strong_tape_and_0.25_N +
               temp_gg_data$sd_strong_tape_and_0.25_N))) +
  geom_abline(slope=1) +  geom_smooth(method='lm', formula= y~x, color = "red", se = FALSE) +
  xlab("Detachment force for protocol standard (N)") +
  ylab("Detachment force for protocol '1 strong tape ; glue ; 0.25 N' (N)") +
  theme_bw(base_size = 22)

#à reutiliser si besoin
# xlim(c(min(temp_gg_data$median_standard -
#              temp_gg_data$sd_standard),
#        max(temp_gg_data$median_standard +
#              temp_gg_data$sd_standard))) +
#   ylim(c(min(temp_gg_data$median_strong_tape_and_0.25_N -
#                temp_gg_data$sd_strong_tape_and_0.25_N),
#          max(temp_gg_data$median_strong_tape_and_0.25_N +
#                temp_gg_data$sd_strong_tape_and_0.25_N))) +

ggsave(file = paste0(plot_path_two_parameters_detachment_protocol, "/detachment_force_species_protocol", ".pdf"),
       plot=p, width=16, height=8, device = "pdf")

