rm(list = ls())

library("config")
library("ggplot2")
library("dplyr")
library("rcompanion")
library("agricolae")
library("FSA")
library("ggpubr")
library("forcats")
library("rlang")
library("DescTools")

#library("mdthemes")

#### FUNCTIONS ####

n_fun <- function(data){
  y_pos = min(data) + (max(data) - min(data)) * 0.1
  return(data.frame(y = y_pos, label = paste0("n = ",length(data))))
}


make_stat = function(temp_data){
  temp_data$Protocol = as.factor(temp_data$Protocol)
  test_stat = c()
  
  # shapiro
  res_shapiro_global = shapiro.test(temp_data[, which(colnames(temp_data) == parameter_list[i])])
  shapiro_global_handler = res_shapiro_global$p.value >= 0.01
  test_stat = c(test_stat, shapiro_global_handler)
  
  
  # bartlett
  res_bartlett = bartlett.test(temp_data[, which(colnames(temp_data) == parameter_list[i])] ~ Protocol, temp_data)
  bartlett_reject = res_bartlett$p.value >= 0.01 #si p value superieure a 0.01 on accepte H0 donc variance egales entre protocoles
  test_stat = c(test_stat, bartlett_reject)
  
  # anova
  aov_res = aov(temp_data[, which(colnames(temp_data) == parameter_list[i])] ~ Protocol, temp_data)
  anova_res = anova(aov_res)
  anova_handler = anova_res$`Pr(>F)`[1] >= 0.01
  
  test_stat = c(test_stat, anova_handler)
  
  # kruskal wallis
  kruskal_res = kruskal.test(temp_data[, which(colnames(temp_data) == parameter_list[i])] ~ Protocol, temp_data)
  kruskal_handler = kruskal_res$p.value >= 0.01
  
  test_stat = c(test_stat, anova_handler)
  
  names(test_stat) = c("shapiro", "bartlett", "anova", "kruskal-wallis")
  
  # Tukey
  tukey_res = TukeyHSD(aov_res, conf.level = 0.99)
  HSD_res = HSD.test(aov_res, "Protocol", group = T)
  tukey_group = HSD_res$groups
  tukey_group = cbind(rownames(tukey_group), tukey_group[, -1])#on extrait les noms de ligne et on les place dans une nouvelle colonne à gauche avec cbind
  #puis on append le tableau de resultats tukey_group auquel on retire les moyennes en colonne 1
  colnames(tukey_group) = c("Protocol", "groups")
  tukey_group = as.data.frame(tukey_group)
  
  
  # Dunn
  dunn_res = dunnTest(temp_data[, which(colnames(temp_data) == parameter_list[i])] ~ Protocol, data = temp_data)
  
  dunn_group = cldList(P.adj ~ Comparison, threshold = 0.01, data = dunn_res$res, 
                       remove.zero = F,
                       remove.space = T)
  dunn_group = dunn_group[, -3]
  colnames(dunn_group) = c("Protocol", "groups")
  # correction of dunn group name (add space)
  for(true_name in unique(temp_data$Protocol)){
    dunn_name = gsub(" ", "", true_name)
    dunn_group$Protocol[which(dunn_group$Protocol == dunn_name)] = true_name
  }
  dunn_group = dunn_group[order(dunn_group$groups), ]#order donne la position des valeurs non ordonnees apres ordre alphabetique
  #order est donne pour lignes car on veut ordonner lignes
  ###
  
  used_test = NA
  if (test_stat["shapiro"] & test_stat["bartlett"]){
    gg_data_test = tukey_group
    used_test = "Tukey"
  } else {
    gg_data_test = dunn_group
    used_test = "Dunn"
  }
  return(gg_data_test)
}


####

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "portable")

# retrieve parameters
# Input
path_metadata_file = opt$concatenate_metadata
path_index = opt$index_path
path_batch_by_id = opt$batch_by_id
plot_path = opt$plot_path
path_integral = opt$integral_path


# read data figure
gg_data = read.table(paste0(plot_path, "/data_figure.csv"), 
                     stringsAsFactors = F,
                     sep = "\t",
                     header = T)

## convert to parameters in the .yml file
parameter_list = c("detachment_force", "energy", "negative_energy", "rigidity", "position_difference", "pression_extension", "pupa_area", "pupa_length", "Glue_area",
                   "log10_detachment_force", "log10_energy", "log10_negative_energy", "log10_rigidity", "log10_position_difference", "log10_pupa_area", "log10_pupa_length", "log10_glue_area")
lab_list = c("Detachment force", "Short term energy", "Proxy of long term energy","Rigidity", "Position difference", "Pression extension", "Pupa area", "Pupa length", "Pupa area",
             "log(Detachment force)", "log(Short term energy)", "log(Proxy of long term energy)", "log(Rigidity)", "log(Position difference)", "log(Pression extention)" , "log(Pupa area)", "log(Pupa length)", "log10_glue_area")
unit_list = c("Newton", "N.mm", "N.mm", "N.mm-1", "mm", "mm", "mm^2", "mm", "um^2", "Newton", "N.mm", "N.mm", "N.mm-1", "mm", "mm^2", "mm", "um^2")

species_list = unique(gg_data$Species)
protocol_list = unique(gg_data$Protocol)
stat_list = c("mean", "max", "min", "median", "sd")

# one parameter plot
## D. melanogaster protocols
plot_path_one_parameter_by_protocol_and_species = paste0(plot_path, "/one_parameter/by_protocol_and_species/")
dir.create(plot_path_one_parameter_by_protocol_and_species, showWarnings = FALSE, recursive = T)


list_plot = list()
for (i in 1:length(parameter_list)){
  temp_data = gg_data %>% 
    filter(Comment == "ok" & 
             Species == "Drosophila_melanogaster" & 
             Protocol != "water")
  
  
  temp_data_all_comment = gg_data %>% 
    filter(Species == "Drosophila_melanogaster" & Protocol != "water") %>%
    filter(Comment == "ok" | Comment == "cuticle_broke" | Comment == "not_detached")
  
  gg_data_test = make_stat(temp_data)
  
  temp_data$Protocol = factor(temp_data$Protocol, levels = gg_data_test$Protocol)
  
  x_labels =  paste(StrAlign(gg_data_test$Protocol, sep = "\\r"),
                 StrAlign(gg_data_test$groups, sep = "\\r"),
                 StrAlign(paste0("n = ", 
                                   unlist(lapply(gg_data_test$Protocol, 
                                                 function(x) {
                                        sum(temp_data_all_comment$Protocol == x)
                                        }))), sep = "\\r"),
                  sep = " | ")
  


  
  p = ggplot(temp_data,
             aes_string(x = "Protocol", y = parameter_list[i])) +
    geom_point(colour = "black", shape = 20, size = 2, stroke = 1) +
    geom_boxplot(width= 0.4, colour= "black", outlier.colour = "grey", fill = NA) +
    theme_bw(base_size = 22) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), axis.title.y = element_blank()) +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    scale_x_discrete(labels = x_labels) +
    geom_text(data = gg_data_test, aes_string(label = "groups", y = min(temp_data[, which(colnames(temp_data) == parameter_list[i])], na.rm = T))) +
    coord_flip()
  
  ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/", parameter_list[i], "_Drosophila_melanogaster", ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
  
  if (! grepl("^log10_", parameter_list[i])){
    list_plot[[parameter_list[i]]] = p + coord_flip()
  }
}

p = ggarrange(plotlist = list_plot[1:6], ncol = 2, nrow = 3, common.legend = T, labels = c("A", "B", "C", "D", "E", "F"))
ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/all_parameters_Drosophila_melanogaster", ".pdf"), 
       plot=p, width=20, height=30, device = "pdf")



## by species
plot_path_one_parameter_by_species = paste0(plot_path, "/one_parameter/by_species/")
dir.create(plot_path_one_parameter_by_species, showWarnings = FALSE, recursive = T)

list_plot = list()
for (i in 1:length(parameter_list)){
  # extract not ok value
  not_ok_gg_data = gg_data %>% 
    filter((
      (Protocol == "strong tape and 0,25 N" | Protocol == "standard") 
      & (Comment == "not_detached") | (Comment == "cuticle_broke")))
  
  temp_data_species = gg_data %>% 
    filter((Protocol == "strong tape and 0,25 N" | Protocol == "standard") 
           & Comment == "ok") %>%
    filter(! is.na(!!as.symbol(parameter_list[i]))) %>%
    group_by(Species) %>%
    filter(length(!!as.symbol(parameter_list[i])) > 1)
  
  temp_data_species = as.data.frame(temp_data_species)
  
  test_stat_species = c()
  
  # shapiro
  res_shapiro_global = shapiro.test(temp_data_species[, which(colnames(temp_data_species) == parameter_list[i])])
  shapiro_global_handler = res_shapiro_global$p.value >= 0.01
  test_stat_species = c(test_stat_species, shapiro_global_handler)
  
  
  # bartlett
  res_bartlett = bartlett.test(temp_data_species[, which(colnames(temp_data_species) == parameter_list[i])] ~ Species, temp_data_species)
  bartlett_reject = res_bartlett$p.value >= 0.01 #si p value superieure a 0.01 on accepte H0 donc variance egales entre protocoles
  test_stat_species = c(test_stat_species, bartlett_reject)
  
  # anova
  aov_res = aov(temp_data_species[, which(colnames(temp_data_species) == parameter_list[i])] ~ Species, temp_data_species)
  anova_res = anova(aov_res)
  anova_handler = anova_res$`Pr(>F)`[1] >= 0.01
  
  test_stat_species = c(test_stat_species, anova_handler)
  
  # kruskal wallis
  kruskal_res = kruskal.test(temp_data_species[, which(colnames(temp_data_species) == parameter_list[i])] ~ Species, temp_data_species)
  kruskal_handler = kruskal_res$p.value >= 0.01
  
  test_stat_species = c(test_stat_species, anova_handler)
  
  names(test_stat_species) = c("shapiro", "bartlett", "anova", "kruskal-wallis")
  
  # Tukey
  tukey_res = TukeyHSD(aov_res, conf.level = 0.99)
  HSD_res = HSD.test(aov_res, "Species", group = T)
  tukey_group = HSD_res$groups
  tukey_group = cbind(rownames(tukey_group), tukey_group[, -1])#on extrait les noms de ligne et on les place dans une nouvelle colonne à gauche avec cbind
  #puis on append le tableau de resultats tukey_group auquel on retire les moyennes en colonne 1
  colnames(tukey_group) = c("Species", "groups")
  tukey_group = as.data.frame(tukey_group)
  
  
  # Dunn
  dunn_res = dunnTest(temp_data_species[, which(colnames(temp_data_species) == parameter_list[i])] ~ Species, data = temp_data_species)
  
  dunn_group = cldList(P.adj ~ Comparison, threshold = 0.01, data = dunn_res$res)
  dunn_group = dunn_group[, -3]
  colnames(dunn_group) = c("Species", "groups")
  dunn_group$Species[which(dunn_group$Species == "s")] = "0s" #dans les resultats de dunn '0s' est affiche 's' donc on modifie
  dunn_group = dunn_group[order(dunn_group$groups), ]#order donne la position des valeurs non ordonnees apres ordre alphabetique
  #order est donne pour lignes car on veut ordonner lignes
  ###
  
  used_test = NA
  if (test_stat_species["shapiro"] & test_stat_species["bartlett"]){
    gg_data_test_species = tukey_group
    used_test = "Tukey"
  } else {
    gg_data_test_species = dunn_group
    used_test = "Dunn"
  }
  
  # reorder by detachment force median
  order_data = temp_data_species %>%
    group_by(Species) %>% 
    summarise(median = median(detachment_force))
  
  order_data = order_data[order(order_data$median), ]
  
  temp_data_species$Species = factor(temp_data_species$Species,
                                     levels = order_data$Species)
  
  gg_data_test_species$Species = factor(gg_data_test_species$Species,
                                     levels = order_data$Species)
  
  x_labels = paste0("***",
                    levels(gg_data_test_species$Species),
                    "***",
                    " [", 
                    gg_data_test_species$groups[
                      unlist(lapply(levels(gg_data_test_species$Species), 
                                    function(x) which(gg_data_test_species$Species == x)))],
                    "]")
  x_labels = gsub("_", " ", x_labels, fixed = T)
  x_labels = gsub("Drosophila", "D.", x_labels, fixed = T)
  x_labels = gsub("Megaselia", "M.", x_labels, fixed = T)
  x_labels = gsub("Scaptodrosophila", "S.", x_labels, fixed = T)
  x_labels = gsub("Zaprionus", "Z.", x_labels, fixed = T)
  
  
  #plot
  p = ggplot(temp_data_species,
             aes_string(x = "Species", y = parameter_list[i])) +
    geom_boxplot(width= 0.4, colour= "red", outlier.colour = "grey") + 
    geom_jitter(position=position_dodge(0.5)) +
    geom_jitter(data = not_ok_gg_data, 
                aes_string(x = "Species", 
                           y = parameter_list[i], 
                           shape = "Comment"), 
                position=position_dodge(0.8)) + 
    scale_shape_manual(values = c(3, 4)) +
    scale_color_manual(values = rep(1, 8)) +
    theme_bw(base_size = 18) +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    xlab("Species") + coord_flip() + scale_x_discrete(labels = x_labels) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave(file = paste0(plot_path_one_parameter_by_species, "/", parameter_list[i], ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
  
  if (! grepl("^log10_", parameter_list[i])){
    list_plot[[parameter_list[i]]] = p + coord_flip()
  }
  
}

p = ggarrange(plotlist = list_plot, common.legend = T)
ggsave(file = paste0(plot_path_one_parameter_by_species, "/all_parameters_all_species", ".pdf"), 
       plot=p, width=40, height=20, device = "pdf")
