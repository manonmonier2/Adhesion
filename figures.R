rm(list = ls())

library("config")
library("ggplot2")
library("dplyr")
library("rcompanion")
library("agricolae")
library("FSA")
library("ggpubr")

#### FUNCTIONS ####

n_fun <- function(data){
  y_pos = max(data) + (max(data) - min(data)) * 0.1
  return(data.frame(y = y_pos, label = paste0("n = ",length(data))))
}


log10_na = function(vect){
  log_vect = c()
  for (i in vect){
    if (is.na(i)) log_vect = c(log_vect, NA)
    else log_vect = c(log_vect, log10(abs(i)))
  }
  return(log_vect)
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

index_table = read.table(path_index, header = T, sep = "\t")
metadata = read.table(path_metadata_file, sep = "\t", header = T)
energy_table = read.table(paste0(path_integral, "/integral.csv"), header = T, sep = "\t")

list_id = index_table$id
gg_data = data.frame()
for (id in list_id){
  gg_data = rbind(gg_data, metadata[which(metadata$Sample_ID == id), ])
}


# parameters calculation
detachment_force = c()
rigidity = c()
energy = c()
position_difference = c()
detachment_position = c()
pression_extension = c()
for (id in gg_data$Sample_ID){
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
  current_metadata = metadata[metadata$Sample_ID == id, ]
  current_index = index_table[index_table$id == id, ]
  
  current_detachment_force = -min(sample$load[current_index$index_4:current_index$index_5])
  
  if(length(which(energy_table$id == id)) == 1) {
    current_energy = energy_table$difference_integrales[energy_table$id == id]
  } else {
    current_energy = NA
  }
  
  current_rigidity = (sample$load[current_index$index_2] - sample$load[current_index$index_1]) / (sample$extension[current_index$index_2] - sample$extension[current_index$index_1])
  current_position_difference = sample$extension[current_index$index_1] - sample$extension[current_index$index_5]
  
  current_pression_extension = sample$extension[current_index$index_2] - sample$extension[current_index$index_1]
  
  detachment_force = c(detachment_force, current_detachment_force)
  energy = c(energy, current_energy)
  rigidity = c(rigidity, current_rigidity)
  position_difference = c(position_difference, current_position_difference)
  
  pression_extension =c(pression_extension, current_pression_extension)
}

gg_data = cbind(gg_data, 
                detachment_force,
                energy,
                rigidity,
                position_difference,
                pression_extension,
                log10_na(detachment_force),
                log10_na(energy),
                log10_na(rigidity),
                log10_na(position_difference),
                log10_na(pression_extension)
)

colnames(gg_data)[(ncol(gg_data) - 4) : ncol(gg_data)] = c("log10_detachment_force", "log10_energy", "log10_rigidity", "log10_position_difference", "log10_pression_extension")

# exclusion of default condition for melanogaster
gg_data = gg_data %>% filter((Protocol != "default" & Species == "Drosophila_melanogaster") | Species != "Drosophila_melanogaster") #on retire les default de melano

# add column "speed" in gg_data
gg_data = gg_data %>%
  mutate(Speed = NA, 
         .after = Protocol) %>%
  mutate(Speed = replace(Speed, Protocol == "speed /3", "1/3")) %>%
  mutate(Speed = replace(Speed, Protocol == "standard", "1")) %>%
  mutate(Speed = replace(Speed, 
                         Protocol == "detached pupae", "1")) %>%
  mutate(Speed = replace(Speed, Protocol == "speed x3", "3")) %>%
  mutate(Speed = replace(Speed, 
                         Protocol == "detached pupae and speed x3", "3"))

parameter_list = c("detachment_force", "energy", "rigidity", "position_difference", "pression_extension",
                   "log10_detachment_force", "log10_energy", "log10_rigidity", "log10_position_difference")
lab_list = c("Detachment force", "Energy", "Rigidity", "Position difference", "Pression extension",
             "log(Detachment force)", "log(Energy)", "log(Rigidity)", "log(Position difference)", "log(Pression extension")
unit_list = c("Newton", "N.mm", "N.mm-1", "mm", "mm", "Newton", "N.mm", "N.mm-1", "mm", "mm")
species_list = unique(gg_data$Species)
protocol_list = unique(gg_data$Protocol)
stat_list = c("mean", "max", "min", "median", "sd")

# stat construction
## by_species
gg_stat_by_species = data.frame()
for(species in sort(species_list)){
  temp_species_data = gg_data %>% filter(Comment == "ok" & Species == species)
  stat_handler = c(species)
  colnames_handler = c("Species")
  for (i in 1:length(parameter_list)){
    for (stat_function in stat_list){
      stat_handler = c(stat_handler, 
                       do.call(stat_function, list(temp_species_data[[parameter_list[i]]], na.rm = T)))
      colnames_handler = c(colnames_handler,
                           paste0(stat_function, "_", parameter_list[i]))
    }
  }
  gg_stat_by_species = rbind(gg_stat_by_species, stat_handler)
}
colnames(gg_stat_by_species) = colnames_handler

# force to numeric type
for (col_name in colnames(gg_stat_by_species)){
  if (col_name == "Species") next
  gg_stat_by_species[[col_name]] = as.numeric(gg_stat_by_species[[col_name]])
}


## for melanogaster by protocol

gg_stat_melano = data.frame()
for(protocol in sort(protocol_list)){
  temp_protocol_data = gg_data %>% filter(Comment == "ok" & Species == "Drosophila_melanogaster" & Protocol == protocol)
  stat_handler = c(protocol)
  colnames_handler = c("Protocol")
  for (i in 1:length(parameter_list)){
    for (stat_function in stat_list){
      stat_handler = c(stat_handler, 
                       do.call(stat_function, list(temp_protocol_data[[parameter_list[i]]], na.rm = T)))
      colnames_handler = c(colnames_handler,
                           paste0(stat_function, "_", parameter_list[i]))
    }
  }
  gg_stat_melano = rbind(gg_stat_melano, stat_handler)
}
colnames(gg_stat_melano) = colnames_handler

# force to numeric type
for (col_name in colnames(gg_stat_melano)){
  if (col_name == "Protocol") next
  gg_stat_melano[[col_name]] = as.numeric(gg_stat_melano[[col_name]])
}

# one parameter plot

## by species
plot_path_one_parameter_by_species = paste0(plot_path, "/one_parameter/by_species/")
dir.create(plot_path_one_parameter_by_species, showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){
  
  #plot
  p = gg_data %>% 
    filter(((Species == "Drosophila_melanogaster" & Protocol == "default") | 
              Species != "Drosophila_melanogaster") & Comment == "ok") %>%
    ggplot(aes_string(x = "Species", y = parameter_list[i], fill = "Protocol")) +
    geom_point(colour = "black", shape = 20, size = 2, stroke = 1)+
    geom_boxplot(width= 0.4, colour= "red", outlier.colour = "grey") + 
    theme_bw(base_size = 18) +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    xlab("Species") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave(file = paste0(plot_path_one_parameter_by_species, "/", parameter_list[i], ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
  
}

## by species only standard protocol
plot_path_one_parameter_by_species = paste0(plot_path, "/one_parameter/by_species/")
dir.create(plot_path_one_parameter_by_species, showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){
  temp_data_species = gg_data %>% filter(Protocol == "standard" & Comment == "ok")
  
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
  colnames(tukey_group) = c("Protocol", "groups")
  tukey_group = as.data.frame(tukey_group)
  
  
  # Dunn
  dunn_res = dunnTest(temp_data_species[, which(colnames(temp_data_species) == parameter_list[i])] ~ Species, data = temp_data_species)
  
  dunn_group = cldList(P.adj ~ Comparison, threshold = 0.01, data = dunn_res$res)
  dunn_group = dunn_group[, -3]
  colnames(dunn_group) = c("Protocol", "groups")
  dunn_group$Protocol[which(dunn_group$Protocol == "s")] = "0s" #dans les resultats de dunn '0s' est affiche 's' donc on modifie
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
  
  
  #plot
  temp_data_species$Protocol = factor(temp_data_species$Species, levels = gg_data_test_species$Species)
  p = ggplot(temp_data_species,
             aes_string(x = "Species", y = parameter_list[i])) +
    geom_point(colour = "black", shape = 20, size = 2, stroke = 1)+
    geom_boxplot(width= 0.4, colour= "red", outlier.colour = "grey") +
    theme_bw(base_size = 22) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    xlab("Species") +
    stat_summary(fun.data = n_fun, geom = "text") +
    geom_text(data = gg_data_test_species, aes_string(x = "Protocol", label = "groups", y = min(temp_data_species[, which(colnames(temp_data_species) == parameter_list[i])], na.rm = T))) +
    ggtitle(paste0(lab_list[i], " by species"))
  
  ggsave(file = paste0(plot_path_one_parameter_by_species, "/", parameter_list[i], "_standard_protocol", ".pdf"),
         plot=p, width=16, height=8, device = "pdf")
  
}


## by protocol
plot_path_one_parameter_by_protocol = paste0(plot_path, "/one_parameter/by_protocol/")
dir.create(plot_path_one_parameter_by_protocol, showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){
  p = ggplot(gg_data %>% filter(Comment == "ok"),
             aes_string(x = "Protocol", y = parameter_list[i], fill = "Protocol")) +
    geom_point(colour = "black", shape = 20, size = 2, stroke = 1)+
    geom_boxplot(width= 0.4, colour= "red", outlier.colour = "grey", fill = NA) +
    coord_flip() +
    theme_bw(base_size = 22) +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    xlab("Protocol")
  
  ggsave(file = paste0(plot_path_one_parameter_by_protocol, "/", parameter_list[i], ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
}


## by protocol and species
plot_path_one_parameter_by_protocol_and_species = paste0(plot_path, "/one_parameter/by_protocol_and_species/")
dir.create(plot_path_one_parameter_by_protocol_and_species, showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){
  temp_data = gg_data %>% filter(Comment == "ok") # on fait un temp_data car on calcule le min et le max
  p = ggplot(temp_data,
             aes_string(x = "Protocol", y = parameter_list[i], fill = "Protocol")) +
    geom_point(colour = "black", shape = 20, size = 2, stroke = 1)+
    geom_boxplot(width= 0.4, colour= "red", outlier.colour = "grey", fill = NA) +
    theme_bw(base_size = 15) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    xlab("Protocol") +
    ylim(min(temp_data[[parameter_list[i]]]), max(temp_data[[parameter_list[i]]])) +
    ggtitle(paste0(lab_list[i], " by protocol and species")) +
    facet_wrap(Species ~ ., scales = "free") # permet de diviser les fenetres par espece
  
  ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/", parameter_list[i], ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
}

### by protocol for Drosophila_melanogaster
list_plot = list()
for (i in 1:length(parameter_list)){
  temp_data = gg_data %>% filter(Comment == "ok" & Species == "Drosophila_melanogaster")
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
  
  
  temp_data$Protocol = factor(temp_data$Protocol, levels = gg_data_test$Protocol)
  p = ggplot(temp_data,
             aes_string(x = "Protocol", y = parameter_list[i])) +
    geom_point(colour = "black", shape = 20, size = 2, stroke = 1) +
    geom_boxplot(width= 0.4, colour= "red", outlier.colour = "grey", fill = NA) +
    theme_bw(base_size = 22) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) +
    ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
    xlab("Protocol") +
    stat_summary(fun.data = n_fun, geom = "text") +
    geom_text(data = gg_data_test, aes_string(x = "Protocol", label = "groups", y = min(temp_data[, which(colnames(temp_data) == parameter_list[i])], na.rm = T))) +
    ggtitle(paste0(lab_list[i], " by protocol for Drosophila melanogaster"), subtitle = paste0(used_test, " test, P-value <= 0.01"))
  
  ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/", parameter_list[i], "_Drosophila_melanogaster", ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
  
  if (! grepl("^log10_", parameter_list[i])){
    list_plot[[parameter_list[i]]] = p + coord_flip()
  }
}

p = ggarrange(plotlist = list_plot, common.legend = T)
ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/all_parameters_Drosophila_melanogaster", ".pdf"), 
       plot=p, width=40, height=20, device = "pdf")

# two parameters plot
## by species
plot_path_two_parameters_by_species = paste0(plot_path, "/two_parameters/by_species/")
dir.create(plot_path_two_parameters_by_species, showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){
  for (j in 1:length(parameter_list)){
    if (i == j) next
    
    #incertitude
    # incertitude_detachment_force = moy(index_table$med_noise_1)
    # incertitude_rigidity = 
    # incertitude_energy = moy(energy_table$aire_index5_moins1)
    # incertitude_detachment_position = sqrt(2)*
    # 
    #plot
    p = ggplot(gg_stat_by_species, 
               aes_string(x = paste0("median_", parameter_list[i]), y = paste0("median_", parameter_list[j]), color = "Species")) +
      geom_point(size = 5, shape = 3) +
      geom_errorbar(xmin = gg_stat_by_species[[paste0("median_", parameter_list[i])]] - gg_stat_by_species[[paste0("sd_", parameter_list[i])]],
                    xmax = gg_stat_by_species[[paste0("median_", parameter_list[i])]] + gg_stat_by_species[[paste0("sd_", parameter_list[i])]]) +
      geom_errorbar(ymin = gg_stat_by_species[[paste0("median_", parameter_list[j])]] - gg_stat_by_species[[paste0("sd_", parameter_list[j])]],
                    ymax = gg_stat_by_species[[paste0("median_", parameter_list[j])]] + gg_stat_by_species[[paste0("sd_", parameter_list[j])]]) +
      xlim(min(gg_stat_by_species[[paste0("min_", parameter_list[i])]], na.rm = T), max(gg_stat_by_species[[paste0("max_", parameter_list[i])]], na.rm = T)) +
      ylim(min(gg_stat_by_species[[paste0("min_", parameter_list[j])]], na.rm = T), max(gg_stat_by_species[[paste0("max_", parameter_list[j])]], na.rm = T)) +
      geom_point(gg_data %>% filter(Comment == "ok"), 
                 mapping = aes_string(x = parameter_list[i], y = parameter_list[j]), alpha = 0.3) +
      #geom_vline(xintercept=) +
      xlab(paste0(lab_list[i], " (", unit_list[i], ")")) +
      ylab(paste0(lab_list[j], " (", unit_list[j], ")")) +
      theme_bw(base_size = 22) 
    
    ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_", parameter_list[i], "_y_", parameter_list[j], ".pdf"), 
           plot=p, width=16, height=8, device = "pdf")
  }
}

## by protocol for Drosophila melanogaster
plot_path_two_parameters_by_protocol_for_drosophila_melanogaster = paste0(plot_path, "/two_parameters/by_protocol_and_species/")
dir.create(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){
  for (j in 1:length(parameter_list)){
    if (i == j) next
    p = ggplot(gg_stat_melano,
               aes_string(paste0("median_", parameter_list[i]), y = paste0("median_", parameter_list[j]), color = "Protocol")) +
      geom_point(size = 1) +
      geom_errorbar(xmin = gg_stat_melano[[paste0("median_", parameter_list[i])]] - gg_stat_melano[[paste0("sd_", parameter_list[i])]],
                    xmax = gg_stat_melano[[paste0("median_", parameter_list[i])]] + gg_stat_melano[[paste0("sd_", parameter_list[i])]]) +
      geom_errorbar(ymin = gg_stat_melano[[paste0("median_", parameter_list[j])]] - gg_stat_melano[[paste0("sd_", parameter_list[j])]],
                    ymax = gg_stat_melano[[paste0("median_", parameter_list[j])]] + gg_stat_melano[[paste0("sd_", parameter_list[j])]]) +
      geom_point(gg_data %>% filter(Comment == "ok" & Species == "Drosophila_melanogaster"), 
                 mapping = aes_string(x = parameter_list[i], y = parameter_list[j]), alpha = 0.3) +
      xlab(paste0(lab_list[i], " (", unit_list[i], ")")) +
      ylab(paste0(lab_list[j], " (", unit_list[j], ")")) +
      theme_bw(base_size = 22) +
      ggtitle(paste0("x: ", lab_list[i], " y: ", lab_list[j] ," by protocol for Drosophila melanogaster"))
    
    ggsave(file = paste0(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, "/x_", parameter_list[i], "_y_", parameter_list[j], "_Drosophila_melanogaster", ".pdf"), 
           plot=p, width=16, height=8, device = "pdf")
  }
}

#plot parameter and speed
plot_path_two_parameters_by_protocol_for_drosophila_melanogaster = paste0(plot_path, "/two_parameters/by_protocol_and_species/")
dir.create(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){
   p = gg_data %>%
     filter(Speed %in% c("1/3", "1", "3")) %>%
     ggplot(aes_string(x = "Protocol", 
                        y = parameter_list[i], 
                        color = "Speed")) +
     geom_boxplot() +
     geom_jitter(mapping = aes_string(x = "Protocol", 
                                      y = parameter_list[i], 
                                      fill = "Protocol"), 
                 alpha = 0.3,
                 show.legend = F) +  
     xlab("Protocol") +
     ylab(paste0(lab_list[i], " (", unit_list[i], ")")) +
     theme_bw(base_size = 22) 
  
  ggsave(file = paste0(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, "/x_", parameter_list[i], "_y_speed_protocol", ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
}

#plot species protocol standard and strong force 0,25N
plot_path_two_parameters_by_protocol_for_drosophila_melanogaster = paste0(plot_path, "/two_parameters/by_protocol_and_species/")
dir.create(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){

  for (j in 1:length(parameter_list)){
    if (i == j) next
    temp_data_species_speed = gg_data %>% filter(Comment == "ok" & (Species == "Drosophila_melanogaster" & Protocol == "standard" & Comment == "ok"))
    
        p = ggplot(gg_stat_by_species,
                    aes_string(paste0("median_", parameter_list[i]), y = paste0("median_", parameter_list[j]), color = "Protocol")) +
            geom_point(size = 1) +
            geom_errorbar(xmin = gg_stat_melano[[paste0("median_", parameter_list[i])]] - gg_stat_melano[[paste0("sd_", parameter_list[i])]],
                    xmax = gg_stat_melano[[paste0("median_", parameter_list[i])]] + gg_stat_melano[[paste0("sd_", parameter_list[i])]]) +
            geom_errorbar(ymin = gg_stat_melano[[paste0("median_", parameter_list[j])]] - gg_stat_melano[[paste0("sd_", parameter_list[j])]],
                    ymax = gg_stat_melano[[paste0("median_", parameter_list[j])]] + gg_stat_melano[[paste0("sd_", parameter_list[j])]]) +
            geom_point(temp_data_species_speed, mapping = aes_string(x = which(temp_data_species_speed$Protocol == "standard"), y = which(temp_data_species_speed$Protocol == "strong tape and 0,25 N")), alpha = 0.3) +
            xlab(paste0(lab_list[i], " (", unit_list[i], ")")) +
            ylab(paste0(lab_list[j], " (", unit_list[j], ")")) +
            theme_bw(base_size = 22) +
            ggtitle(paste0("x: ", lab_list[i], " y: ", lab_list[j] ," standard and strong tape 0,25N"))

            ggsave(file = paste0(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, "/x_", parameter_list[i], "_y_", parameter_list[j], "_parameter_and_speed", ".pdf"),
              plot=p, width=16, height=8, device = "pdf")
  }
}

#superposition D.melano no_cond et D.melano strongforce
plot_path_superposition = paste0(plot_path, "/superposition/")
dir.create(plot_path_superposition, showWarnings = FALSE, recursive = T)

temp_data = gg_data %>% filter(Comment == "ok" & Species == "Drosophila_melanogaster" & (Protocol == "standard" | Protocol == "0,25 N"))
id_temp_data = temp_data$Sample_ID

time_load_extension = data.frame()

for (id in id_temp_data){
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
  index_sample = which(index_table$id == id)
  protocol_name = temp_data$Protocol[which(temp_data$Sample_ID == id)]
  
  # if (parameter_list[i] == "detachment_force"){
  #   temp_data = temp_data %>% filter(Protocol != "cond2")
  # }
  index_to_plot = as.numeric(index_table[index_sample,"index_1"]):as.numeric(index_table[index_sample,"index_3"])
  valeur_index_2 = as.numeric(index_table[index_sample,"index_2"])
  Tnew = sample$time - sample$time[valeur_index_2]
  Tnew = Tnew[index_to_plot]
  Enew = sample$extension - sample$extension[valeur_index_2]
  Enew = Enew[index_to_plot]
  
  
  temp_time_load_extension = data.frame("time" = Tnew,
                                        "load" = sample$load[index_to_plot],
                                        "extension" = Enew, 
                                        "protocol" = rep(protocol_name, length(index_to_plot)),
                                        "id" = rep(id, length(index_to_plot)))
  time_load_extension = rbind(time_load_extension, temp_time_load_extension)
}

p_tl_global = ggplot(data = time_load_extension, aes(x = time, y = load, color = protocol, fill = protocol)) +
  geom_path() + labs(fill = protocol)
theme_minimal() + 
  #xlim(range_time) + 
  #ylim(range_load) +
  theme(legend.position = "none")
#facet_wrap(species ~ ., scales = "free")


ggsave(file = paste0(plot_path_superposition, "/superposition_nocond_strongforce_Drosophila_melanogaster", ".pdf"), 
       plot=p_tl_global, width=16, height=8, device = "pdf")

#superposition D.melano div3 et D.melano cond3
plot_path_superposition = paste0(plot_path, "/superposition/")
dir.create(plot_path_superposition, showWarnings = FALSE, recursive = T)

temp_data = gg_data %>% filter(Comment == "ok" & Species == "Drosophila_melanogaster" & (Protocol == "detached pupae and speed x3" | Protocol == "speed /3"))
id_temp_data = temp_data$Sample_ID

time_load_extension = data.frame()

for (id in id_temp_data){
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
  index_sample = which(index_table$id == id)
  protocol_name = temp_data$Protocol[which(temp_data$Sample_ID == id)]
  
  # if (parameter_list[i] == "detachment_force"){
  #   temp_data = temp_data %>% filter(Protocol != "cond2")
  # }
  index_to_plot = as.numeric(index_table[index_sample,"index_1"]):as.numeric(index_table[index_sample,"index_5"])
  valeur_index_2 = as.numeric(index_table[index_sample,"index_2"])
  Tnew = sample$time - sample$time[valeur_index_2]
  Tnew = Tnew[index_to_plot]
  Enew = sample$extension - sample$extension[valeur_index_2]
  Enew = Enew[index_to_plot]
  
  
  temp_time_load_extension = data.frame("time" = Tnew,
                                        "load" = sample$load[index_to_plot],
                                        "extension" = Enew, 
                                        "protocol" = rep(protocol_name, length(index_to_plot)),
                                        "id" = rep(id, length(index_to_plot)))
  time_load_extension = rbind(time_load_extension, temp_time_load_extension)
}

p_tl_global = ggplot(data = time_load_extension, aes(x = time, y = load, color = protocol, fill = id)) +
  geom_path() +
  theme_minimal() + 
  #xlim(range_time) + 
  #ylim(range_load) +
  theme(legend.position = "none")
#facet_wrap(species ~ ., scales = "free")

ggsave(file = paste0(plot_path_superposition, "/superposition_cond3_div3_Drosophila_melanogaster", ".pdf"), 
       plot=p_tl_global, width=16, height=8, device = "pdf")

p_el_global = ggplot(data = time_load_extension, aes(x = extension, y = load, color = protocol, fill = id)) +
  geom_path() +
  theme_minimal() + 
  #xlim(range_time) + 
  #ylim(range_load) +
  theme(legend.position = "none")


ggsave(file = paste0(plot_path_superposition, "/superposition_ext_cond3_div3_Drosophila_melanogaster", ".pdf"), 
       plot=p_tl_global, width=16, height=8, device = "pdf")

#superposition D.melano cond1 et D.melano cond3
plot_path_superposition = paste0(plot_path, "/superposition/")
dir.create(plot_path_superposition, showWarnings = FALSE, recursive = T)

temp_data_2 = gg_data %>% filter(Comment == "ok" & Species == "Drosophila_melanogaster" & (Protocol == "detached pupae" | Protocol == "detached pupae and speed x3"))
id_temp_data_2 = temp_data_2$Sample_ID

time_load_extension_2 = data.frame()

for (id in id_temp_data_2){
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
  index_sample = which(index_table$id == id)
  protocol_name = temp_data_2$Protocol[which(temp_data_2$Sample_ID == id)]
  
  # if (parameter_list[i] == "detachment_force"){
  #   temp_data = temp_data %>% filter(Protocol != "cond2")
  # }
  index_to_plot = as.numeric(index_table[index_sample,"index_1"]):as.numeric(index_table[index_sample,"index_5"])
  valeur_index_2 = as.numeric(index_table[index_sample,"index_2"])
  Tnew = sample$time - sample$time[valeur_index_2]
  Tnew = Tnew[index_to_plot]
  Enew = sample$extension - sample$extension[valeur_index_2]
  Enew = Enew[index_to_plot]
  
  
  temp_time_load_extension = data.frame("time" = Tnew,
                                        "load" = sample$load[index_to_plot],
                                        "extension" = Enew, 
                                        "protocol" = rep(protocol_name, length(index_to_plot)),
                                        "id" = rep(id, length(index_to_plot)))
  time_load_extension_2 = rbind(time_load_extension_2, temp_time_load_extension)
}

p_el_cond1_cond3_global = ggplot(data = time_load_extension_2, aes(x = extension, y = load, color = protocol, fill = id)) +
  geom_path() +
  theme_minimal() + 
  #xlim(range_time) + 
  #ylim(range_load) +
  theme(legend.position = "none")
#facet_wrap(species ~ ., scales = "free")

ggsave(file = paste0(plot_path_superposition, "/superposition_ext_cond3_cond1_Drosophila_melanogaster", ".pdf"), 
       plot=p_el_cond1_cond3_global, width=16, height=8, device = "pdf")








