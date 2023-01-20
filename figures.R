rm(list = ls())

library("config")
library("ggplot2")
library("dplyr")

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
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "default")

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
for (id in gg_data$Sample_ID){
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
  current_metadata = metadata[metadata$Sample_ID == id, ]
  current_index = index_table[index_table$id == id, ]
  
  current_detachment_force = min(sample$load[current_index$index_4:current_index$index_5])
  if(length(which(energy_table$id == id)) == 1) {
    current_energy = energy_table$integrale_zone_negative[energy_table$id == id]
  } else {
    current_energy = NA
  }
    
  current_rigidity = (sample$load[current_index$index_2] - sample$load[current_index$index_1]) / (sample$extension[current_index$index_2] - sample$extension[current_index$index_1])
  current_position_difference = abs(sample$extension[current_index$index_1] - sample$extension[current_index$index_5])
  current_detachment_position = sample$extension[current_index$index_5]
  
  detachment_force = c(detachment_force, current_detachment_force)
  energy = c(energy, current_energy)
  rigidity = c(rigidity, current_rigidity)
  position_difference = c(position_difference, current_position_difference)
  detachment_position = c(detachment_position, current_detachment_position)
}

gg_data = cbind(gg_data, 
                detachment_force,
                energy,
                rigidity,
                position_difference,
                detachment_position,
                log10_na(detachment_force),
                log10_na(energy),
                log10_na(rigidity),
                log10_na(position_difference),
                log10_na(detachment_position)
                )

colnames(gg_data)[22:26] = c("log10_detachment_force", "log10_energy", "log10_rigidity", "log10_position_difference", "log10_detachment_position")

# exclusion of flora data
gg_data = gg_data %>% filter(Experimenter != "Flora")

parameter_list = c("detachment_force", "energy", "rigidity", "position_difference", "detachment_position",
                   "log10_detachment_force", "log10_energy", "log10_rigidity", "log10_position_difference", "log10_detachment_position")
lab_list = c("Detachment force", "Energy", "Rigidity", "Position difference", "Detachment position",
             "log(Detachment force)", "log(Energy)", "log(Rigidity)", "log(Position difference)", "log(Detachment position)")
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
  p = ggplot(gg_data %>% filter(Comment == "ok"),
             aes_string(x = "Species", y = parameter_list[i], fill = "Species")) +
    geom_boxplot() +
    coord_flip() +
    theme_bw(base_size = 18) +
    ylab(lab_list[i]) +
    xlab("Species")
  ggsave(file = paste0(plot_path_one_parameter_by_species, "/", parameter_list[i], ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
  
}


## by protocol
plot_path_one_parameter_by_protocol = paste0(plot_path, "/one_parameter/by_protocol/")
dir.create(plot_path_one_parameter_by_protocol, showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){
  p = ggplot(gg_data %>% filter(Comment == "ok"),
             aes_string(x = "Protocol", y = parameter_list[i], fill = "Protocol")) +
    geom_boxplot() +
    coord_flip() +
    theme_bw(base_size = 22) +
    ylab(lab_list[i]) +
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
    geom_boxplot() +
    theme_bw(base_size = 15) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ylab(lab_list[i]) +
    xlab("Protocol") +
    ylim(min(temp_data[[parameter_list[i]]]), max(temp_data[[parameter_list[i]]])) +
    ggtitle(paste0(lab_list[i], " by protocol and species")) +
    facet_wrap(Species ~ ., scales = "free") # permet de diviser les fenetres par espece
  
  ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/", parameter_list[i], ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
}

### by protocol for Drosophila_melanogaster
for (i in 1:length(parameter_list)){
  temp_data = gg_data %>% filter(Comment == "ok" & Species == "Drosophila_melanogaster")
  p = ggplot(temp_data,
             aes_string(x = "Protocol", y = parameter_list[i], fill = "Protocol")) +
    geom_boxplot() +
    theme_bw(base_size = 22) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab(lab_list[i]) +
    xlab("Protocol") +
    stat_summary(fun.data = n_fun, geom = "text") +
    ggtitle(paste0(lab_list[i], " by protocol for Drosophila melanogaster")) +
    facet_wrap(Species ~ ., scales = "free")
  
  ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/", parameter_list[i], "_Drosophila_melanogaster", ".pdf"), 
         plot=p, width=16, height=8, device = "pdf")
}

# two parameters plot
## by species
plot_path_two_parameters_by_species = paste0(plot_path, "/two_parameters/by_species/")
dir.create(plot_path_two_parameters_by_species, showWarnings = FALSE, recursive = T)

for (i in 1:length(parameter_list)){
  for (j in 1:length(parameter_list)){
    if (i == j) next
    
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
      xlab(lab_list[i]) +
      ylab(lab_list[j]) +
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
      xlab(lab_list[i]) +
      ylab(lab_list[j]) +
      theme_bw(base_size = 22) +
      ggtitle(paste0("x: ", lab_list[i], " y: ", lab_list[j] ," by protocol for Drosophila melanogaster"))
    
    ggsave(file = paste0(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, "/x_", parameter_list[i], "_y_", parameter_list[j], "_Drosophila_melanogaster", ".pdf"), 
           plot=p, width=16, height=8, device = "pdf")
  }
}
