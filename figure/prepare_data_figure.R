rm(list = ls())

library("config")
library("dplyr")

#### FUNCTIONS ####

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
negative_energy = c()
position_difference = c()
detachment_position = c()
pression_extension = c()
pupa_area = c()
pupa_length = c()

for (id in gg_data$Sample_ID){
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
  current_metadata = metadata[metadata$Sample_ID == id, ]
  current_index = index_table[index_table$id == id, ]
  
  current_detachment_force = -min(sample$load[current_index$index_4:current_index$index_5])
  
  if(length(which(energy_table$id == id)) == 1) {
    current_energy = abs(energy_table$integrale_compression[energy_table$id == id] - energy_table$sum_decompression_negative_and_positive[energy_table$id == id])
    current_energy_negative = 
      abs(energy_table$integrale_decompression_negative[energy_table$id == id])
  } else {
    current_energy = NA
    current_energy_negative = NA
  }
  
  current_rigidity = (sample$load[current_index$index_2] - sample$load[current_index$index_1]) / (sample$extension[current_index$index_2] - sample$extension[current_index$index_1])
  current_position_difference = sample$extension[current_index$index_1] - sample$extension[current_index$index_5]
  current_pression_extension = sample$extension[current_index$index_2] - sample$extension[current_index$index_1]
  
  
  if(length(which(metadata$Sample_ID == id)) == 1) {
    current_pupa_area = ((gg_data$Scale_um[gg_data$Sample_ID == id]^2) * gg_data$Area[gg_data$Sample_ID == id]) / (gg_data$Scale_px[gg_data$Sample_ID == id]^2)
  } else {
    current_pupa_area = NA
  }
  
  if(length(which(metadata$Sample_ID == id)) == 1) {
    current_pupa_length = gg_data$Scale_um[gg_data$Sample_ID == id] * gg_data$Feret[gg_data$Sample_ID == id] / gg_data$Scale_px[gg_data$Sample_ID == id]
  } else {
    current_pupa_length = NA
  }
  
  
  detachment_force = c(detachment_force, current_detachment_force)
  energy = c(energy, current_energy)
  negative_energy = c(negative_energy, current_energy_negative)
  rigidity = c(rigidity, current_rigidity)
  position_difference = c(position_difference, current_position_difference)
  pression_extension = c(pression_extension, current_pression_extension)
  pupa_area = c(pupa_area, current_pupa_area)
  pupa_length = c(pupa_length, current_pupa_length)
  
}

gg_data = cbind(gg_data, 
                detachment_force,
                energy,
                negative_energy,
                rigidity,
                position_difference,
                pression_extension,
                pupa_area,
                pupa_length,
                log10_na(detachment_force),
                log10_na(energy),
                log10_na(negative_energy),
                log10_na(rigidity),
                log10_na(position_difference),
                log10_na(pression_extension),
                log10_na(pupa_area),
                log10_na(pupa_length),
                log10_na(gg_data$Glue_area)
)

colnames(gg_data)[(ncol(gg_data) - 8) : ncol(gg_data)] = c("log10_detachment_force", "log10_energy", "log10_negative_energy", "log10_rigidity", "log10_position_difference", "log10_pression_extension", "log10_pupa_area", "log10_pupa_length", "log10_glue_area")

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

write.table(x = gg_data,
            file = paste0(plot_path, "/data_figure.csv"),
            sep = "\t",
            col.names = T,
            row.names = F,
            quote = F)


