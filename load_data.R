rm(list = ls())

library("readxl")
library("config")
library("cli")

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "portable")

# retrieve parameters
# Input
concatenate_batch_file = opt$concatenate_file
concatenate_id_file = opt$concatenate_id_file
concatenate_metadata_file = opt$concatenate_metadata

# Output
path_batch_by_id = opt$batch_by_id

#### MAIN

# load data
metadata = read.table(concatenate_metadata_file, sep = "\t", header = T, check.names = F)
concatenate_id = read.table(concatenate_id_file, header = T)

# work only with id present and unique in the metadata and id concatenate file
list_unique_id = c()
for (i in 1:length(metadata$Sample_ID)){
  nb_hit_id_file = length(which(concatenate_id$Sample_ID == metadata$Sample_ID[i]))
  # check unicity in the id file
  if (nb_hit_id_file == 1){
    nb_hit_metadata_file = length(which(metadata$Sample_ID == metadata$Sample_ID[i]))
    if (nb_hit_metadata_file == 1){
      list_unique_id = c(list_unique_id, metadata$Sample_ID[i])
    }
  }
}

# restrict the data to the selected id
restricted_metadata = metadata[which(metadata$Sample_ID %in% list_unique_id), ]

# extract data curve from the concatenate batch file
dir.create(path_batch_by_id, showWarnings = FALSE)

batches = read.table(concatenate_batch_file, sep = "\t", header = T)

pupe_id = 0
# browse the concatenate batch file (by column triplet)
for (i in seq(1, ncol(batches), by = 3)) {
  pupe_id = pupe_id + 1
  # get current id
  current_id = concatenate_id$Sample_ID[pupe_id]
  # skip id not present in the restricted metadata file
  if (! current_id %in% restricted_metadata$Sample_ID) next
  
  pupe_data = batches[, c(i,(i+1), (i+2))]
  
  colnames(pupe_data) = c("time", "load", "extension")
  
  # recalibration of the curve to 0 on the first 20 load values
  temp = pupe_data$load[1:20]
  sigma_temp = sd(pupe_data$load[1:20], na.rm = T)
  
  # do not recalibrate the data if the noise sigma is less than 0.01 (ie strong noise)
  if (sigma_temp <= 0.01) {
    new_start = which(abs(temp) >= (0.5 * sigma_temp))[1]
    new_index = new_start:length(pupe_data$load)
    pupe_data = data.frame("time" = pupe_data$time[new_index],
                           "load" = pupe_data$load[new_index] - median(pupe_data$load[1:20]),
                           "extension" = pupe_data$extension[new_index])
  } else {
    pupe_data = data.frame("time" = pupe_data$time,
                           "load" = pupe_data$load,
                           "extension" = pupe_data$extension)
  }
  
  # write pupa data
  file_path = paste0(path_batch_by_id, "/", current_id, ".csv")
  write.table(pupe_data, file = file_path, quote = F, col.names = T, row.names = F, sep = "\t")
}




