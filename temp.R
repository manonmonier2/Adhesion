rm(list = ls())

library("config")
library("dplyr")

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "portable")

# retrieve parameters
# Input
path_integral = opt$integral_path
path_output = opt$index_path
path_metadata_file = opt$concatenate_metadata


file_path_integral_not_ok = paste0(path_integral, "/integral_id_not_running.log")
file_path_jalon_not_ok = paste0(dirname(path_output), "/index_id_not_running.log")

integral = read.table(file_path_integral_not_ok)$V1
jalon = read.table(file_path_jalon_not_ok)$V1

sort(as.numeric(integral[which(! (integral %in% jalon))]))

metadata = read.table(path_metadata_file, sep = "\t", header = T)

temp = metadata %>% filter(Sample_ID == "2022053107")
