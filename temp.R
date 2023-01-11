rm(list = ls())

library("config")


# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "default")

# retrieve parameters
# Input
path_integral = opt$integral_path
path_output = opt$index_path

file_path_integral_not_ok = paste0(path_integral, "/integral_id_not_running.log")
file_path_jalon_not_ok = paste0(dirname(path_output), "/index_id_not_running.log")

integral = read.table(file_path_integral_not_ok)
jalon = read.table(file_path_jalon_not_ok)

integral[which(! (integral %in% jalon))]