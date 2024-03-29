rm(list = ls())

library("config")
library("dplyr")

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "portable")

# retrieve parameters
# Input
path_batch_by_id = opt$batch_by_id
index_path = opt$index_path

# Output
path_output = opt$plot_path
path_integral = opt$integral_path

#### MAIN

df_index = read.table(index_path, header = T, sep = "\t")


log_path = paste0(path_integral, "/log_file.txt")

dir.create(path_output, showWarnings = FALSE)
dir.create(path_integral, showWarnings = FALSE)

write(toString(Sys.time()), log_path, append=TRUE)

# get id from batch directory
list_id = tools::file_path_sans_ext(list.files(path_batch_by_id, pattern = ".*.csv$"))

not_running = c() # vector containing all the non running batch name

vect_id = c()
vect_compression = c()
vect_decompression = c()

for (name in list_id) {
  run = tryCatch(
    expr = {
      #
      # loading batch data
      batch_data = read.table(paste0(path_batch_by_id, "/", name, '.csv'), sep = "\t", header = T)
      
      # restrict milestone data to the current batch
      ## check if the milestones exist for this batch data (skip it if not)
      if (! name %in% df_index$id) next
      
      batch_milestone = df_index %>% filter(id == name)
      
      # compression zone
      ## definition: from contact to pupae (index_1) to maximum extension (index_2)
      data_compression = data.frame("load" = batch_data$load[batch_milestone$index_1 : batch_milestone$index_2],
                                    "extension" = batch_data$extension[batch_milestone$index_1 : batch_milestone$index_2])
      
      ## interpolation
      interpolation_compression = approxfun(
        data_compression$extension,
        data_compression$load, 
        ties = "ordered",
        na.rm = T,
        rule = 2    # rule = 2 allow the interpolation function to not return NA
        )
      
      # plot(data_compression$extension, data_compression$load, type = "l")
      # points(x = seq(min(data_compression$extension), max(data_compression$extension), 0.01),
      #        y = interpolation_compression(seq(min(data_compression$extension), max(data_compression$extension), 0.01)))
      # curve(interpolation_compression, from = min(data_compression$extension, na.rm = T), to = max(data_compression$extension, na.rm = T))

      ## integration
      integration_limit = range(data_compression$extension, na.rm = T)
      integration_compression = integrate(
        f = interpolation_compression,
        lower = integration_limit[1], 
        upper = integration_limit[2],
        subdivisions = 2000
        )
      
      
      # decompression zone
      ## definition: from contact to to maximum extension (index_2) to pupae full detachment (index_5)
      data_decompression = data.frame("load" = batch_data$load[batch_milestone$index_2 : batch_milestone$index_5],
                                    "extension" = batch_data$extension[batch_milestone$index_2 : batch_milestone$index_5])
      
      ## interpolation
      interpolation_decompression = approxfun(
        data_decompression$extension,
        data_decompression$load, 
        na.rm = T,
        rule = 2    # rule = 2 allow the interpolation function to not return NA
      )
      
      # plot(data_decompression$extension, data_decompression$load, type = "l")
      # points(x = seq(min(data_decompression$extension), max(data_decompression$extension), 0.01),
      #               y = interpolation_decompression(seq(min(data_decompression$extension), max(data_decompression$extension), 0.01)))
      # curve(interpolation_decompression, from = min(data_decompression$extension, na.rm = T), to = max(data_decompression$extension, na.rm = T))

      ## integration
      integration_limit = range(data_decompression$extension, na.rm = T)
      integration_decompression = integrate(
        f = interpolation_decompression,
        lower = integration_limit[1], 
        upper = integration_limit[2],
        subdivisions = 2000
        )
      
      # save result for output
      vect_id = c(vect_id, name)
      vect_compression = c(vect_compression, integration_compression$value)
      vect_decompression = c(vect_decompression, integration_decompression$value)
      
    },
    error = function(e){ 
      print(name)
      return(name)
    }
  )
  
  # retrieve the not running id
  if (length(run) == 1){
    if (run %in% list_id) {
      not_running = c(not_running, name)
    }
  }
}

df_res = data.frame("id" = vect_id,
                     "integrale_compression" = abs(vect_compression),
                     "integrale_decompression" = abs(vect_decompression),
                     "difference_integrales" = abs(vect_compression - vect_decompression))

file_path = paste(path_integral, "/integral.csv", sep = "")
write.table(df_res, file = file_path, quote = F, col.names = T, row.names = F, sep = "\t")

file_path_not_running = paste(path_integral, "/integral_id_not_running.log", sep = "")
write.table(not_running, file = file_path_not_running, row.names = F, col.names = F, quote = F)