rm(list = ls())

library("readxl")
library("config")


# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "default")

# retrieve parameters
path_data = opt$batches
path_output_file_data = opt$concatenate_file
path_output_file_id = opt$concatenate_id_file

# get all batch file (.xlsx file)
list_infile = list.files(path_data, pattern = ".xlsx$", full.names = T)

nb_row = 500 # number of measurements (ie line) expected for each pupa

merge_pupe_data = ''
merge_id = c()
merge_comment = c()

for (infile in list_infile){
  sheet1 = read_excel(infile, sheet = 1)
  sheet2 = read_excel(infile, sheet = 2)
  
  list_id = sheet2$Sample_ID
  list_comment = sheet2$`Comment on this sample`
  
  # check the concordance of dimensions between sheet 1 and 2
  if (dim(sheet1)[2] / 3 != length(list_id)){
    # skip file with with pb (doesn't stop the script)
    print(paste0("Pb in: ", infile, " - file ignored"))
    next
  }
  
  pupes_data = ''
  for (i in seq(1, ncol(sheet1), by = 3)) {
    temp = sheet1[, c(i,(i+1), (i+2))]
    colnames(temp) = c("Time", "Load", "Extension")
    while(nrow(temp) < nb_row){
      temp = rbind(temp, rep(NA, 3))
    }
    if (pupes_data == ''){
      pupes_data = temp
    } else {
      pupes_data = cbind(pupes_data, temp)
    }
  }
  
  if (merge_pupe_data == '') {
    merge_pupe_data = pupes_data
  }else {
    merge_pupe_data = cbind(merge_pupe_data, pupes_data)
  }
  
  merge_id = c(merge_id, list_id)
  merge_comment = c(merge_comment, list_comment)
}

colnames(merge_pupe_data) = rep(c("Time", "Load", "Extension"), times = length(merge_id))
  
merge_comment_id = data.frame(
  "Comment on this sample" = merge_comment,
  "Sample_ID" = merge_id)


write.table(merge_pupe_data, file=path_output_file_data, row.names = F, quote = F, sep = "\t")
write.table(merge_comment_id, file=path_output_file_id, row.names = F, quote = F, sep = "\t")

