rm(list = ls())

library("readxl")
library("config")


# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "default")

# retrieve parameters
path_data = opt$protocol
path_output_file = opt$concatenate_protocol_file

# get all batch file (.xlsx file)
list_infile = list.files(path_data, pattern = ".xlsx$", full.names = T)

#
raw_name = c("Drosohila_hydei", "Drosophila.nanoptera", "Drosophila_malanogaster", "Drosophila_pachae", "Drosophila_Virilis", "Scaptodrosophila_lebanonen", "Drosophila_nanoptera")
correct_name = c("Drosophila_hydei", "Drosophila_nannoptera", "Drosophila_melanogaster", "Drosophila_pachea", "Drosophila_virilis", "Scaptodrosophila_lebanonensis", "Drosophila_nannoptera")

raw_comment = c("cuticle_broked", "cuticule_broke", "cuticule broke", "cuticle broke", "no_tape","no_scotch", "two_pupae_too_close", "2 at 1 time", "not detached", "not normal", "pbm_machine", "attached_from_the_bottom", "2_at_1_time", "pb_0", "pb_NA", "not_normal", "noscotch_notbroken", "tape_attached")
correct_comment = c("cuticle_broke", "cuticle_broke", "cuticle_broke", "cuticle_broke", "no_adhesive_paper", "no_adhesive_paper", "two_pupae", "two_pupae", "not_detached", "pb_machine", "pb_machine", "attached_at_the_bottom", "two_pupae", "pb_machine", "pb_machine", "pb_machine", "no_adhesive_paper", "pb_scotch")

temp = data.frame(raw = raw_comment,
                  correct = correct_comment)
#

data_df = data.frame()
protocol_vect = c()

for (infile in list_infile){
  
  # load sheet 1
  sheet =  read_excel(infile, sheet = 1)
  
  # deal with supplementary NA column (remove it)
  sheet = sheet[, apply(sheet, 2, function(x) ! sum(is.na(x)) == nrow(sheet))]
    
  # rename species
  list_species_name = sheet$Species
  list_corrected_species_name = unlist(lapply(list_species_name, function(x) if(x %in% raw_name) {correct_name[x == raw_name]} else {x}))
  sheet$Species = list_corrected_species_name
  
  # comment correction
  list_comment = sheet$Comment
  list_corrected_comment = unlist(lapply(list_comment, function(x) if(x %in% raw_comment) {correct_comment[x == raw_comment]} else {x}))
  sheet$Comment = list_corrected_comment
  
  
  # convert the date format to character
  sheet$Time_on_substrate = as.character(sheet$Time_on_substrate)
  sheet$Timestamp = as.character(sheet$Timestamp)
  
  # concatenate the first 15 columns
  data_df = rbind(data_df, sheet[, 1:15])
  colnames(data_df)
  colnames(sheet)
  
  # detect the type of protocol (2 types: 'default' for 'Manon_results_file' or 'specific'. For 'specific', protocols are retrieved in protocol column (16))
  if (basename(infile) == "Manon_results_file.xlsx"){
    protocol_vect = c(protocol_vect, rep("default", nrow(sheet)))
  } else {
    protocol_vect = c(protocol_vect, unlist(sheet[, 16]))
  }
}

sort(unique(protocol_vect))

sort(unique(data_df$Comment))
table(data_df$Comment)

sort(unique(data_df$Species))

protocol_df = data.frame()
unique(protocol_vect)


# colnames(merge_pupe_data) = rep(c("Time", "Load", "Extension"), times = length(merge_id))
# 
# merge_comment_id = data.frame(
#   "Comment on this sample" = merge_comment,
#   "Sample_ID" = merge_id)
# 
# # write the output files (create repository if necessary)
# dir.create(dirname(path_output_file_data), showWarnings = FALSE)
# write.table(merge_pupe_data, file=path_output_file_data, row.names = F, quote = F, sep = "\t")
# dir.create(dirname(path_output_file_id), showWarnings = FALSE)
# write.table(merge_comment_id, file=path_output_file_id, row.names = F, quote = F, sep = "\t")

