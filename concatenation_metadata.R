rm(list = ls())

library("readxl")
library("config")


# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "default")

# retrieve parameters
# Input
path_data = opt$metadata

# Output
path_output_file = opt$concatenate_metadata

#### MAIN

# get all batch file (.xlsx file)
list_infile = list.files(path_data, pattern = ".xlsx$", full.names = T)

#
raw_name = c("Drosohila_hydei", "Drosophila.nanoptera", "Drosophila_malanogaster", "Drosophila_pachae", "Drosophila_Virilis", "Scaptodrosophila_lebanonen", "Drosophila_nanoptera")
correct_name = c("Drosophila_hydei", "Drosophila_nannoptera", "Drosophila_melanogaster", "Drosophila_pachea", "Drosophila_virilis", "Scaptodrosophila_lebanonensis", "Drosophila_nannoptera")

raw_comment = c("cuticle_broked", "cuticule_broke", "cuticule broke", "cuticle broke", "no_tape","no_scotch", "two_pupae_too_close", "2 at 1 time", "not detached", "not normal", "pbm_machine", "attached_from_the_bottom", "2_at_1_time", "pb_0", "pb_NA", "not_normal", "noscotch_notbroken", "tape_attached")
correct_comment = c("cuticle_broke", "cuticle_broke", "cuticle_broke", "cuticle_broke", "no_adhesive_paper", "no_adhesive_paper", "two_pupae", "two_pupae", "not_detached", "pb_machine", "pb_machine", "attached_at_the_bottom", "two_pupae", "pb_machine", "pb_machine", "pb_machine", "no_adhesive_paper", "pb_scotch")

raw_protocol = c("noscotch", "nocond", "strong", "scotch_fin", "tesa")
correct_protocol = c("no_scotch", "no_cond", "strongforce", "strongtape", "default")

species_with_incorrect_stock = c("Drosophila_takahashii", "Drosophila_pachea", "Drosophila_nannoptera", "Drosophila_pseudoobscura", "Drosophila_eugracilis", "Drosophila_elegans", "Drosophila_prostipennis", "Drosophila_funebris", "Drosophila_rhopaloa", "Drosophila_kurseongensis", "Scaptodrosophila_lebanonensis", "Zaprionus_lachaisei", "Drosophila_malerkotliana", "Zaprionus_indianus", "Drosophila_ananassae", "Drosophila_immigrans", "Drosophila_hydei", "Drosophila_quadraria", "Drosophila_tropicalis", "Drosophila_virilis")
correct_stock_by_species = c("14022-0311.07", "15090-1698.01_14.2", "15090-1692.00", "14011-0121.94", "Prud_homme_Gompel", "14027-0461.03", "14022-0291.00", "M_Monier", "BaVi067", "SaPa058", "J_David", "S_Prigent", "S_Prigent", "S_Prigent", "Prud_homme_Gompel", "F_Borne", "F_Borne", "J_David", "S_Prigent", "15010-1051.86")

raw_stock = c("biar001", "Biar001", "biariso001", "biariso003", "biarmipes")
correct_stock = c("Iso_001", "Iso_001", "Iso_001", "Iso_003", "G224")
#

data_df = data.frame()
protocol_vect = c()

for (infile in list_infile){
  
  # load sheet 1
  sheet =  read_excel(infile, sheet = 1)
  
  # deal with supplementary NA column (remove it)
  sheet = sheet[, apply(sheet, 2, function(x) ! sum(is.na(x)) == nrow(sheet))]
    
  
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
    
    # protocol correction
    list_protocol = unlist(sheet[, 16])
    list_corrected_protocol = unlist(lapply(list_protocol, function(x) if(x %in% raw_protocol) {correct_protocol[x == raw_protocol]} else {x}))
    sheet[, 16] = list_corrected_protocol
    protocol_vect = c(protocol_vect, unlist(sheet[, 16]))
  }
}

# rename species
list_species_name = data_df$Species
list_corrected_species_name = unlist(lapply(list_species_name, function(x) if(x %in% raw_name) {correct_name[x == raw_name]} else {x}))
data_df$Species = list_corrected_species_name

# comment correction
list_comment = data_df$Comment
list_corrected_comment = unlist(lapply(list_comment, function(x) if(x %in% raw_comment) {correct_comment[x == raw_comment]} else {x}))
data_df$Comment = list_corrected_comment


# stock correction by stock
list_stock = data_df$Stock
list_corrected_stock = unlist(lapply(list_stock, function(x) if(x %in% raw_stock) {correct_stock[x == raw_stock]} else {x}))
data_df$Stock = list_corrected_stock

# stock correction by species
list_stock = data_df$Stock
list_corrected_stock = unlist(lapply(1:length(data_df$Species), function(x) if(data_df$Species[x] %in% species_with_incorrect_stock) {correct_stock_by_species[data_df$Species[x] == species_with_incorrect_stock]} else {data_df$Stock[x]}))
data_df$Stock = list_corrected_stock

df_protocol_to_condition = data.frame(
  "cond1" = c(T,F,F,F,F,F,F,F,F,F,F,F,F),
  "cond2" = c(T,F,T,F,F,F,F,F,F,F,F,F,F),
  "cond3" = c(T,F,F,F,T,F,F,F,F,F,F,F,F),
  "div3" = c(F,F,F,T,F,F,F,F,F,F,F,F,F),
  "x3" = c(F,F,F,F,T,F,F,F,F,F,F,F,F),
  "0s" = c(F,F,F,F,F,T,F,F,F,F,F,F,F),
  "5min" = c(F,F,F,F,F,F,T,F,F,F,F,F,F),
  "no_scotch" = c(F,T,F,F,F,F,F,F,F,F,F,F,F),
  "strongforce" = c(F,F,F,F,F,F,F,T,F,F,F,F,F),
  "3japf" = c(F,F,F,F,F,F,F,F,T,F,F,F,F),
  "no_cond" = c(F,F,F,F,F,F,F,F,F,T,F,F,F),
  "water" = c(T,F,F,F,F,F,F,F,F,F,T,F,F),
  "strongtape" = c(F,F,F,F,F,F,F,F,F,F,F,T,F),
  "scotch_fin_strong_force" = c(F,F,F,F,F,F,F,T,F,F,F,T,F),
  "default" = c(F,F,F,F,F,F,F,F,F,F,F,F,T),
  check.names=FALSE
)

rownames(df_protocol_to_condition) = paste0("protocol",1:(nrow(df_protocol_to_condition)))

protocol_df = as.data.frame(do.call(rbind, lapply(protocol_vect, function(x) x == colnames(df_protocol_to_condition))))
colnames(protocol_df) = colnames(df_protocol_to_condition)

data_df = cbind(data_df, protocol_df)

# write the output file (create repository if necessary)
dir.create(dirname(path_output_file), showWarnings = FALSE)
write.table(data_df, file=path_output_file, row.names = F, quote = F, sep = "\t")


