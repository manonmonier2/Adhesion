rm(list = ls())

library("readxl")
library("config")
library("dplyr")

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "manon_acanthoptera")

# retrieve parameters
# Input
path_data = opt$metadata
path_imagej = opt$imagej_path

# Output
path_output_file = opt$concatenate_metadata

#### FUNCTIONS


load_sheet1 = function(infile){
  # load sheet 1
  sheet =  read_excel(infile, sheet = 1)
  
  # deal with supplementary NA column (remove it)
  sheet = sheet[, apply(sheet, 2, function(x) ! sum(is.na(x)) == nrow(sheet))]
  
  return(sheet)
}


#### MAIN

#
raw_name = c("Drosohila_hydei", "Drosophila.nanoptera", "Drosophila_malanogaster", "Drosophila_pachae", "Drosophila_Virilis", "Scaptodrosophila_lebanonen", "Drosophila_nanoptera", "Drosophila_lachaisei", "Drosophila yakuba")
correct_name = c("Drosophila_hydei", "Drosophila_nannoptera", "Drosophila_melanogaster", "Drosophila_pachea", "Drosophila_virilis", "Scaptodrosophila_lebanonensis", "Drosophila_nannoptera", "Zaprionus_lachaisei", "Drosophila_yakuba")

raw_comment = c("cuticle_broked", "cuticule_broke", "cuticule broke", "cuticle broke", "no_tape","no_scotch", "two_pupae_too_close", "2 at 1 time", "not detached", "not normal", "pbm_machine", "attached_from_the_bottom", "2_at_1_time", "pb_0", "pb_NA", "not_normal", "noscotch_notbroken", "tape_attached")
correct_comment = c("cuticle_broke", "cuticle_broke", "cuticle_broke", "cuticle_broke", "no_adhesive_paper", "no_adhesive_paper", "two_pupae", "two_pupae", "not_detached", "pb_machine", "pb_machine", "attached_at_the_bottom", "two_pupae", "pb_machine", "pb_machine", "pb_machine", "no_adhesive_paper", "pb_scotch")

raw_protocol = c("noscotch", "no_scotch", "nocond", "no_cond", "default", "strong", "strongforce", "scotch_fin", "strongtape", "tesa", "div3", "x3", "scotch_fin_strong_force", "3japf", "cond1", "cond2", "cond3", "0,25 N")
correct_protocol = c("no tape ; glue", "no tape ; glue", "standard", "standard", "standard", "0.25 N", "0.25 N", "1 strong tape ; glue", "1 strong tape ; glue", "standard", "speed /3", "speed x3", "1 strong tape ; glue ; 0.25 N", "3 d", "1 tape ; no glue", "2 tapes ; no glue", "1 tape ; no glue ; speed x3", "0.25 N")

species_with_incorrect_stock = c("Drosophila_takahashii", "Drosophila_pachea", "Drosophila_nannoptera", "Drosophila_pseudoobscura", "Drosophila_eugracilis", "Drosophila_elegans", "Drosophila_prostipennis", "Drosophila_funebris", "Drosophila_rhopaloa", "Drosophila_kurseongensis", "Scaptodrosophila_lebanonensis", "Zaprionus_lachaisei", "Drosophila_malerkotliana", "Zaprionus_indianus", "Drosophila_ananassae", "Drosophila_immigrans", "Drosophila_hydei", "Drosophila_quadraria", "Drosophila_tropicalis", "Drosophila_virilis")
correct_stock_by_species = c("14022-0311.07", "15090-1698.01_14.2", "15090-1692.00", "14011-0121.94", "Prud_homme_Gompel", "14027-0461.03", "14022-0291.00", "M_Monier", "BaVi067", "SaPa058", "J_David", "S_Prigent", "S_Prigent", "S_Prigent", "Prud_homme_Gompel", "F_Borne", "F_Borne", "J_David", "S_Prigent", "15010-1051.86")

raw_stock = c("biar001", "Biar001", "biariso001", "biariso003", "biarmipes", "Virilis", "Scalaris", "Lachaisei", "Nanoptera", "Pachea", "CantonS", "canronS", "Hydei", "Immigrans", "Malerkotliana", "maleskotliana", "Md221", "suzvincennes", "suzukii_vincennes", "Indianus", "Kuseongensis", "Kurseongensis", "suzAM", "suzJD")
correct_stock = c("Iso_001", "Iso_001", "Iso_001", "Iso_003", "G224", "virilis", "scalaris", "lachaisei", "nanoptera", "pachea", "cantonS", "cantonS", "hydei", "immigrans", "malerkotliana", "malerkotliana", "md221", "suzukii_Vincennes", "suzukii_Vincennes", "indianus", "kurseongensis", "kurseongensis", "AM", "JD")
#

# get all metadata file (.xlsx file)
list_infile = list.files(path_data, pattern = ".xlsx$", full.names = T)

# Manon_results_file.xlsx contains all the metadata id, the others files contains protocol precision
index_main_medadata = which(basename(list_infile) == "Manon_results_file.xlsx")

data_df = load_sheet1(list_infile[index_main_medadata])[, 1:15]
data_df = cbind(data_df, rep("standard", nrow(data_df)))
names(data_df)[names(data_df) == tail(names(data_df), 1)] = 'Protocol'
list_infile = list_infile[-index_main_medadata]

# pupa_adhesion_print.xlsx contains all the metadata id for Flora's data
index_flora_medadata = which(basename(list_infile) == "pupa_adhesion_print.xlsx")
path_flora_metadata = list_infile[index_flora_medadata]

list_infile = list_infile[-index_flora_medadata]

not_ok = c()

# check for flora metadata (.csv file)
list_infile_flora = list.files(path_data, pattern = ".csv$", full.names = T)

for (infile in list_infile){
  
  sheet = load_sheet1(infile)
  
  # browse sample id on the sheet
  for (id in sheet$Sample_ID){
    hit = which(data_df$Sample_ID == id)
    hit_in_sheet = which(sheet$Sample_ID == id)
    # one hit in each file
    if (length(hit) == 1 && length(hit_in_sheet) == 1){
      if (data_df$Protocol[hit] != "standard"){
        break("duplicate between protocol files")
      }
      data_df[hit, 16] = sheet[hit_in_sheet, 16]
    } else {
      not_ok = c(not_ok, id)
    }
  }
}


# flora metadata
sheet = as.data.frame(load_sheet1(path_flora_metadata))

for(row in 1:nrow(sheet)) {
  handler_row_df = c(
    "Sample_ID" = sheet[row, "Sample_ID"],
    "Experimenter" = "Flora",
    "Species" = sheet[row, "Species"],
    "Stock" = sheet[row, "Stock"],
    "Load at Minimum Load(N)" = NA,
    "Temperature_assay_°C(°C)" = sheet[row, "Temperature_assay"],
    "Temperature_culture_°C(°C)" = sheet[row, "Temperature_culture"],
    "Humidity_%" = sheet[row, "Humidity_."],
    "Pressure" = sheet[row, "Pressure_mba"],
    "Type of substrate" = NA,
    "Substrate number" = NA,
    "Comment on this sample" = sheet[row, "Comment_on_this_sample"],
    "Timestamp" = NA,
    "force_detachment_mN" = sheet[row, "force_detachment_mN"],
    "Time_on_substrate" = NA,
    "Protocol" = sheet[row, "protocol"]
  )
  
  # check id unicity
  hit = which(sheet[row, 1] %in% sheet[, 1])
  hit_in_data_df = which(data_df$Sample_ID == sheet[row, 1])
  # one hit in each file
  if (length(hit) == 1 && length(hit_in_data_df) == 0){
    data_df = rbind(data_df, handler_row_df)
  } else {
    not_ok = c(not_ok, sheet[row, 1])
  }
}


write.table(not_ok, file=paste0(dirname(path_output_file), "/id_not_found_in_manon_results.log"), row.names = F, col.names = F, quote = F, sep = "\t")

# rename species
list_species_name = data_df$Species
list_corrected_species_name = unlist(lapply(list_species_name, function(x) if(x %in% raw_name) {correct_name[x == raw_name]} else {x}))
data_df$Species = list_corrected_species_name

# comment correction
colnames(data_df)[which(colnames(data_df) == "Comment on this sample")] = "Comment"
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

# protocol correction
list_protocol = data_df$Protocol
list_corrected_protocol = unlist(lapply(list_protocol, function(x) if(x %in% raw_protocol) {correct_protocol[x == raw_protocol]} else {x}))
data_df$Protocol = list_corrected_protocol

temp = data.frame(raw_name = raw_name,
                  correct_name = correct_name)
write.table(temp, file=paste0(dirname(path_output_file), "/species_name_correction.csv"), row.names = F, quote = F, sep = "\t")

temp = data.frame(raw_comment = raw_comment,
                  correct_comment = correct_comment)
write.table(temp, file=paste0(dirname(path_output_file), "/comment_correction.csv"), row.names = F, quote = F, sep = "\t")

temp = data.frame(raw_protocol = raw_protocol,
                  correct_protocol = correct_protocol)
write.table(temp, file=paste0(dirname(path_output_file), "/protocol_correction.csv"), row.names = F, quote = F, sep = "\t")

temp = data.frame(raw_stock = raw_stock,
                  correct_stock = correct_stock)
write.table(temp, file=paste0(dirname(path_output_file), "/stock_correction.csv"), row.names = F, quote = F, sep = "\t")


# Integration of the imageJ results

concatenate_data_imagej = data.frame()
list_type = c("size", "glue", "side")
for(file_type in list_type){
  list_imagej_file = list.files(path_imagej, full.names = T, 
                                pattern = paste0(file_type, ".csv$"))
  concatenate_by_type = data.frame()
  for(imagej_file in list_imagej_file){
    imagej_data = read.table(imagej_file, sep = ",", header = T)
    
    # remove duplicated unique id (keep first occurrences of each duplicated id)
    
    imagej_id = sub("^(\\d+)\\D+.*$", "\\1", imagej_data$Label)
    imagej_data$Label = imagej_id
    
    imagej_data = imagej_data[!duplicated(imagej_data$Label), ]
    
    if (file_type == "glue"){
      temp_imagej_data = data.frame(
        "Sample_ID" = imagej_data$Label,
        "Glue_area" = imagej_data$Area)
    } else if (file_type == "size") {
      temp_imagej_data = cbind(data.frame(
        "Sample_ID" = imagej_data$Label),
        imagej_data)
    } else if (file_type == "side") {
      temp_imagej_data = cbind(data.frame(
        "Sample_ID" = imagej_data$Label),
        "Side" = imagej_data$Length)
    }
    
    concatenate_by_type = rbind(concatenate_by_type, 
                                temp_imagej_data)
  }
  
  if (nrow(concatenate_data_imagej) > 0){
    concatenate_data_imagej = base::merge(concatenate_data_imagej,
                                          concatenate_by_type,
                                          all.x = T,
                                          all.y = T)
  } else {
    concatenate_data_imagej = concatenate_by_type
  }
}

id_not_running = c()
# id not in metadata
id_not_in_metadata = which(! concatenate_data_imagej$Sample_ID %in% 
                             data_df$Sample_ID)

if(length(id_not_in_metadata) > 0){
  id_not_running = c(id_not_running, 
                     concatenate_data_imagej$Sample_ID[id_not_in_metadata])
  concatenate_data_imagej = concatenate_data_imagej[-id_not_in_metadata, ]
}

# id not unique in imageJ
count_by_id = table(concatenate_data_imagej$Sample_ID)
not_unique_id = names(count_by_id[which(count_by_id > 1)])

if(length(not_unique_id) > 0){
  id_not_running = c(id_not_running, 
                     unique(concatenate_data_imagej$Sample_ID[not_unique_id]))
  concatenate_data_imagej = concatenate_data_imagej[-which(concatenate_data_imagej$Sample_ID %in% 
                                                             not_unique_id), ]
}

write.table(id_not_running, file = paste0(dirname(path_imagej), "/imagej_id_not_running.log"), row.names = F, col.names = F, quote = F)

# remove "Label" col
concatenate_data_imagej = concatenate_data_imagej[, -2]
data_df = base::merge(data_df, concatenate_data_imagej, 
                      by = "Sample_ID", all = T)

# second run
path_imagej_second_run = paste0(dirname(path_imagej), "/second_run/second_run.csv")
path_random_names = paste0(dirname(path_imagej), "/second_run/random_names.csv")

imagej_data = read.table(path_imagej_second_run, sep = ";", header = T)
random_names = read.table(path_random_names, sep = ",", header = T)
random_names = random_names[-which(random_names$result_pupa_glue.Label == ""), ]

data = base::merge(imagej_data, random_names, 
                   by = "random", all = T)
data = data[!is.na(data$Area_random), ]


data = data[c("result_pupa_glue.Label", "Area_random")]
colnames(data) = c("Sample_ID", "Glue_area_second")
data$Sample_ID = sub("^(\\d+)\\D+.*$", "\\1", data$Sample_ID)

# remove id non present in the current metadata dataframe
data = data[data$Sample_ID %in% data_df$Sample_ID, ]

data_df = base::merge(data_df, data, by = "Sample_ID", all = T)

# write the output file (create repository if necessary)
dir.create(dirname(path_output_file), showWarnings = FALSE)
write.table(data_df, file=path_output_file, row.names = F, quote = F, sep = "\t")
