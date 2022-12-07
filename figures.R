rm(list = ls())

library("config")
library("ggplot2")
library("dplyr")
# library(svglite)

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "portable")

# retrieve parameters
# Input
path_metadata_file = opt$concatenate_metadata
path_index = opt$index_path
path_batch_by_id = opt$batch_by_id

index_table = read.table(path_index, header = T, sep = "\t")
metadata = read.table(path_metadata_file, sep = "\t", header = T)

list_id = index_table$id
gg_data = metadata[which(list_id %in% metadata$Sample_ID), ]

detachment = c()
raideur_moyenne = c()
for (id in list_id){
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
  current_metadata = metadata[metadata$Sample_ID == id, ]
  current_index = index_table[index_table$id == id, ]
  current_detachment = min(sample$load[current_index$index_4:current_index$index_5])
  current_raideur_moyenne = (sample$load[current_index$index_2] - sample$load[current_index$index_1]) / (sample$extension[current_index$index_2] - sample$extension[current_index$index_1])
  
  detachment = c(detachment, current_detachment)
  raideur_moyenne = c(raideur_moyenne, current_raideur_moyenne)
}



gg_data = cbind(gg_data, 
                detachment,
                raideur_moyenne)


# detachment
ggplot(gg_data %>% filter(Comment == "ok" & default),
       aes(x = Species, y = detachment, fill = Species)) +
  geom_boxplot() +
  coord_flip()+
  theme_bw()

ggplot(gg_data %>% filter(Comment != "ok" | ! default)
       , aes(x = Species, y = detachment, fill = Species)) +
  geom_boxplot() +
  coord_flip()+
  theme_bw()

# raideur_moyenne
ggplot(gg_data %>% filter(Comment == "ok")
       , aes(x = Species, y = raideur_moyenne, fill = Species)) +
  geom_boxplot() +
  coord_flip()+
  theme_bw()



