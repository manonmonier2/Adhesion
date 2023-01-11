rm(list = ls())

library("config")
library("ggplot2")
library("dplyr")
# library(svglite)

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "default")

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


### test error bar (centrer sur la mediane (par espèce))

# raideur_moyenne
ggplot(gg_data %>% filter(Comment == "ok")
       , aes(x = detachment, y = raideur_moyenne, color = Species)) +
  geom_point() +
  # geom_crossbar() +
  geom_errorbar(ymin = 0.1, ymax = 0.1) +
  # geom_errorbarh(aes(xmin = 0.1, xmax = 0.2)) +
  theme_bw()

# Create a simple example dataset
df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p = ggplot(df, aes(trt, resp, colour = group))
+ geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)
+ geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)

