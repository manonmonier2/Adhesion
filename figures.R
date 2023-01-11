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
plot_path = opt$plot_path

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



# creation of the stat data frame by species
plot_path_by_species = paste0(plot_path, "/by_species/")
dir.create(plot_path_by_species, showWarnings = FALSE)

gg_stat = gg_data %>%
  filter(Comment == "ok") %>%
  group_by(Species)%>% 
  summarise(Mean_detachment = mean(detachment), 
            Max_detachment = max(detachment), 
            Min_detachment = min(detachment), 
            Median_detachment = median(detachment), 
            Std_detachment = sd(detachment),
            
            Mean_raideur_moyenne = mean(raideur_moyenne), 
            Max_raideur_moyenne = max(raideur_moyenne), 
            Min_raideur_moyenne = min(raideur_moyenne), 
            Median_raideur_moyenne = median(raideur_moyenne), 
            Std_raideur_moyenne = sd(raideur_moyenne))

attach(gg_stat)

p1 = ggplot(gg_stat, 
            aes(x = Median_detachment, y = Median_raideur_moyenne, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Min_detachment, xmax = Max_detachment) +
  geom_errorbar(ymin = Min_raideur_moyenne, ymax = Max_raideur_moyenne) +
  xlim(min(Min_detachment), max(Max_detachment)) +
  ylim(min(Min_raideur_moyenne), max(Max_raideur_moyenne)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = detachment, y = raideur_moyenne), alpha = 0.3) +
  xlab("Detachment") +
  ylab("Raideur moyenne") +
  theme_bw()

ggsave(file = paste0(plot_path_by_species, "/x_detachment_y_raideur_moyenne", ".pdf"), 
       plot=p1, width=16, height=8, device = "pdf")

# creation of the stat data frame by condition
plot_path_by_species = paste0(plot_path, "/by_condition/")
dir.create(plot_path_by_species, showWarnings = FALSE)

list_condition = c(
  "cond1", "cond2", "cond3", "div3", "x3", "X0s", "X5min", "no_scotch", 
  "strongforce", "X3japf", "no_cond", "water", "strongtape", 
  "scotch_fin_strong_force", "default")

list_condition %in% attributes(gg_data)$names

gg_stat = gg_data %>%
  filter(Comment == "ok") %>%
  group_by(Species)%>% 
  summarise(Mean_detachment = mean(detachment), 
            Max_detachment = max(detachment), 
            Min_detachment = min(detachment), 
            Median_detachment = median(detachment), 
            Std_detachment = sd(detachment),
            
            Mean_raideur_moyenne = mean(raideur_moyenne), 
            Max_raideur_moyenne = max(raideur_moyenne), 
            Min_raideur_moyenne = min(raideur_moyenne), 
            Median_raideur_moyenne = median(raideur_moyenne), 
            Std_raideur_moyenne = sd(raideur_moyenne))
