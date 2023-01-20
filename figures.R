rm(list = ls())

library("config")
library("ggplot2")
library("dplyr")

#### FUNCTIONS ####

n_fun <- function(data){
  y_pos = max(data) + (max(data) - min(data)) * 0.1
  return(data.frame(y = y_pos, label = paste0("n = ",length(data))))
}

####

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "portable")

# retrieve parameters
# Input
path_metadata_file = opt$concatenate_metadata
path_index = opt$index_path
path_batch_by_id = opt$batch_by_id
plot_path = opt$plot_path
path_integral = opt$integral_path

index_table = read.table(path_index, header = T, sep = "\t")
metadata = read.table(path_metadata_file, sep = "\t", header = T)
energy_table = read.table(paste0(path_integral, "/integral.csv"), header = T, sep = "\t")

list_id = index_table$id
gg_data = data.frame()
for (id in list_id){
  gg_data = rbind(gg_data, metadata[which(metadata$Sample_ID == id), ])
}

# parameters calculation
detachment_force = c()
rigidity = c()
energy = c()
position_difference = c()
detachment_position = c()
for (id in gg_data$Sample_ID){
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
  current_metadata = metadata[metadata$Sample_ID == id, ]
  current_index = index_table[index_table$id == id, ]
  
  current_detachment_force = min(sample$load[current_index$index_4:current_index$index_5])
  if(length(which(energy_table$id == id)) == 1) {
    current_energy = energy_table$integrale_zone_negative[energy_table$id == id]
  } else {
    current_energy = NA
  }
    
  current_rigidity = (sample$load[current_index$index_2] - sample$load[current_index$index_1]) / (sample$extension[current_index$index_2] - sample$extension[current_index$index_1])
  current_position_difference = abs(sample$extension[current_index$index_1] - sample$extension[current_index$index_5])
  current_detachment_position = sample$extension[current_index$index_5]
  
  detachment_force = c(detachment_force, current_detachment_force)
  energy = c(energy, current_energy)
  rigidity = c(rigidity, current_rigidity)
  position_difference = c(position_difference, current_position_difference)
  detachment_position = c(detachment_position, current_detachment_position)
}

gg_data = cbind(gg_data, 
                detachment_force,
                energy,
                rigidity,
                position_difference,
                detachment_position)

gg_stat = gg_data %>%
  filter(Comment == "ok") %>%
  group_by(Species)%>% 
  summarise(Mean_detachment = mean(detachment_force, na.rm = T), 
            Max_detachment = max(detachment_force, na.rm = T), 
            Min_detachment = min(detachment_force, na.rm = T), 
            Median_detachment = median(detachment_force, na.rm = T), 
            Std_detachment = sd(detachment_force, na.rm = T),
            
            Mean_energy = mean(energy, na.rm = T), 
            Max_energy = max(energy, na.rm = T), 
            Min_energy = min(energy, na.rm = T), 
            Median_energy = median(energy, na.rm = T), 
            Std_energy = sd(energy, na.rm = T),
            
            Mean_rigidity = mean(rigidity, na.rm = T), 
            Max_rigidity = max(rigidity, na.rm = T), 
            Min_rigidity = min(rigidity, na.rm = T), 
            Median_rigidity = median(rigidity, na.rm = T), 
            Std_rigidity = sd(rigidity, na.rm = T),
            
            Mean_position_difference = mean(position_difference, na.rm = T), 
            Max_position_difference = max(position_difference, na.rm = T), 
            Min_position_difference = min(position_difference, na.rm = T), 
            Median_position_difference = median(position_difference, na.rm = T), 
            Std_position_difference = sd(position_difference, na.rm = T),
            
            Mean_detachment_position = mean(detachment_position, na.rm = T), 
            Max_detachment_position = max(detachment_position, na.rm = T), 
            Min_detachment_position = min(detachment_position, na.rm = T), 
            Median_detachment_position = median(detachment_position, na.rm = T), 
            Std_detachment_position = sd(detachment_position, na.rm = T)
            )

attach(gg_stat)#force R a considerer 'detachment position' etc comme des colonnes

gg_stat_protocol_drosophila_melanogaster = gg_data %>%
  filter(Comment == "ok" & Species == "Drosophila_melanogaster") %>%
  group_by(Protocol)%>% 
  summarise(Mean_detachment = mean(detachment_force, na.rm = T), 
            Max_detachment = max(detachment_force, na.rm = T), 
            Min_detachment = min(detachment_force, na.rm = T), 
            Median_detachment = median(detachment_force, na.rm = T), 
            Std_detachment = sd(detachment_force, na.rm = T),
            
            Mean_energy = mean(energy, na.rm = T), 
            Max_energy = max(energy, na.rm = T), 
            Min_energy = min(energy, na.rm = T), 
            Median_energy = median(energy, na.rm = T), 
            Std_energy = sd(energy, na.rm = T),
            
            Mean_rigidity = mean(rigidity, na.rm = T), 
            Max_rigidity = max(rigidity, na.rm = T), 
            Min_rigidity = min(rigidity, na.rm = T), 
            Median_rigidity = median(rigidity, na.rm = T), 
            Std_rigidity = sd(rigidity, na.rm = T),
            
            Mean_position_difference = mean(position_difference, na.rm = T), 
            Max_position_difference = max(position_difference, na.rm = T), 
            Min_position_difference = min(position_difference, na.rm = T), 
            Median_position_difference = median(position_difference, na.rm = T), 
            Std_position_difference = sd(position_difference, na.rm = T),
            
            Mean_detachment_position = mean(detachment_position, na.rm = T), 
            Max_detachment_position = max(detachment_position, na.rm = T), 
            Min_detachment_position = min(detachment_position, na.rm = T), 
            Median_detachment_position = median(detachment_position, na.rm = T), 
            Std_detachment_position = sd(detachment_position, na.rm = T)
  )
attach(gg_stat_protocol_drosophila_melanogaster)


# one parameter plot
## by species
plot_path_one_parameter_by_species = paste0(plot_path, "/one_parameter/by_species/")
dir.create(plot_path_one_parameter_by_species, showWarnings = FALSE, recursive = T)

### detachment_force
p = ggplot(gg_data %>% filter(Comment == "ok"),
       aes(x = Species, y = detachment_force, fill = Species)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw(base_size = 22) +
  ylab("Detachment force") +
  xlab("Species")

ggsave(file = paste0(plot_path_one_parameter_by_species, "/detachment_force", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### energy
p = ggplot(gg_data %>% filter(Comment == "ok"),
           aes(x = Species, y = energy, fill = Species)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw(base_size = 22) +
  ylab("Energy") +
  xlab("Species")

ggsave(file = paste0(plot_path_one_parameter_by_species, "/energy", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### rigidity
p = ggplot(gg_data %>% filter(Comment == "ok"),
           aes(x = Species, y = rigidity, fill = Species)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw(base_size = 22) +
  ylab("Rigidity") +
  xlab("Species")

ggsave(file = paste0(plot_path_one_parameter_by_species, "/rigidity", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### position_difference
p = ggplot(gg_data %>% filter(Comment == "ok"),
           aes(x = Species, y = position_difference, fill = Species)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw(base_size = 22) +
  ylab("Position difference") +
  xlab("Species")

ggsave(file = paste0(plot_path_one_parameter_by_species, "/position_difference", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### detachment_position
p = ggplot(gg_data %>% filter(Comment == "ok"),
           aes(x = Species, y = detachment_position, fill = Species)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw(base_size = 22) +
  ylab("Detachment position") +
  xlab("Species")

ggsave(file = paste0(plot_path_one_parameter_by_species, "/detachment_position", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")


## by protocol
plot_path_one_parameter_by_protocol = paste0(plot_path, "/one_parameter/by_protocol/")
dir.create(plot_path_one_parameter_by_protocol, showWarnings = FALSE, recursive = T)

### detachment_force
p = ggplot(gg_data %>% filter(Comment == "ok"),
           aes(x = Protocol, y = detachment_force, fill = Protocol)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw(base_size = 22) +
  ylab("Detachment force") +
  xlab("Protocol")

ggsave(file = paste0(plot_path_one_parameter_by_protocol, "/detachment_force", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

## by protocol and species
plot_path_one_parameter_by_protocol_and_species = paste0(plot_path, "/one_parameter/by_protocol_and_species/")
dir.create(plot_path_one_parameter_by_protocol_and_species, showWarnings = FALSE, recursive = T)

### detachment force
p = ggplot(gg_data %>% filter(Comment == "ok"),
       aes(x = Protocol, y = detachment_force, fill = Protocol)) +
  geom_boxplot() +
  theme_bw(base_size = 22) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ylab("Detachment force") +
  xlab("Protocol") +
  ylim(min(detachment_force), max(detachment_force)) +
  ggtitle("Detachment force by protocol and species") +
  facet_wrap(Species ~ ., scales = "free")

ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/detachment_force", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### detachment force Drosophila_melanogaster
p = ggplot(gg_data %>% filter(Comment == "ok" & Species == "Drosophila_melanogaster"),
           aes(x = Protocol, y = detachment_force, fill = Protocol)) +
  geom_boxplot() +
  theme_bw(base_size = 22) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Detachment force") +
  xlab("Protocol") +
  stat_summary(fun.data = n_fun, geom = "text") +
  ggtitle("Detachment force by protocol for Drosophila melanogaster") +
  facet_wrap(Species ~ ., scales = "free")

ggsave(file = paste0(plot_path_one_parameter_by_protocol_and_species, "/detachment_force_Drosophila_melanogaster", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

# two parameters plot
## by species
plot_path_two_parameters_by_species = paste0(plot_path, "/two_parameters/by_species/")
dir.create(plot_path_two_parameters_by_species, showWarnings = FALSE, recursive = T)

### x: detachment force y: energy
p = ggplot(gg_stat, 
           aes(x = Median_detachment, y = Median_energy, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Median_detachment - Std_detachment, xmax = Median_detachment + Std_detachment) +
  geom_errorbar(ymin = Median_energy - Std_energy, ymax = Median_energy + Std_energy) +
  xlim(min(Min_detachment), max(Max_detachment)) +
  ylim(min(Min_energy), max(Max_energy)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = detachment_force, y = energy), alpha = 0.3) +
  xlab("Detachment force") +
  ylab("Energy") +
  theme_bw(base_size = 22) 

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_detachment_force_y_energy", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### x: detachment force y: rigidity
p = ggplot(gg_stat, 
            aes(x = Median_detachment, y = Median_rigidity, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Min_detachment, xmax = Max_detachment) +
  geom_errorbar(ymin = Min_rigidity, ymax = Max_rigidity) +
  xlim(min(Min_detachment), max(Max_detachment)) +
  ylim(min(Min_rigidity), max(Max_rigidity)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = detachment_force, y = rigidity), alpha = 0.3) +
  xlab("Detachment force") +
  ylab("Rigidity") +
  theme_bw(base_size = 22)

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_detachment_force_y_rigidity", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### x: detachment force y: position difference
p = ggplot(gg_stat, 
           aes(x = Median_detachment, y = Median_position_difference, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Min_detachment, xmax = Max_detachment) +
  geom_errorbar(ymin = Min_position_difference, ymax = Max_position_difference) +
  xlim(min(Min_detachment), max(Max_detachment)) +
  ylim(min(Min_position_difference), max(Max_position_difference)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = detachment_force, y = position_difference), alpha = 0.3) +
  xlab("Detachment force") +
  ylab("Position difference") +
  theme_bw(base_size = 22)

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_detachment_force_y_position_difference", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")


### x: detachment force y: detachment position
p = ggplot(gg_stat, 
           aes(x = Median_detachment, y = Median_detachment_position, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Min_detachment, xmax = Max_detachment) +
  geom_errorbar(ymin = Min_detachment_position, ymax = Max_detachment_position) +
  xlim(min(Min_detachment), max(Max_detachment)) +
  ylim(min(Min_detachment_position), max(Max_detachment_position)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = detachment_force, y = detachment_position), alpha = 0.3) +
  xlab("Detachment force") +
  ylab("Detachment position") +
  theme_bw(base_size = 22)

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_detachment_force_y_detachment_position", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### x: energy y: rigidity
p = ggplot(gg_stat, 
           aes(x = Median_energy, y = Median_rigidity, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Min_energy, xmax = Max_energy) +
  geom_errorbar(ymin = Min_rigidity, ymax = Max_rigidity) +
  xlim(min(Min_energy), max(Max_energy)) +
  ylim(min(Min_rigidity), max(Max_rigidity)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = energy, y = rigidity), alpha = 0.3) +
  xlab("Energy") +
  ylab("Rigidity") +
  theme_bw(base_size = 22)

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_energy_y_rigidity", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### x: energy y: position difference
p = ggplot(gg_stat, 
           aes(x = Median_energy, y = Median_position_difference, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Min_energy, xmax = Max_energy) +
  geom_errorbar(ymin = Min_position_difference, ymax = Max_position_difference) +
  xlim(min(Min_energy), max(Max_energy)) +
  ylim(min(Min_position_difference), max(Max_position_difference)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = energy, y = position_difference), alpha = 0.3) +
  xlab("Energy") +
  ylab("Position difference") +
  theme_bw(base_size = 22)

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_energy_y_position_difference", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### x: energy y: detachment position
p = ggplot(gg_stat, 
           aes(x = Median_energy, y = Median_detachment_position, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Min_energy, xmax = Max_energy) +
  geom_errorbar(ymin = Min_detachment_position, ymax = Max_detachment_position) +
  xlim(min(Min_energy), max(Max_energy)) +
  ylim(min(Min_detachment_position), max(Max_detachment_position)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = energy, y = detachment_position), alpha = 0.3) +
  xlab("Energy") +
  ylab("Detachment position") +
  theme_bw(base_size = 22)

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_energy_y_detachment_position", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### x: rigidity y: position difference
p = ggplot(gg_stat, 
           aes(x = Median_rigidity, y = Median_position_difference, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Min_rigidity, xmax = Max_rigidity) +
  geom_errorbar(ymin = Min_position_difference, ymax = Max_position_difference) +
  xlim(min(Min_rigidity), max(Max_rigidity)) +
  ylim(min(Min_position_difference), max(Max_position_difference)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = rigidity, y = position_difference), alpha = 0.3) +
  xlab("Rigidity") +
  ylab("Position difference") +
  theme_bw(base_size = 22)

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_rigidity_y_position_difference", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### x: rigidity y: detachment position
p = ggplot(gg_stat, 
           aes(x = Median_rigidity, y = Median_detachment_position, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Min_rigidity, xmax = Max_rigidity) +
  geom_errorbar(ymin = Min_detachment_position, ymax = Max_detachment_position) +
  xlim(min(Min_rigidity), max(Max_rigidity)) +
  ylim(min(Min_detachment_position), max(Max_detachment_position)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = rigidity, y = detachment_position), alpha = 0.3) +
  xlab("Rigidity") +
  ylab("Detachment position") +
  theme_bw(base_size = 22)

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_rigidity_y_detachment_position", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### x: position difference y: detachment position
p = ggplot(gg_stat, 
           aes(x = Median_position_difference, y = Median_detachment_position, color = Species)) +
  geom_point(size = 5, shape = 3) +
  geom_errorbar(xmin = Min_position_difference, xmax = Max_position_difference) +
  geom_errorbar(ymin = Min_detachment_position, ymax = Max_detachment_position) +
  xlim(min(Min_position_difference), max(Max_position_difference)) +
  ylim(min(Min_detachment_position), max(Max_detachment_position)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = position_difference, y = detachment_position), alpha = 0.3) +
  xlab("Position difference") +
  ylab("Detachment position") +
  theme_bw(base_size = 22)

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_position_difference_y_detachment_position", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

## x: detachment force y: energy by protocol for Drosophila melanogaster
plot_path_two_parameters_by_protocol_for_drosophila_melanogaster = paste0(plot_path, "/two_parameters/by_protocol_and_species/")
dir.create(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, showWarnings = FALSE, recursive = T)

gg_stat_protocol_drosophila_melanogaster$Median_energy = abs(gg_stat_protocol_drosophila_melanogaster$Median_energy)
gg_stat_protocol_drosophila_melanogaster$Std_energy = abs(gg_stat_protocol_drosophila_melanogaster$Std_energy)

gg_stat_protocol_drosophila_melanogaster$Median_detachment = abs(gg_stat_protocol_drosophila_melanogaster$Median_detachment)
gg_stat_protocol_drosophila_melanogaster$Std_detachment = abs(gg_stat_protocol_drosophila_melanogaster$Std_detachment)

gg_data$detachment_force = abs(gg_data$detachment_force)

p = ggplot(gg_stat_protocol_drosophila_melanogaster, 
           aes(x = Median_detachment, y = Median_energy, color = Protocol)) +
  geom_point(size = 1) +
  geom_errorbar(xmin = abs(Median_detachment - Std_detachment), xmax = abs(Median_detachment + Std_detachment)) +
  geom_errorbar(ymin = abs(Median_energy - Std_energy), ymax = abs(Median_energy + Std_energy)) +
  geom_point(gg_data %>% filter(Comment == "ok" & Species == "Drosophila_melanogaster"), 
             mapping = aes(x = detachment_force, y = energy), alpha = 0.3) +
  # scale_x_continuous(trans='log10') +
  # scale_y_continuous(trans='log10') +
  coord_trans(x = "log10", y = "log10") +
  xlab("Detachment force") +
  ylab("Energy") +
  theme_bw(base_size = 22) +
  ggtitle("x: Detachment force, y: Energy by protocol for Drosophila melanogaster")

ggsave(file = paste0(plot_path_two_parameters_by_protocol_for_drosophila_melanogaster, "/x_detachment_force_y_energy_Drosophila_melanogaster", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

# table(metadata$Protocol[metadata$Species == "Drosophila_melanogaster"])
# 
# temp = metadata %>% filter(Species == "Drosophila_melanogaster" & Comment == "ok") 
# table(temp$Protocol)
# 
# 
# temp = gg_data %>% filter(Comment == "ok") %>% group_by(Species) %>% 
#   summarise(table(Protocol))
# 
# 
# list_protocol = unique(gg_data$Protocol)
# list_species = unique(gg_data$Species)
# df_res = data.frame()
# for(species in sort(list_species)){
#   handler = gg_data %>% filter(Comment == "ok" & Species == species)
#   new_row = c()
#   for(protocol in list_protocol){
#     new_row = c(new_row, sum(handler$Protocol == protocol))
#   }
#   df_res = rbind(df_res, new_row)
# }
# 
# colnames(df_res) = list_protocol
# rownames(df_res) = list_species
# 
# 
# list_protocol = unique(metadata$Protocol)
# list_species = unique(metadata$Species)
# df_res_metadata = data.frame()
# for(species in sort(list_species)){
#   handler = metadata %>% filter(Comment == "ok" & Species == species)
#   new_row = c()
#   for(protocol in list_protocol){
#     new_row = c(new_row, sum(handler$Protocol == protocol))
#   }
#   df_res_metadata = rbind(df_res_metadata, new_row)
# }
# 
# colnames(df_res_metadata) = list_protocol
# rownames(df_res_metadata) = list_species
# 
# write.table(df_res, paste0(plot_path, "/table_nb_pupe_apres_index_comment_ok.csv"), sep = "\t")
# write.table(df_res_metadata, paste0(plot_path, "/table_nb_pupe_avant_index_comment_ok.csv"), sep = "\t")