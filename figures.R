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
  summarise(Mean_detachment = mean(detachment_force), 
            Max_detachment = max(detachment_force), 
            Min_detachment = min(detachment_force), 
            Median_detachment = median(detachment_force), 
            Std_detachment = sd(detachment_force),
            
            Mean_energy = mean(energy), 
            Max_energy = max(energy), 
            Min_energy = min(energy), 
            Median_energy = median(energy), 
            Std_energy = sd(energy),
            
            Mean_rigidity = mean(rigidity), 
            Max_rigidity = max(rigidity), 
            Min_rigidity = min(rigidity), 
            Median_rigidity = median(rigidity), 
            Std_rigidity = sd(rigidity),
            
            Mean_position_difference = mean(position_difference), 
            Max_position_difference = max(position_difference), 
            Min_position_difference = min(position_difference), 
            Median_position_difference = median(position_difference), 
            Std_position_difference = sd(position_difference),
            
            Mean_detachment_position = mean(detachment_position), 
            Max_detachment_position = max(detachment_position), 
            Min_detachment_position = min(detachment_position), 
            Median_detachment_position = median(detachment_position), 
            Std_detachment_position = sd(detachment_position)
            )

attach(gg_stat)#force R a considerer 'detachment position' etc comme des colonnes

# one parameter plot
## by species
plot_path_one_parameter_by_species = paste0(plot_path, "/one_parameter/by_species/")
dir.create(plot_path_one_parameter_by_species, showWarnings = FALSE, recursive = T)

### detachment_force
p = ggplot(gg_data %>% filter(Comment == "ok"),
       aes(x = Species, y = detachment_force, fill = Species)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  ylab("Detachment force") +
  xlab("Species")

ggsave(file = paste0(plot_path_one_parameter_by_species, "/detachment_force", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### energy
p = ggplot(gg_data %>% filter(Comment == "ok"),
           aes(x = Species, y = energy, fill = Species)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  ylab("Energy") +
  xlab("Species")

ggsave(file = paste0(plot_path_one_parameter_by_species, "/energy", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### rigidity
p = ggplot(gg_data %>% filter(Comment == "ok"),
           aes(x = Species, y = rigidity, fill = Species)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  ylab("Rigidity") +
  xlab("Species")

ggsave(file = paste0(plot_path_one_parameter_by_species, "/rigidity", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### position_difference
p = ggplot(gg_data %>% filter(Comment == "ok"),
           aes(x = Species, y = position_difference, fill = Species)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  ylab("Position difference") +
  xlab("Species")

ggsave(file = paste0(plot_path_one_parameter_by_species, "/position_difference", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")

### detachment_position
p = ggplot(gg_data %>% filter(Comment == "ok"),
           aes(x = Species, y = detachment_position, fill = Species)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
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
  theme_bw() +
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
  theme_bw() +
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
  theme_bw() +
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
  geom_errorbar(xmin = Min_detachment, xmax = Max_detachment) +
  geom_errorbar(ymin = Min_energy, ymax = Max_energy) +
  xlim(min(Min_detachment), max(Max_detachment)) +
  ylim(min(Min_energy), max(Max_energy)) +
  geom_point(gg_data %>% filter(Comment == "ok"), 
             mapping = aes(x = detachment_force, y = energy), alpha = 0.3) +
  xlab("Detachment force") +
  ylab("Energy") +
  theme_bw() 

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
  theme_bw()

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
  theme_bw()

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
  theme_bw()

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
  theme_bw()

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
  theme_bw()

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
  theme_bw()

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
  theme_bw()

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
  theme_bw()

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
  theme_bw()

ggsave(file = paste0(plot_path_two_parameters_by_species, "/x_position_difference_y_detachment_position", ".pdf"), 
       plot=p, width=16, height=8, device = "pdf")


