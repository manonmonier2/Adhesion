rm(list = ls())

library("config")
library("ggplot2")
# library(svglite)

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "default")

# retrieve parameters
# Input
path_metadata_file = opt$concatenate_metadata
path_index = opt$index_path
path_batch_by_id = opt$batch_by_id
comment_accepted = gsub(" +$", "", gsub("^ +", "", unlist(strsplit(opt$comment_accepted, ","))))

# Output
plot_path = opt$plot_path

path_plot_by_species = paste0(plot_path, "/by_species/")
dir.create(path_plot_by_species, showWarnings = FALSE)

# load data
index_table = read.table(path_index, header = T, sep = "\t")
metadata = read.table(path_metadata_file, sep = "\t", header = T)

list_id = tools::file_path_sans_ext(list.files(path_batch_by_id, pattern = ".*.csv$"))
species = unique(metadata$Species)
gg_data = data.frame()
for (id in list_id){
  
  species_name = metadata$Species[which(metadata$Sample_ID == id)]
  
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
  
  # skip non running id
  if (! id %in% index_table$id) next
  index_sample = index_table[which(index_table$id == id), ]
  
  index_to_plot = as.numeric(index_sample["index_1"]):as.numeric(index_sample["index_5"])

  valeur_index_2 = as.numeric(index_sample["index_2"])
  Tnew = sample$time - sample$time[valeur_index_2]
  Tnew = Tnew[index_to_plot]
  Enew = sample$extension - sample$extension[valeur_index_2]
  Enew = Enew[index_to_plot]
  
  temp_gg_data = data.frame("time" = Tnew,
                            "load" = sample$load[index_to_plot],
                            "extension" = Enew,
                            "species" = rep(species_name, length(index_to_plot)),
                            "id" = rep(id, length(index_to_plot)))
  
  gg_data = rbind(gg_data, temp_gg_data)
}

range_time = range(gg_data$time)
range_load = range(gg_data$load)
range_extension = range(gg_data$extension)

p_tl_global = ggplot(data = gg_data, aes(x = time, y = load, color = species, fill = id)) +
  geom_path() +
  theme_minimal() + 
  xlim(range_time) + 
  ylim(range_load) +
  theme(legend.position = "none") +
  facet_wrap(species ~ ., scales = "free")

ggsave(paste0(path_plot_by_species, "/time_load.pdf"), p_tl_global)

p_el_global = ggplot(data = gg_data, aes(x = extension, y = load, color = species, fill = id)) +
  geom_path() +
  theme_minimal() +
  xlim(range_extension) + 
  ylim(range_load) +
  theme(legend.position = "none") +
  facet_wrap(species ~ ., scales = "free")

ggsave(paste0(path_plot_by_species, "/extension_load.pdf"), p_el_global)

path_plot_species_extension_load = paste0(path_plot_by_species, "/extension_load/")
path_plot_species_time_load = paste0(path_plot_by_species, "/time_load/")

dir.create(path_plot_species_extension_load, showWarnings = FALSE)
dir.create(path_plot_species_time_load, showWarnings = FALSE)

# un plot par espece
for (focus_species in unique(gg_data$species)) {
  sub_gg_data = gg_data[gg_data$species == focus_species, ]
  p_tl = ggplot(data = sub_gg_data, aes(x = time, y = load, color = id, fill = id)) +
    geom_path() +
    theme_minimal() + labs(title = focus_species) +
    # xlim(range_time) + 
    # ylim(range_load) +
    theme(legend.position = "none")
  ggsave(paste0(path_plot_species_time_load, "/", focus_species, ".pdf"), p_tl)
  
  p_el = ggplot(data = sub_gg_data, aes(x = extension, y = load, color = id, fill = id)) +
    geom_path() +
    theme_minimal() + labs(title = focus_species) +
    # xlim(range_extension) + 
    # ylim(range_load) +
    theme(legend.position = "none")
  ggsave(paste0(path_plot_species_extension_load, "/", focus_species, ".pdf"), p_el)
}
