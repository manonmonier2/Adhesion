rm(list = ls())

library("config")
library("ggplot2")
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

sub_gg_data = subset(gg_data, gg_data$Comment == "ok")

# detachment
ggplot(sub_gg_data, aes(x = Species, y = detachment, fill = Species)) +
  geom_boxplot() +
  coord_flip()+
  theme_bw()

# raideur_moyenne
ggplot(sub_gg_data, aes(x = Species, y = raideur_moyenne, fill = Species)) +
  geom_boxplot() +
  coord_flip()+
  theme_bw()

# path_data = "/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster"
# 
# path_output = "/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/distance/"
# 
# ############# f(t) = F ###########################"
# tropicalis = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster/2021122112_graph.csv", header = T, sep = "", fileEncoding = "UTF-7")
# melanogaster = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster/2020072703_graph.csv", header = T, sep = "", fileEncoding = "UTF-7")
# virilis = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster/2021120109_graph.csv", header = T)
# 
# plot(virilis$time, virilis$load,type ="l", xlab = "time (s)", ylab = "load (N)", col = "red")
# lines(melanogaster$time, melanogaster$load)
# lines(tropicalis$time, tropicalis$load, col = "blue")
# 
# 
# ############### f(x) = F ############################
# tropicalis_index1_index5 = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster/2021122112_graph_index1_index5.csv", header = T, sep = ",", dec = ".", fileEncoding = "UTF-7")
# melanogaster_index1_index5 = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster/2020072703_graph_index1_index5.csv", header = T, sep = ",", fileEncoding = "UTF-7")
# virilis_index1_index5 = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster/2021120109_graph_index1_index5.csv", header = T,sep = ",", fileEncoding = "UTF-7")
# 
# plot(virilis_index1_index5$extension, virilis_index1_index5$load,type ="l", xlab = "extension (mm)", ylab = "load (N)", xlim=c(-0.5,1), col = "red")
# lines(melanogaster_index1_index5$extension, melanogaster_index1_index5$load)
# lines(tropicalis_index1_index5$extension, tropicalis_index1_index5$load, col = "blue")
# 
# 
# #################f(t) = x #################################
# melanogaster_start_index5 = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster/2020072703_graph_start_index5.csv", header = T, sep = ",", dec = ".", fileEncoding = "UTF-7")
# tropicalis_start_index5 = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster/2021122112_graph_start_index5.csv", header = T, sep = ",", dec = ".", fileEncoding = "UTF-7")
# virilis_start_index5 = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster/2021120109_graph_start_index5.csv", header = T, sep = ",", dec = ".", fileEncoding = "UTF-7")
# 
# plot(melanogaster_start_index5$time,melanogaster_start_index5$extension,type ="l", xlab = "time (s)", ylab = "extension (mm)")
# lines(tropicalis_start_index5$time, tropicalis_start_index5$extension, col = "blue")
# lines(virilis_start_index5$time, virilis_start_index5$extension, col = "red")
# 
# ################# f(integral)=force ##################
# int_distance_force_species = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/int_distance_force_species.csv", header = T, sep = ",", dec = ".", fileEncoding = "UTF-7")
# 
# sd_force_tropicalis = log(abs(sd(int_distance_force_species_tropicalis$force)))
# sd_force_melanogaster = sd(int_distance_force_species_melanogaster$force)
# sd_force_virilis = sd(int_distance_force_species_virilis$force)
# 
# sd_integral_tropicalis = sd(int_distance_force_species_tropicalis$int_somme)
# sd_integral_melanogaster = sd(int_distance_force_species_melanogaster$int_somme)
# sd_integral_virilis = sd(int_distance_force_species_virilis$int_somme)
# 
# 
# index = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/index.csv", header = T, sep = ",", dec = ".", fileEncoding = "UTF-7")
# # calcul des valeurs seuils pour réduire la courbe
# med_noise_6 <- as.numeric(index$sd_noise_6)
# mean_noise_6 = mean(med_noise_6, na.rm=TRUE) #na.rm : retire les NA dans le calcul
# seuil_force = log(abs(mean_noise_6))
# seuil_integrale = log(abs(mean_noise_6) * 0.2)
# 
# #calcul des ecart type sur force et integrale
# 
# #
# extract_mean_species = function(data, species){
#   temp_x = log(mean(int_distance_force_species[which(int_distance_force_species$Species == species), ]$int_somme))
#   temp_y = log(mean(abs(int_distance_force_species[which(int_distance_force_species$Species == species), ]$force)))
#   temp_x2 = sd(int_distance_force_species[which(int_distance_force_species$Species == species), ]$int_somme)
#   temp_y2 = sd(abs(int_distance_force_species[which(int_distance_force_species$Species == species), ]$force))
#   temp_x3 = temp_x + abs(temp_x2)
#   temp_y3 = temp_y + abs(temp_y2)
#   temp_x4 = temp_x - abs(temp_x2)
#   temp_y4 = temp_y - abs(temp_y2)
#   handler = c(temp_x, temp_y,temp_x2, temp_y2, temp_x3, temp_y3, temp_x4, temp_y4)
#   return(handler)
# }
# 
# list_species = c("Drosophila_melanogaster", "Drosophila_tropicalis", "Drosophila_virilis")
# coord_moy = sapply(list_species, function(x) extract_mean_species(extract_mean_species, x))
# rownames(coord_moy) = c("x", "y")
# 
# #opacite
# 
# int_distance_force_species_opacity = read.table("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/int_distance_force_species_opacity.csv", header = T, sep = ",", dec = ".", fileEncoding = "UTF-7")
# 
# list_id = int_distance_force_species_opacity$id
# for ( id in list_id){
#   if (int_distance_force_species_opacity$Species == "Drosophila_melanogaster" | int_distance_force_species_opacity$Species == "Drosophila_virilis" | int_distance_force_species_opacity$Species == "Drosophila_tropicalis")
#     int_distance_force_species_opacity$opacity[id] = 1
#   else
#     int_distance_force_species_opacity$opacity[id] = 0.5
#   
# }
# 
# 
# #
# p <-ggplot(int_distance_force_species, aes(x=log(int_somme), y=log(abs(force)), color=Species)) + geom_point(size=2) 
# p <- p + geom_point(x=coord_moy[1, "Drosophila_melanogaster"],y=coord_moy[2, "Drosophila_melanogaster"], colour="black", size = 5, shape = 15)
# p <- p + geom_point(x=coord_moy[1, "Drosophila_tropicalis"],y=coord_moy[2, "Drosophila_tropicalis"], colour="blue", size = 5, shape = 15)
# p <- p + geom_point(x=coord_moy[1, "Drosophila_virilis"],y=coord_moy[2, "Drosophila_virilis"], colour="red", size = 5, shape = 15)
# # p <- p + geom_point(x=coord_moy[1, "Drosophila_virilis"],y=coord_moy[2, "Drosophila_virilis"], colour="red", size = 5)
# # p <- p + geom_segment(x = coord_moy[1, "Drosophila_melanogaster"], y = coord_moy[3, "Drosophila_melanogaster"], xend = coord_moy[1, "Drosophila_melanogaster"], yend = coord_moy[4, "Drosophila_melanogaster"], colour = "black")
# # p <- p + geom_segment(x = coord_moy[4, "Drosophila_melanogaster"], y = coord_moy[2, "Drosophila_melanogaster"], xend = coord_moy[3, "Drosophila_melanogaster"], yend = coord_moy[2, "Drosophila_melanogaster"], colour = "black")
# #p <- p + geom_pointrange(data=mean_sd, mapping=aes(x=mean_sd[1, "Drosophila_melanogaster"], y=mean_sd[2, "Drosophila_melanogaster"], ymin=-4, ymax=-2), size=1, color="black", fill="black", shape=22)
# p <- p + scale_color_manual(values = c("#C0C0C0", "#C0C0C0", "#C0C0C0", "#C0C0C0",
#                                        "#C0C0C0", "#C0C0C0", "#C0C0C0", "#C0C0C0",
#                                        "#C0C0C0", "#C0C0C0", "#C0C0C0", "black",
#                                        "#C0C0C0", "#C0C0C0", "#C0C0C0", "#C0C0C0", 
#                                        "#C0C0C0", "#C0C0C0", "#C0C0C0", "#C0C0C0",
#                                        "blue", "red", "#C0C0C0", "#C0C0C0", 
#                                        "#C0C0C0", "#C0C0C0", "#C0C0C0", "#C0C0C0"))
# p <- p +  xlim(seuil_integrale, -1) + ylim(seuil_force, -1)
# p <- p + theme_classic()
# p
# 
# ################# arbre #########################
# path_data2 = "/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/data_curve/data_graph_poster/"
# data = read_excel(paste(path_data2, "Manon_Flora_results_file.xlsx", sep = ""))
# data_ok <- subset(data, data$Comment == "ok")
# data_not_detached <- subset(data, data$Comment == "not_detached")
# data_cuticle_broke <- subset(data, data$Comment == "cuticle_broke")
# 
# force <- as.numeric(as.character(data_ok$force_detachment_mN))
# force1 <- as.numeric(as.character(data_not_detached$force_detachment_mN))
# force2 <- as.numeric(as.character(data_cuticle_broke$force_detachment_mN))
# 
# 
# data_ok$Species <- factor(data_ok$Species, levels = c('Megaselia_scalaris','Scaptodrosophila_lebanonen', 'Drosophila_immigrans','Drosophila_funebris',
#                                                       'Zaprionus_lachaisei', 'Zaprionus_indianus', 'Drosophila_pachea',
#                                                       'Drosophila_nanoptera', 'Drosophila_virilis',
#                                                       'Drosophila_quadraria','Drosophila_mauritiana','Drosophila_simulans',
#                                                       'Drosophila_melanogaster', 'Drosophila_yakuba','Drosophila_eugracilis',
#                                                       'Drosophila_biarmipes', 'Drosophila_suzukii', 'Drosophila_prostipennis',
#                                                       'Drosophila_takahashii', 'Drosophila_rhopaloa',
#                                                       'Drosophila_kurseongensis', 'Drosophila_elegans',
#                                                       'Drosophila_malerkotliana','Drosophila_ananassae',
#                                                       'Drosophila_tropicalis',  ordered = TRUE))
# n_fun <- function(data_ok){
#   return(data.frame(y = 700, label = paste0("n = ",length(data_ok))))
# }
# n_fun_not_detached <- function(data_not_detached){
#   return(data.frame(y = 900, label = paste0("n = ",length(data_not_detached))))
# }
# n_fun_cuticle_broke <- function(data_cuticle_broke){
#   return(data.frame(y = 1000, label = paste0("n = ",length(data_cuticle_broke))))
# }
# p1 <- ggplot2::ggplot(data= data_ok, aes(x=Species, y= force))
# p1 <- p1 + geom_point(aes(),shape = 21, colour = "black", size = 1.5, stroke = 1)
# p1 <- p1 + geom_point(data=data_not_detached, aes(x=Species, y= force1),shape = 21, colour = "forest green", size = 1.5, stroke = 1 )
# p1 <- p1 + geom_point(data=data_cuticle_broke, aes(x=Species, y= force2),shape = 21, colour = "medium violet red", size = 1.5, stroke = 1 )
# 
# p1 <- p1 + geom_boxplot(aes(), width= 0.4, outlier.colour = "black", color = "black", alpha = 0) + xlab("Species") + ylab("Force_mN")
# p1 <- p1 + scale_y_continuous(breaks = c(0,200,400,600,800))
# p1 <- p1 + stat_summary(fun.data = n_fun, geom = "text", size=5) 
# p1 <- p1 + stat_summary(fun.data = n_fun_not_detached, geom = "text", size=5, color = "forest green") 
# p1 <- p1 + stat_summary(fun.data = n_fun_cuticle_broke, geom = "text", size=5, color = "medium violet red") 
# p1 <- p1 + theme_classic() + theme(plot.title = element_text(color="black", size=100, face = "bold"), axis.title = element_text(size = 20), 
#                                    axis.text = element_text(color="black", size=10), axis.text.x = element_text(size = 20))
# p1 <- p1 + coord_flip() 
# p1
# 
# 
# #arbre phylogénétique
# library(phylogram)
# 
# a <- read.dendrogram(text = "(megaselia,(scapto,((((immigrans, funebris),(lachaisei, indianus)),((pachea, nanoptera), 
#                       virilis)),(((quadraria,((((((mauritiana, simulans),melanogaster), yakuba),eugracilis),
#                       ((biarmipes, suzukii),(prostipennis, takahashii))),((rhopaloa, kurseongensis), 
#                       elegans))),(malerkotliana, ananassae)),tropicalis))));")
# plot(a, yaxt = "n")
# 
# ################ force and glue prints ##################
# glue_prints = read.table("/perso/monier/Documents/Adhesion_test_pictures/mesures_glue_prints.csv", header = T, sep = ",", fileEncoding = "UTF-7")
# 
# #rename the column Sample_ID by id
# adhesion <- rename(data, id = Sample_ID)
# 
# #merge fichier data adhesion et mesures empreintes
# adhesion_glue_prints <- merge(adhesion, glue_prints, by = "id")
# force3 <- as.numeric(as.character(adhesion_glue_prints$force_detachment_mN))
# 
# #plot force en fonction glue prints area
# #plot(adhesion_glue_prints$force, adhesion_glue_prints$glue_area_um,type ="o", xlab = "time (s)", ylab = "load (N)", col = "firebrick")
# 
# p2 <-ggplot(adhesion_glue_prints, aes(x=glue_area_mm, y=force3, color=Species)) + geom_point(size=2)
# # p1 <- p1 + scale_color_manual(values = c("#808080", "#808080", "#808080", "#808080",
# #                                          "#808080", "#808080", "#808080", "#808080",
# #                                          "#808080", "#808080", "#FF0000", "#808080",
# #                                          "#808080", "#808080", "#808080", "#808080", 
# #                                          "#808080", "#0000FF", "#808080", "#808080",
# #                                          "#00FF00", "#808080", "#808080", "#808080", 
# #                                          "#808080", "#808080"))
# 
# p2 <- p2 + scale_color_manual(values = c("#C0C0C0", "#C0C0C0", "black", "#C0C0C0",
#                                         "#C0C0C0","#C0C0C0", "blue", "red"))
# p2 <- p2 + theme_classic() +  ylab("Adhesion force (mN)") + xlab("Glue surface contact (mm²)")
# p2 <- p2 + theme(plot.title = element_text(color="black", size=100, face = "bold"), axis.title = element_text(size = 20), 
#                  axis.text = element_text(color="black", size=10), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))
# p2
#     
# 
# 
# 
#     