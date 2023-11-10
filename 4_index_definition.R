rm(list = ls())

library("config")
library("readxl")

###
detect_index_3 = function(vect){
  nb_hit = 0
  for (i in 1:(length(vect) - 1)){
    if (is.na(vect[i]) | is.na(vect[i + 1])){
      nb_hit = 0
    } else if(temp[i] == vect[i + 1]){
      nb_hit = nb_hit + 1
    } else {
      nb_hit = 0
    }
    if (nb_hit == 5){
      return(i - 5)
    }
  }
  return(NA)
}
###


# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "portable")

# retrieve parameters
# Input
path_data = opt$batch_by_id
path_metadata_file = opt$concatenate_metadata

# Output
path_output = opt$index_path


#### MAIN

dir.create(dirname(path_output), showWarnings = FALSE)

metadata_file = read.table(path_metadata_file, sep = "\t", header = T)

# definition of the time limit for a noise peak
time_limit = 3

# ##
list_id_df = c()
list_index_1 = c()
list_index_2 = c()
list_index_2_graph = c()
list_index_3 = c()
list_index_4 = c()
list_index_5 = c()
list_index_6 = c()
list_med_noise_1 = c()
list_med_noise_4 = c()
list_med_noise_6 = c()
list_moy_noise_1 = c()
list_moy_noise_4 = c()
list_moy_noise_6 = c()
list_sd_noise_1 = c()
list_sd_noise_4 = c()
list_sd_noise_6 = c()
list_true_index_5 = c()

not_ok = c()
list_id = tools::file_path_sans_ext(list.files(path_data, pattern = ".*.csv$"))
for(id in list_id){
  run = tryCatch(
    expr = {
      # if (id %in% not_ok) next
      data = read.table(paste(path_data, "/",id, '.csv', sep = ""), sep = "\t", header = T)
      # plot(data$time, data$load, type = "l")
      # plot(data$time, data$extension, type = "l")
      
      # get experience time
      exp_time = max(data$time)#duree de l'exp
      freq_time = exp_time / length(data$time)#tps entre 2 pts
      delta_time = round(time_limit / freq_time)#nb de pts limite pour d?finir qu'on sort du pic du bruit
      
      # noise 1 temp
      noise_1_temp = data$load[1:20]
      sigma_1_temp = sd(data$load[1:20])
      
      # region 1
      
      index_3xamp = which(data$load >= (3 * sigma_1_temp))[1]#trouve la position à partir de laquelle on a 3x sigma du bruit
      
      noise_bool = data$load >= (3 * sigma_1_temp)#vecteur bruit transform? en bool?en si sup ? 3*bruit
      dat = which(noise_bool)#r?cup?rer index de qd ?a devient 3*bruit
      list_region = split(dat, cumsum(c(1, diff(dat) != 1)))#d?compose en r?gion o? on passe ? 3*bruit
      
      len_region = unlist(lapply(list_region, function(x) { # lapply ?quivaut ? bouble for
        # avec it?ration sur x dans list_region
        length(x)
      }
      ))
      if (sum(len_region >= delta_time) == 0){
        temp = rev(data$load[1:index_3xamp])#on prend toutes les valeurs entre la première valeur et celle ou l'amplitude est 3x amp et on le retourne
        index_1 = length(temp) - which(temp <= (sigma_1_temp))[1] + 1
        if (is.na(index_1)) index_1 = 1 # si on a calcule le bruit sur une region plus grande que la region d'amplitude, il y a un risque que la dispersion des donnees augmente apres la region d'amplitude et que sd soit atteint dans cette region
      } else if (which(len_region >= delta_time)[1] > 1){ #si avant la region 2 on a une autre zone ou on depasse 3*bruit
        index_1 = list_region[[which(len_region >= delta_time)[1]]][1]#alors l'index 1 est def au d?but de cette region o? on a + de 13 pts
      } else { #sinon on calcule de mani?re classique
        temp = rev(data$load[1:index_3xamp])#on prend toutes les valeurs entre la première valeur et celle ou l'amplitude est 3x amp et on le retourne
        index_1 = length(temp) - which(temp <= (sigma_1_temp))[1] + 1
        if (is.na(index_1)) index_1 = 1 # si on a calcule le bruit sur une region plus grande que la region d'amplitude, il y a un risque que la dispersion des donnees augmente apres la region d'amplitude et que sd soit atteint dans cette region
      }
      
      # noise 1
      
      noise_1 = median(data$load[1:index_1])
      moy_noise_1 = mean(data$load[1:index_1])
      sd_noise_1 = sd(data$load[1:index_1])
      
      # index 2
      index_2_graph = which(data$load == max(na.omit(data$load))) #index utilise pour graph seulement
      
      if (metadata_file$Protocol[metadata_file$Sample_ID == id] == "5 min") {
        index_2 = which(data$load == max(na.omit(data$load)))
      } else {
        index_2 = which(data$extension == max(na.omit(data$extension)))[1]#index utilise pour calculs
      }
      
      
      # index 3 
      temp = data$load[index_2:(length(data$load))]#on prend portion de courbe depuis force max jusqu'a la fin
      
      index_3 = NA
      nb_croissant = 0
      for (i in 1:(length(temp) - 1)){
        if ((! is.na(temp[i])) & (! is.na(temp[i + 1]))){
          if (temp[i] < temp [i+1]){
            nb_croissant = nb_croissant + 1
            if (nb_croissant == 3) {
              index_3 = index_2 + i - nb_croissant
              break
            }
          } else {
            nb_croissant = 0
          }
        } else {
          nb_croissant = 0
        }
      }
      
      ##
      # verifier si le temps entre index 2 et index 3 est inferieur a 1s, sinon redefinition de index 3 selon data$extension
      index_2_3_time_gap = 1
      if (is.na(index_3)){
        temp = data$extension[index_2:(length(data$load))]#on prend portion de courbe depuis force max jusqu'a la fin
        index_3 = detect_index_3(vect = temp) + index_2
      } else if (data$time[index_3] - data$time[index_2] > index_2_3_time_gap){
        temp = data$extension[index_2:(length(data$load))]#on prend portion de courbe depuis force max jusqu'a la fin
        index_3 = detect_index_3(vect = temp) + index_2
      }
      
      # verifier si index 3 existe puis si le temps entre index 2 et index 3 est inferieur a 1s, sinon redifinition de index 3 a 0.5s apr?s index 2
      if (is.na(index_3) | data$time[index_3] - data$time[index_2] > index_2_3_time_gap){
        temp = data$time[index_2:(length(data$load))]
        index_3 = which.min(abs(abs(temp - data$time[index_2]) - 0.5)) + index_2
      }
      
      # index 4
      if (metadata_file$Protocol[metadata_file$Sample_ID == id] == "5 min") {
        index_3_4_time_gap = 300
        zone_4_lim = data$time[index_3] + index_3_4_time_gap
        index_4 = which(data$time >= zone_4_lim)[1]
      } else if (metadata_file$Protocol[metadata_file$Sample_ID == id] == "0 s") {
        index_4 = index_3
      } else {
        index_3_4_time_gap = 10
        zone_4_lim = data$time[index_3] + index_3_4_time_gap
        index_4 = which(data$time >= zone_4_lim)[1]
      }
      
      
      ## noise 4
      
      noise_4_temp = data$load[index_4: (index_4 - 10)]
      min_noise_4_temp = min(noise_4_temp) - 0.03
      max_noise_4_temp = max(noise_4_temp) - 0.03
      
      amp_noise_4_temp = abs(min_noise_4_temp) + abs(max_noise_4_temp)
      
      noise_4 = median(data$load[index_3:index_4]) - 0.03
      moy_noise_4 = mean(data$load[index_3:index_4])
      sd_noise_4 = sd(data$load[index_3:index_4])
      
      ## noise 6 temp
      noise_6_temp = data$load[length(data$load): (length(data$load) - 50)]
      min_noise_6_temp = min(noise_6_temp)
      max_noise_6_temp = max(noise_6_temp)
      
      amp_noise_6_temp = abs(min_noise_6_temp) + abs(max_noise_6_temp)
      #we define noise on region 6
      
      ## index 5
      #
      min_detach = min(data$load)
      #

      #if the pic is big enough meaning 3* amplitude
      #we go through index 4 to the end
      #vect_bool = we test load is inferior or equal to amplitude which give a vector of true and false
      if(abs(min_detach) > (3 * amp_noise_6_temp)){
        temp = data$load[index_4:length(data$load)]
        vect_bool = temp <= - amp_noise_6_temp

        #detect first time there is a false so when it is superior to noise amplitude 
        #while stops when while is false
        #the goal is to detect when we reach the noise amplitude after index 4
        i = 1
        while(!vect_bool[i]){
          i = i + 1
        }

        #we detect when we reach for the second time the noise amplitude
        #it is defined as index 5
        while(vect_bool[i]){
          i = i + 1
        }
        index_5 = i - 1 + index_4
        true_index_5 = "sup"
      } else {
        index_5 = NA
      }
      
      ## cas o? l'index 5 n'est pas trouv?
      #s'il n'y a pas de pic ou pour autre raison on a pas pu définir avex la methode precedente
      #we take the median because we are sure to find a pic, there is 50 percent of the data on both side
      #we define index5 as soon as we enter into noise
      #this case is less precise, index5 should be a bit more on the left
      if (is.na(index_5)) {
        temp = data$load[index_4:length(data$load)]
        median_noise_6_temp = median(noise_6_temp)
        vect_bool = temp < median_noise_6_temp
        first_true = min(which(vect_bool)) #when we are inferior to the median
        #corresponds to the first while
        first_false = min(which(! vect_bool[first_true:length(vect_bool)])) + first_true - 1 #
        #corresponds to the second which
        index_5 = first_false + index_4 - 1
        true_index_5 = "inf"
      }
      
      # index 6
      index_6 = dim(data)[1]
      
      # noise 6
      noise_6 = median(data$load[index_6:index_5])
      moy_noise_6 = mean(data$load[index_6:index_5])
      sd_noise_6 = sd(data$load[index_6:index_5])
      
      #
      list_id_df = c(list_id_df, id)
      list_index_1 = c(list_index_1, index_1)
      list_index_2 = c(list_index_2, index_2)
      list_index_2_graph = c(list_index_2_graph, index_2_graph)
      list_index_3 = c(list_index_3, index_3)
      list_index_4 = c(list_index_4, index_4)
      list_index_5 = c(list_index_5, index_5)
      list_index_6 = c(list_index_6, index_6)
      list_med_noise_1 = c(list_med_noise_1, noise_1)
      list_med_noise_4 = c(list_med_noise_4, noise_4)
      list_med_noise_6 = c(list_med_noise_6, noise_6)
      list_moy_noise_1 = c(list_moy_noise_1, moy_noise_1)
      list_moy_noise_4 = c(list_moy_noise_4, moy_noise_4)
      list_moy_noise_6 = c(list_moy_noise_6, moy_noise_6)
      list_sd_noise_1 = c(list_sd_noise_1, sd_noise_1)
      list_sd_noise_4 = c(list_sd_noise_4, sd_noise_4)
      list_sd_noise_6 = c(list_sd_noise_6, sd_noise_6)
      list_true_index_5 = c(list_true_index_5, true_index_5)
    },
    error = function(e){ 
      # print(id)
      return(id)
    }
  )
  
  # retrieve the not running id
  if (length(run) == 1){
    if (run %in% list_id) {
      comment = metadata_file$Comment[metadata_file$Sample_ID == run]
      species = metadata_file$Species[metadata_file$Sample_ID == run]
      not_ok = c(not_ok, paste0(run, '\t', comment, '\t', species))
    }
  }
}

df = data.frame("id"= list_id_df,
                "index_1" = list_index_1,
                "index_2" = list_index_2,
                "index_2_graph" = list_index_2_graph,
                "index_3" = list_index_3,
                "index_4" = list_index_4,
                "index_5" = list_index_5,
                "index_6" = list_index_6,
                "med_noise_1" = list_med_noise_1,
                "med_noise_4" = list_med_noise_4,
                "med_noise_6" = list_med_noise_6,
                "moy_noise_1" = list_moy_noise_1, 
                "moy_noise_4" = list_moy_noise_4, 
                "moy_noise_6" = list_moy_noise_6, 
                "sd_noise_1" = list_sd_noise_1, 
                "sd_noise_4" = list_sd_noise_4, 
                "sd_noise_6" = list_sd_noise_6,
                "true_index_5" = list_true_index_5)

# Manual correction

tryCatch(expr = {df[which(df$id == "2022050406"), "index_1"] = 237})

tryCatch(expr = {df[which(df$id == "2022050240"), "index_1"] = 203})
tryCatch(expr = {df[which(df$id == "2022021129"), "index_4"] = 419})

tryCatch(expr = {
  df[which(df$id == "2022032412"), "index_2"] = 278
  df[which(df$id == "2022032412"), "index_3"] = 281
  df[which(df$id == "2022032412"), "index_4"] = 320
  df[which(df$id == "2022032412"), "index_5"] = 325
})

write.table(df, file = path_output, quote = F, col.names = T, row.names = F, sep = "\t")

write.table(not_ok, file = paste0(dirname(path_output), "/index_id_not_running.log"), row.names = F, col.names = F, quote = F)




