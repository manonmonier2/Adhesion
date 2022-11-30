rm(list = ls())

library("readxl")
library("config")

###fonction
suppression_derniere_valeur = function(vect) {
  if (length(vect)>2){
    vect = vect[1:(length(vect)-1)]
  }
  return(vect)
  
}


# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "default")

# retrieve parameters
path_data = opt$concatenate_file
path_id_edit = opt$corrected_id_file


path_output = opt$plot_path
path_data_curve = opt$data_curve
index_path = opt$index_path

df_index = read.table(paste0(index_path, "/index.csv"), header = T, sep = "\t")

path_integral = opt$integral_path
log_path = paste0(path_integral, "/log_file.txt")

dir.create(path_output, showWarnings = FALSE)
dir.create(path_integral, showWarnings = FALSE)

write(toString(Sys.time()), log_path, append=TRUE)

file_id = read.table(path_id_edit, sep = "\t", header = T)

vect_int_interpolation = c()
vect_int_zone_negative = c()
vect_int_somme = c()
vect_id = c()
vect_incertitude = c()
vect_diff_index5_index1 = c()
vect_aire_index5_moins1 = c()

list_id = file_id$Sample_ID
small_pic = 0
for (id in list_id) {
  # chargement des donn?es de la courbe
  sample = read.table(paste(path_data_curve, "/", id, '.csv', sep = ""), sep = "\t", header = T)
  
  #plot(sample$extension, sample$load, type = "l")
  #abline(v = sample$extension[df_index[which(df_index$id == id), "index_5"]])
  
  # definition des zone avance / recule
  ## avance
  reach_max = df_index[which(df_index$id == id), "index_2"]
  
  avance = data.frame("load" = sample$load[1:reach_max],
                      "extension" = sample$extension[1:reach_max])
  
  ## recule
  ### gamme des x de recule = de load max ? la fin de l'exp?rience
  range_x = c(min(na.omit(sample$extension)), max(na.omit(sample$extension[1:reach_max])))
  recule = data.frame("load" = sample$load[reach_max:dim(sample)[1]][which(sample$extension[reach_max:dim(sample)[1]] >= range_x[1])],
                      "extension" = sample$extension[reach_max:dim(sample)[1]][which(sample$extension[reach_max:dim(sample)[1]] >= range_x[1])])
  
  # plot avance / recule
  # par(mfrow = c(2, 1))
  # plot(avance$extension, avance$load, type = "l", main = id)
  # plot(recule$extension, recule$load, type = "l", main = id)
  # par(mfrow = c(1, 1))
  # 
  # index_2
  index_2 = df_index[which(df_index$id == id), "index_2"]
  
  # d?nition des zones sur avance
  ## zone 2
  borne_zone_2 = df_index[which(df_index$id == id), c("index_1", "index_2")]
  avance_zone_2 = avance[as.numeric(borne_zone_2["index_1"]):as.numeric(borne_zone_2["index_2"]), ]
  
  # definition des zones sur recule
  ## zone 3 4 et 5 (tant que extension > 0)
  
  borne_zone_3 = df_index[which(df_index$id == id), c("index_2", "index_3")]
  borne_zone_4 = df_index[which(df_index$id == id), c("index_3", "index_4")]
  borne_zone_5 = df_index[which(df_index$id == id), c("index_4", "index_5")]
  
  recule_zone_3 = recule[(as.numeric(borne_zone_3["index_2"]):as.numeric(borne_zone_3["index_3"])) - index_2 + 1, ]
  recule_zone_4 = recule[(as.numeric(borne_zone_4["index_3"]):as.numeric(borne_zone_4["index_4"])) - index_2 + 1, ]
  recule_zone_5 = recule[(as.numeric(borne_zone_5["index_4"]):as.numeric(borne_zone_5["index_5"])) - index_2 + 1, ]
  
  # si le pic de detachement est superieur a 0 (on passe au suivant)
  if (length(which(! recule_zone_5$load > 0)) == 0){
    msg = paste("Pic de detachement positif: ", id, sep = "")
    print(msg)
    write(toString(msg), log_path, append=TRUE)
    next
  } else {
    recule_zone_5_positive = recule_zone_5[1:(which(! recule_zone_5$load > 0)[1] - 1), ]
    recule_zone_5_negative = recule_zone_5[which(! recule_zone_5$load > 0)[1] : dim(recule_zone_5)[1], ]
  }
  
  # fusion des zones 3, 4 et 5 positive
  recule_merge_zone = rbind(recule_zone_3, recule_zone_4, recule_zone_5_positive)
  
  # suppression des valeurs dans recule o? load est sup?rieur au load de avance
  if (length(which(recule_merge_zone$load > max(na.omit(avance_zone_2$load)))) > 0 ){
    recule_merge_zone = recule_merge_zone[-which(recule_merge_zone$load > max(na.omit(avance_zone_2$load))), ]
  }
  
  #plot(recule_merge_zone$extension, recule_merge_zone$load, type = "l", main = id)
  #lines(avance_zone_2$extension, avance_zone_2$load)
  
  ## reduction zone 2 (faire correspondre les x entre avance et recule) 
  min_x = which(avance_zone_2$extension >= min(recule_merge_zone$extension))[1]
  zone_2_reduite = avance_zone_2[min_x:dim(avance_zone_2)[1], ]
  
  # plot(recule_merge_zone$extension, recule_merge_zone$load, type = "l", main = id)
  # lines(zone_2_reduite$extension, zone_2_reduite$load)
  # 
  # interpolation
  ## x des points  ? interpoler
  list_x_interpol = zone_2_reduite$extension
  
  res_interpol = approx(recule_merge_zone$extension, recule_merge_zone$load, xout = list_x_interpol)
  
  # nouvelles coordonn?es
  ## R?cup?ration des coordonn?es des points interpol?s
  recul_interpol =  data.frame("extension" = res_interpol$x,
                               "load" = res_interpol$y)
  
  
  ## Diff?rence entre les points recule (interpol?) et avance
  res_difference = data.frame("extension" = recul_interpol$extension,
                              "load" = recul_interpol$load - zone_2_reduite$load)
  
  # plot zone soustraite
  # par(mfrow = c(2, 1))
  # plot(recul_interpol$extension, recul_interpol$load, type = "l", main = id)
  # lines(zone_2_reduite$extension, zone_2_reduite$load)
  # plot(res_difference$extension, res_difference$load, type = "l", main = id)
  # par(mfrow = c(1, 1))
  # 
  # calcul d'int?gral recule > 0
  # remove non approximable point (on x axis)
  temp = res_difference$extension
  index_to_remove = c()
  while(is.unsorted(temp)){
    temp2 = diff(temp)
    # si il y a au moins un point a elenver
    if (length(which(temp2 <= 0)) > 0){
      # on recupere le premier index
      temp_index = which(temp2 <= 0)[1]
      # on l'ajoute dans la liste d'index a retirer
      index_to_remove = c(index_to_remove, temp_index + 1 + length(index_to_remove))
      temp = temp[ - (which(temp2 <= 0)[1] + 1)]
    } else {
      break
    }
  }
  # remove y = 0 point
  if (sum(na.omit(res_difference$load) == 0) > 0){
    index_to_remove = c(index_to_remove, which(res_difference$load == 0))
  }
  
  if (length(index_to_remove) > 0) {
    if (length(res_difference$extension[-index_to_remove]) < 2 ){
      next
    }
    int_interpolation = integrate(approxfun(res_difference$extension[-index_to_remove],res_difference$load[-index_to_remove], ties = "ordered"), 
                                  range(res_difference$extension[-index_to_remove])[1], range(res_difference$extension[-index_to_remove])[2])
  } else {
    int_interpolation = integrate(approxfun(res_difference$extension,res_difference$load, ties = "ordered"), 
                                  range(res_difference$extension)[1], range(res_difference$extension)[2])
  }
  
  
  # calcul d'int?gral sur recule < 0 jusqu'au bruit (index_5)
  int_zone_negative = integrate(approxfun(recule_zone_5_negative$extension, recule_zone_5_negative$load), 
                                range(recule_zone_5_negative$extension)[1], range(recule_zone_5_negative$extension)[2])
  
  #calcul de lintegrale laire negative moins celle entre index5 et le point juste avant (celle qui nous interesse)
  int_zone_negative_incertitude = integrate(approxfun(suppression_derniere_valeur(recule_zone_5_negative$extension), suppression_derniere_valeur(recule_zone_5_negative$load)), 
                                            range(suppression_derniere_valeur(recule_zone_5_negative$extension))[1], range(suppression_derniere_valeur(recule_zone_5_negative$extension))[2])
  #on soustrait 
  incertitude_integrale = abs(int_zone_negative$value - int_zone_negative_incertitude$value)
  
  if (length(recule_zone_5_negative$extension)>2) { #si on a un vecteur recule_zone_5_negative avec au moins 3 pts on fait le calcul
    aire_index5_moins1 = (abs(diff(tail(recule_zone_5_negative$extension, 2))) * tail(recule_zone_5_negative$load,2)[2])/2
    #avec tail on prend les deux dernières valeurs de recule_zone_5_negative donc index5 et la valeur juste avant
    #on fait la difference entre leurs valeurs dextension et on multiplie par la deuxieme valeur du vecteur avec les deux dernieres vaeurs
    
  } else { #si on a le vect recule_zone_5_negative avec moins de 3 pts, on decide que ca fait 0
    aire_index5_moins1 = 0
    small_pic = small_pic +1
    
  }
  
  extension_index5 = sample$extension[df_index[which(df_index$id == id), "index_5"]]
  extension_index1 = sample$extension[df_index[which(df_index$id == id), "index_1"]]
  diff_index5_index1 = extension_index5 - extension_index1
  
  # somme des int?grales
  int_somme = abs(int_interpolation$value) + abs(int_zone_negative$value)
  
  vect_int_interpolation = c(vect_int_interpolation, int_interpolation$value)#il faut mettre value car cest le resultat dune fonction r et il y a plein dautre valeur
  vect_int_zone_negative = c(vect_int_zone_negative, int_zone_negative$value)
  vect_int_somme = c(vect_int_somme, int_somme)
  vect_id = c(vect_id, id)
  vect_incertitude = c(vect_incertitude, incertitude_integrale)
  vect_diff_index5_index1 = c(vect_diff_index5_index1, diff_index5_index1)
  vect_aire_index5_moins1 = c(vect_aire_index5_moins1, aire_index5_moins1)
}

tab_int = data.frame("id" = vect_id,
                     "integrale_zone_positive" = abs(vect_int_interpolation),
                     "integrale_zone_negative" = abs(vect_int_zone_negative),
                     "integrale_somme" = abs(vect_int_somme),
                     "incertitude" = abs(vect_incertitude), 
                     "diff_extension_index5_index1" = vect_diff_index5_index1, 
                     "aire_index5_moins1" = vect_aire_index5_moins1)

file_path = paste(path_integral, "/integral.csv", sep = "")
write.table(tab_int, file = file_path, quote = F, col.names = T, row.names = F, sep = "\t")

