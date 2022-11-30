rm(list = ls())

library("readxl")
library("config")
library("cli")

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "default")

# retrieve parameters
path_data = opt$concatenate_file
path_id = opt$concatenate_id_file
path_id_edit = opt$corrected_id_file
path_output = opt$data_curve
comment_accepted = unlist(strsplit(opt$comment_accepted, ","))

dir.create(path_output, showWarnings = FALSE)

file_id = read.table(path_id, sep = "\t", header = T)
data = read.table(path_data, sep = "\t", header = T)

file_id$Comment.on.this.sample = gsub(" +", "_", file_id$Comment.on.this.sample)

#remplacer les commentaires avec des erreurs
vect_comment = c("cuticle_broked", "cuticule_broke", "no_tape","no_scotch", "two_pupae_too_close")
vect_comment_corrige = c("cuticle_broke", "cuticle_broke", "no_adhesive_paper","no_adhesive_paper", "two_pupae")
names(vect_comment_corrige) = vect_comment

for (pb_comment in names(vect_comment_corrige)) {
  file_id$Comment.on.this.sample[which(file_id$Comment.on.this.sample == pb_comment)] = vect_comment_corrige[pb_comment]
}

# mofidication des id pour les rendre unique
list_id = file_id$Sample_ID
id_count = table(list_id)
handler_done = c()
for(id in list_id){
  if(id_count[id] > 1 & (! id %in% handler_done)){
    handler_done = c(handler_done, id)
    vers = 1
    for(i in which(list_id == id)){
      list_id[i] = paste(list_id[i], "_", vers, sep = "")
      vers = vers + 1
    }
  }
}

file_id$Sample_ID = list_id

dir.create(dirname(path_id_edit), showWarnings = FALSE)
file_id_edit = file_id[which(file_id$Comment.on.this.sample %in% comment_accepted) , ]
write.table(file_id_edit, path_id_edit, row.names = F, quote = F, sep = "\t")

pupe_id = 0
# distinguer chaque id de la table id (3 colonnes par 3 colonnes)
for (i in seq(1, ncol(data), by = 3)) {# seq permet de prendre une séquence de nombre : seq(nb initial, nb final, pas) ici on prend un pas de 2 pour aller de 2 en 2 sur les colonnes
  pupe_id = pupe_id + 1
  # filter on comment
  current_comment = file_id$Comment.on.this.sample[pupe_id]
  if(! current_comment %in% comment_accepted) next # si le commentaire n'est pas accepte on revient au debut de la boucle
  
  pupe_data = data[, c(i,(i+1), (i+2))]
  
  colnames(pupe_data) = c("time", "load", "extension")
  
  # recalibrage de la courbe
  temp = pupe_data$load[1:20]
  sigma_temp = sd(pupe_data$load[1:20])
  
  # ne pas recalibrer les donnees si le sigma du bruit est inferieur a 0.01
  if (sigma_temp <= 0.01) {
    new_start = which(abs(temp) >= (0.5 * sigma_temp))[1]
    new_index = new_start:length(pupe_data$load)
    pupe_data = data.frame("time" = pupe_data$time[new_index],
                           "load" = pupe_data$load[new_index] - median(pupe_data$load[1:20]),
                           "extension" = pupe_data$extension[new_index])
  } else {
    pupe_data = data.frame("time" = pupe_data$time,
                           "load" = pupe_data$load,
                           "extension" = pupe_data$extension)
  }
  
  # ecrire les donnees de la pupe
  file_path = paste0(path_output, "/", list_id[pupe_id], ".csv")
  write.table(pupe_data, file = file_path, quote = F, col.names = T, row.names = F, sep = "\t")
}




