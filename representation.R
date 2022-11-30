rm(list = ls())

library("ggplot2")
library("svglite")
library("readxl")
library("config")

### functions ###

gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

###

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "default")

# retrieve parameters
path_data = opt$concatenate_file
path_id = opt$concatenate_id_file
path_output = opt$data_curve


path_local = "/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_03_03_22_all_data/Manon/data/"

df = read.table(paste0(path_local, "/data_curve/index/index.csv"), header = T, sep = "\t")

results = read_excel(paste0(path_local, "/Manon_results_file.xlsx"), sheet = 1)

#list_id = read_excel("/perso/monier/Documents/Adhesion_test_data/Integrales/Jean_Noel_26_11_21/Manon/data/test2.xlsx", sheet = 2)

sheet2 = read_excel(paste0(path_local, "/control3.xlsx"), sheet = 2)

if (dim(sheet2)[2] == 1){
  list_id = sheet2
  check_file = FALSE
} else if (dim(sheet2)[2] == 2){
  #
  #remplacer des commentaires faux
  #sheet2[307, 1] = "pb_machine"
  #sheet2[308, 1] = "pb_machine"
  # mofidication des id pour les rendre unique
  list_id = sheet2$Sample_ID
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
  #
  check_file = TRUE
} else {
  print("Format error")
}

path_output = paste0(path_local, "/plot_zone_load_time/")

# plot f(time) = load
dir.create(path_output, showWarnings = FALSE)

for (id in list_id){
  if (check_file){
    # check in comment is "ok"
    comment = tail(sheet2$`Comment on this sample`[list_id == id], 1)
    if (comment != 'ok' & comment != "0s") {
      next
    }
  }
  if (id != "2020082806") next
  sample = read.table(paste0(path_local, "/data_curve/", id, '.csv'), sep = "\t", header = T)
  index_sample = which(df$id == id)
  if (sum(is.na(df[index_sample, c("index_1", "index_2_graph", "index_3", "index_4", "index_5", "index_6")])) == 0){
    gg_index = c(1:df[index_sample, "index_1"], 
                 df[index_sample, "index_1"]:df[index_sample, "index_2_graph"], 
                 df[index_sample, "index_2_graph"]:df[index_sample, "index_3"],
                 df[index_sample, "index_3"]:df[index_sample, "index_4"],
                 df[index_sample, "index_4"]:df[index_sample, "index_5"],
                 df[index_sample, "index_5"]:dim(sample)[1])
    
    gg_region = as.factor(c(rep(1, length(1:df[index_sample, "index_1"])),
                            rep(2, length((df[index_sample, "index_1"]):df[index_sample, "index_2_graph"])),
                            rep(3, length((df[index_sample, "index_2_graph"]):df[index_sample, "index_3"])),
                            rep(4, length((df[index_sample, "index_3"]):df[index_sample, "index_4"])),
                            rep(5, length((df[index_sample, "index_4"]):df[index_sample, "index_5"])),
                            rep(6, length((df[index_sample, "index_5"]):dim(sample)[1]))))
    
    gg_data = data.frame("time" = sample$time[gg_index],
                         "load" = sample$load[gg_index],
                         "region" = gg_region)
    
    new_index_pos = gg_data$time[unlist(df[index_sample, c("index_1", "index_2_graph", "index_3", "index_4", "index_5", "index_6")]) + c(0, 1, 2, 3, 4, 5)]
    
    p = ggplot(data = gg_data, aes(x = time, y = load, color = region)) +
      geom_line() +
      theme_minimal() +
      geom_vline(xintercept = new_index_pos, linetype = "dashed", col = gg_color_hue(6)) +
      labs(x = 'Time', y = 'Load', title = id) +
      annotate("text", x = gg_data$time[df[index_sample, "index_1"]], y=0.07, label= df[index_sample, "med_noise_1"]) +
      annotate("text", x = gg_data$time[df[index_sample, "index_4"]], y=0.07, label= df[index_sample, "med_noise_4"]) +
      annotate("text", x = gg_data$time[df[index_sample, "index_6"]], y=0.06, label= df[index_sample, "med_noise_6"]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    pdf(file = paste(path_output, id, ".pdf", sep = ""), width = 12, height = 8)
    print(p)
    dev.off()
  }
}


path_output2 = paste0(path_local, "/plot_zone_load_extension/")


# plot f(extension) = load
dir.create(path_output2, showWarnings = FALSE)

# which(list_id == id)
# length(list_id)

for (id in list_id){
  if (check_file){
    # check in comment is "ok"
    comment = tail(sheet2$`Comment on this sample`[list_id == id], 1)
    if (comment != 'ok' & comment != "0s") {
      next
    }
  }
  sample = read.table(paste0(path_local, "/data_curve/", id, '.csv'), sep = "\t", header = T)
  index_sample = which(df$id == id)
  if (sum(is.na(df[index_sample, c("index_1", "index_2_graph", "index_3", "index_4", "index_5", "index_6")])) == 0){
    gg_index = c(1:df[index_sample, "index_1"], 
                 df[index_sample, "index_1"]:df[index_sample, "index_2"], 
                 df[index_sample, "index_2"]:df[index_sample, "index_3"],
                 df[index_sample, "index_3"]:df[index_sample, "index_4"],
                 df[index_sample, "index_4"]:df[index_sample, "index_5"],
                 df[index_sample, "index_5"]:dim(sample)[1])
    
    gg_region = as.factor(c(rep(1, length(1:df[index_sample, "index_1"])),
                            rep(2, length((df[index_sample, "index_1"]):df[index_sample, "index_2"])),
                            rep(3, length((df[index_sample, "index_2"]):df[index_sample, "index_3"])),
                            rep(4, length((df[index_sample, "index_3"]):df[index_sample, "index_4"])),
                            rep(5, length((df[index_sample, "index_4"]):df[index_sample, "index_5"])),
                            rep(6, length((df[index_sample, "index_5"]):dim(sample)[1]))))
    
    gg_data = data.frame("extension" = sample$extension[gg_index],
                         "load" = sample$load[gg_index],
                         "region" = gg_region)
    
    new_index_pos = gg_data$extension[unlist(df[index_sample, c("index_1", "index_2", "index_3", "index_4", "index_5", "index_6")]) + c(0, 1, 2, 3, 4, 5)]
    
    # r?duction du jeu de donnees
    new_gg_start = df[index_sample, "index_1"] - 10
    if (new_gg_start < 1) new_gg_start = 1
    
    new_gg_end = df[index_sample, "index_5"] + 10
    if (new_gg_end > 500) new_gg_end = 500
    
    new_gg_index = new_gg_start:new_gg_end

    gg_data = data.frame("extension" = gg_data$extension[new_gg_index],
                         "load" = gg_data$load[new_gg_index],
                         "region" = gg_data$region[new_gg_index])
    
    new_index_pos[length(new_index_pos)] = tail(na.omit(gg_data$extension), 1)
    
    gg_data = na.omit(gg_data)
    p = ggplot(data = gg_data, aes(x = extension, y = load, color = region)) +
      geom_line() +
      theme_minimal() +
      geom_vline(xintercept = new_index_pos, linetype = "dashed", col = gg_color_hue(6)) +
      labs(x = 'Extension', y = 'Load', title = id) +
      annotate("text", x = gg_data$extension[df[index_sample, "index_1"]], y=0.07, label= df[index_sample, "med_noise_1"]) +
      annotate("text", x = gg_data$extension[df[index_sample, "index_4"]], y=0.07, label= df[index_sample, "med_noise_4"]) +
      annotate("text", x = gg_data$extension[df[index_sample, "index_6"]], y=0.06, label= df[index_sample, "med_noise_6"]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    p
    
    ggsave(file = paste(path_output2, id, ".svg", sep = ""), plot=p, width=10, height=8, device = "svg")
    ggsave(file = paste(path_output2, id, ".pdf", sep = ""), plot=p, width=10, height=8, device = "pdf")
    
  }
}

