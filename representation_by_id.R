rm(list = ls())

library("ggplot2")
# library("svglite")
library("config")

### functions ###

gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

###

# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "portable")

# retrieve parameters
path_metadata_file = opt$concatenate_metadata
plot_path = opt$plot_path
path_index = opt$index_path
path_batch_by_id = opt$batch_by_id
comment_accepted = gsub(" +$", "", gsub("^ +", "", unlist(strsplit(opt$comment_accepted, ","))))

# load data
df = read.table(path_index, header = T, sep = "\t")
metadata = read.table(path_metadata_file, sep = "\t", header = T)

dir.create(plot_path, showWarnings = FALSE)

# plot f(time) = load
path_output_tl = paste0(plot_path, "/load_time/")
dir.create(path_output_tl, showWarnings = FALSE)

# plot f(extension) = load
path_output_el = paste0(plot_path, "/load_extension/")
dir.create(path_output_el, showWarnings = FALSE)

list_id = df$id
for (id in list_id){
  current_comment = metadata$Comment[which(metadata$Sample_ID == id)]
  
  # if (file.exists(paste0(path_output_tl, id, ".pdf"))) next
  
  sample = read.table(paste0(path_batch_by_id, "/", id, '.csv'), sep = "\t", header = T)
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
                         "extension" = sample$extension[gg_index],
                         "region" = gg_region)
    
    new_index_pos = gg_data$time[unlist(df[index_sample, c("index_1", "index_2_graph", "index_3", "index_4", "index_5", "index_6")]) + c(0, 1, 2, 3, 4, 5)]
    
    p_tl = ggplot(data = gg_data, aes(x = time, y = load, color = region)) +
      geom_line() +
      theme_minimal() +
      geom_vline(xintercept = new_index_pos, linetype = "dashed", col = gg_color_hue(6)) +
      labs(x = 'Time', y = 'Load', title = id) +
      annotate("text", x = gg_data$time[df[index_sample, "index_1"]], y=0.07, label= df[index_sample, "med_noise_1"]) +
      annotate("text", x = gg_data$time[df[index_sample, "index_4"]], y=0.07, label= df[index_sample, "med_noise_4"]) +
      annotate("text", x = gg_data$time[df[index_sample, "index_6"]], y=0.06, label= df[index_sample, "med_noise_6"]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(file = paste0(path_output_tl, id, ".pdf"), plot=p_tl, width=12, height=8, device = "pdf")
    
    # plot range reduction for extension(x) = load
    new_index_pos = gg_data$extension[unlist(df[index_sample, c("index_1", "index_2", "index_3", "index_4", "index_5", "index_6")]) + c(0, 1, 2, 3, 4, 5)]
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

    p_el = ggplot(data = gg_data, aes(x = extension, y = load, color = region)) +
      geom_line() +
      theme_minimal() +
      geom_vline(xintercept = new_index_pos, linetype = "dashed", col = gg_color_hue(6)) +
      labs(x = 'Extension', y = 'Load', title = id) +
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(file = paste0(path_output_el, id, ".pdf"), plot=p_el, width=12, height=8, device = "pdf")
  }
}

