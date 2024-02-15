rm(list = ls())

library("config")
library("ggplot2")
library("dplyr")
library("rcompanion")
library("agricolae")
library("FSA")
library("ggpubr")
library("DescTools")
library("ggtext")
library("doBy")
library("ggrepel")
library("reshape2")
library("gridExtra")
library("pivottabler") ### doc : http://www.pivottabler.org.uk/articles/v04-regularlayout.html
library("extrafont")
library("ggsci")
library("ggnewscale")
library("ape")
# font_import()
loadfonts(device = "win")


reorder_by_factor = function(data, factor_name, fun, parameter) {
  order_data = data %>%
    group_by(!!as.symbol(factor_name)) %>% 
    filter(!is.na(!!as.symbol(parameter))) %>%
    filter(is.finite(!!as.symbol(parameter))) %>%
    summarise(fun = 
                do.call(fun, list(x = (!!as.symbol(parameter)))))
  order_data = as.data.frame(order_data[order(order_data$fun), ])
  
  return(order_data[[factor_name]])
}


format_label = function(factor_name, factor_labels, stat_group = NULL, n_data = NULL) {
  if (factor_name == "Species"){
    
    a_col = levels(factor_labels)
    a_col = gsub("_", " ", a_col, fixed = T)
    a_col = gsub("Drosophila", "D.", a_col, fixed = T)
    a_col = gsub("Megaselia", "M.", a_col, fixed = T)
    a_col = gsub("Scaptodrosophila", "S.", a_col, fixed = T)
    a_col = gsub("Zaprionus", "Z.", a_col, fixed = T)
    a_col = substr(a_col, 1, 8)
    #a_col = paste0("*", a_col, "*")
    a_col = StrAlign(a_col, sep = "\\l")
  } else {
    a_col = StrAlign(levels(factor_labels), sep = "\\r")
  }
  
  if (length(stat_group) > 0){
    ordered_groups = unlist(lapply(levels(stat_group[[factor_name]]),
                                   function(x) {
                                     stat_group[stat_group[[factor_name]] == x, ]$groups
                                   }))
    group_diversity = sort(unique(unlist(lapply(ordered_groups, function(x) {
      return(base::unlist(strsplit(x, split = "")))
    }))))
    
    edited_groups = unlist(lapply(ordered_groups, function(g) {
      base::paste(unlist(lapply(group_diversity, function(x) {
        if (grepl(x, g)) {
          return(x)
        } else {
          return(" ")
        }
      })), collapse = "")
    }))
    
    
    b_col = edited_groups
  } else {
    b_col = ""
  }
  
  if (length(n_data) > 0){
    
    n_count = unlist(lapply(levels(n_data[[factor_name]]), 
                            function(x) {
                              sum(n_data[[factor_name]] == x)
                            }))
    c_col = StrAlign(paste0(n_count), sep = "\\r")
  } else {
    c_col = ""
  }
  
  labels =  paste(a_col,
                  b_col,
                  c_col,
                  sep = "   ")
  return(labels)
}


# load config file
opt = config::get(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/config.yml"), config = "manon_acanthoptera")

# retrieve parameters
# Input
path_index = opt$index_path
path_batch_by_id = opt$batch_by_id
plot_path = opt$plot_path
path_integral = opt$integral_path

parameter_list = gsub(" +$", "", 
                      gsub("^ +", "", unlist(strsplit(opt$parameter_list, ","))))
parameter_list_tree = gsub(" +$", "", 
                           gsub("^ +", "", unlist(strsplit(opt$parameter_list_tree, ","))))
lab_list = gsub(" +$", "", 
                gsub("^ +", "", unlist(strsplit(opt$lab_list, ","))))
unit_list = gsub(" +$", "",
                 gsub("^ +", "", unlist(strsplit(opt$unit_list, ","))))
stat_list = gsub(" +$", "", 
                 gsub("^ +", "", unlist(strsplit(opt$stat_list, ","))))

comment_list = gsub(" +$", "", 
                    gsub("^ +", "", unlist(strsplit(opt$comment_list, ","))))

comment_lab_list = gsub(" +$", "", 
                        gsub("^ +", "", unlist(strsplit(opt$comment_lab_list, ","))))

# read data figure
gg_data = read.table(paste0(plot_path, "/data_figure.csv"), 
                     stringsAsFactors = F,
                     sep = "\t",
                     header = T)

species_list = unique(gg_data$Species)
stock_list = unique(gg_data$Stock)
protocol_list = unique(gg_data$Protocol)


####data prepatation

temp_gg_data = gg_data  %>%
  filter(Species != "Megaselia_abdita") %>%
  filter(Species != "Megaselia_scalaris") %>%
  filter(Species != "Drosophila_elegans") %>%
  filter(Species != "Drosophila_quadraria") %>%
  filter(
    
    ((Species == "Drosophila_melanogaster" & Protocol == "standard" & Stock == "cantonS") |
       (Species == "Drosophila_hydei" & Protocol == "1 strong tape ; 0.25 N") |
       (Species == "Drosophila_suzukii" & Stock == "WT3") |
       (Species == "Drosophila_biarmipes" & Stock == "G224") |
       (Species == "Drosophila_simulans" & Stock == "simulans_vincennes"))
    |
      (! Species %in% c("Drosophila_melanogaster", 
                        "Drosophila_suzukii", 
                        "Drosophila_biarmipes", 
                        "Drosophila_simulans", 
                        "Drosophila_hydei"))
  ) %>%
  filter(Comment == "ok") %>%
  group_by(Species)

temp_gg_data$Species = factor(temp_gg_data$Species, 
                              levels = unique(temp_gg_data$Species),
                              ordered = T)

result_df <- data.frame()

# Create a list of color gradients for each parameter in parameter_list_tree

color_gradients <- list(detachment_force = c("blue", "red"), 
                        pupa_shape = c("green", "red"),
                        detachment_force_div_glue_area = c("black", "red"),
                        glue_area_mm = c("yellow", "red")
)

for (i in 1:length(parameter_list_tree)){
  
  parameter_name <- parameter_list_tree[i]
  median_col_name <- paste0("median_", parameter_name)
  
  temp_gg_data <- temp_gg_data %>%
    mutate(!!median_col_name := median((!!sym(parameter_name)), na.rm = TRUE))
  
}

# Extract relevant columns for gg_repel_data and aggregate by Species
gg_repel_data <- temp_gg_data %>%
  select(Species, starts_with("median_")) %>%
  group_by(Species) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  distinct()

new_names <- c( "Detachment force" = "median_detachment_force", 
               "Pupa shape" = "median_pupa_shape",
               "Detachment force divided by glue area" = "median_detachment_force_div_glue_area",
               "Glue area" = "median_glue_area_mm")
gg_repel_data <- rename(gg_repel_data, all_of(new_names))

# Additional processing for short_name
short_name <- gg_repel_data$Species
short_name <- gsub("_", " ", short_name, fixed = TRUE)
short_name <- gsub("Drosophila", "D.", short_name, fixed = TRUE)
short_name <- gsub("Megaselia", "M.", short_name, fixed = TRUE)
short_name <- gsub("Scaptodrosophila", "S.", short_name, fixed = TRUE)
short_name <- gsub("Zaprionus", "Z.", short_name, fixed = TRUE)
short_name <- substr(short_name, 1, 8)

# Combine relevant columns and add to 'result_df'
result_df <- cbind(gg_repel_data, 
                   data.frame("species_short" = short_name))

result_df <- melt(result_df)
result_df <- setNames(result_df, c("Species", "species_short", "parameter", "median"))


# result_df$Species = factor(result_df$Species,
#                                  levels = c("Drosophila_mauritiana",
#                                  "Drosophila_simulans",
#                                  "Drosophila_melanogaster",
#                                  "Drosophila_yakuba",
#                                  "Drosophila_eugracilis",
#                                  "Drosophila_biarmipes",
#                                  "Drosophila_suzukii",
#                                  "Drosophila_prostipennis",
#                                  "Drosophila_takahashii",
#                                  "Drosophila_rhopaloa",
#                                  "Drosophila_kurseongensis",
#                                  "Drosophila_malerkotliana",
#                                  "Drosophila_ananassae",
#                                  "Drosophila_pseudoobscura",
#                                  "Drosophila_tropicalis",
#                                  "Zaprionus_lachaisei",
#                                  "Zaprionus_indianus",
#                                  "Drosophila_immigrans",
#                                  "Drosophila_funebris",
#                                  "Drosophila_pachea",
#                                  "Drosophila_nannoptera",
#                                  "Drosophila_hydei",
#                                  "Drosophila_littoralis",
#                                  "Drosophila_virilis",
#                                  "Scaptodrosophila_lebanonensis"), ordered = T)

levels_order_species <- c("D. mauri",
                          "D. simul",
                          "D. melan",
                          "D. yakub",
                          "D. eugra",
                          "D. biarm",
                          "D. suzuk",
                          "D. prost",
                          "D. takah",
                          "D. rhopa",
                          "D. kurse",
                          "D. maler",
                          "D. anana",
                          "D. pseud",
                          "D. tropi",
                          "Z. lacha",
                          "Z. india",
                          "D. immig",
                          "D. funeb",
                          "D. pache",
                          "D. nanno",
                          "D. hydei",
                          "D. litto",
                          "D. viril",
                          "S. leban")

levels_order_parameter <- c("Pupa shape",
                            "Glue area",
                            "Detachment force",
                            "Detachment force divided by glue area")



### categoriser les valeurs de medianes pour chaque parametre
plot_path_one_parameter_by_species = paste0(plot_path, "/one_parameter/by_species/")
dir.create(plot_path_one_parameter_by_species, showWarnings = FALSE, recursive = T)

p <- ggplot() +
  
  geom_tile(
    data = result_df %>% filter(parameter == "Detachment force divided by glue area"), 
    aes(x = parameter, 
        y = factor(species_short, level = levels_order_species), fill = median),
    color = "white", lwd = 1.5, linetype = 1
  ) + 
  scale_fill_gradient(low = "orange",
                      high =  "red",
                      na.value = "white") +
  labs(fill = "Detachment force divided by glue area") +
  
  new_scale_fill() +
  
  geom_tile(
    data = result_df %>% filter(parameter == "Detachment force"), 
    aes(x = parameter, 
        y = factor(species_short, level = levels_order_species), fill = median),
    color = "white", lwd = 1.5, linetype = 1
  ) + 
  scale_fill_gradient(low = "lightblue",
                      high =  "blue",
                      na.value = "white") +
  labs(fill = "median_detachment_force") +
  
  
  new_scale_fill() +
  
  geom_tile(
    data = result_df %>% filter(parameter == "Pupa shape"), 
    aes(x = parameter, 
        y = factor(species_short, level = levels_order_species), fill = median),
    color = "white", lwd = 1.5, linetype = 1
  ) + 
  scale_fill_gradient(low = "lightgreen",
                      high =  "darkgreen",
                      na.value = "white") +
  labs(fill = "median_pupa_shape") +
  
  new_scale_fill() +
  
  
  
  geom_tile(
    data = result_df %>% filter(parameter == "Glue area"), 
    aes(x = parameter, 
        y = factor(species_short, level = levels_order_species), fill = median),
    color = "white", lwd = 1.5, linetype = 1
  ) + 
  scale_fill_gradient(low = "pink",
                      high =  "purple",
                      na.value = "white") +
  scale_x_discrete(limits = levels_order_parameter) +
  # theme_bw(base_size = 18) +
  labs(fill = "median_glue_area_mm") +
  theme(legend.position="right",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15, family = "Courier New"),
        axis.text.y = element_text( size = 15, family = "Courier New"),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_equal() #square shaped


p


ggsave(file = paste0(plot_path_one_parameter_by_species, "/heatmap", ".pdf"), 
       plot=p, width=20, height=10, device = cairo_pdf)

#####arbre phylo#######

tree <- read.tree(text = "(((((((((((((D._mauritiana,D._simulans),D._melanogaster),D._yakuba),
                  D._eugracilis),((D._biarmipes,D._suzukii),(D._prostipennis,D._takahashii))),
                  ((D._rhopaloa,D._kurseongensis)))),(D._malerkotliana,D._ananassae))
                  ,D._pseudoobscura),D._tropicalis),(((Z._lachaisei,Z._indianus),(D._immigrans,D._funebris)),
                  (((D._pachea,D._nannoptera),D._hydei),(D._littoralis,D._virilis)))),S._lebanonensis));")

# Plot the tree using plot.phylo
plot(tree)
