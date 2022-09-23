### Day 3
#### Step-by-step analysis of data
#### 05 disparity

# your name/the name you gave to your data folder
# EDITORNAME <- "CNicolas2016" # for example
EDITORNAME <- "editor_name"

#############################################
# set output folder + path

output_folder <- file.path(".", 
                           "3_output", 
                           EDITORNAME)
# set input folder
input_folder <- file.path(".",
                          "1_data",
                          EDITORNAME)

#############################################

# load data; decide on one of the two data sets (with/without outliers)!!!!
outlines_centered_scaled_meta_data_PCA <- 
  readRDS(file = file.path(output_folder, 
                           paste0("outlines_centered_scaled_meta_data_wo_outliers_PCA_", EDITORNAME, ".RDS")))
# outlines_centered_scaled_meta_data_PCA <-
#   readRDS(file = file.path(output_folder, 
#                            paste0("outlines_centered_scaled_meta_data_PCA_", EDITORNAME, ".RDS")))

# load the artefact_ID-cluster information, note! it is dependent on which data set (with/without outliers) you used in the previous step!
current_treecut <- 
  readr::read_csv(file = file.path(output_folder, paste0("artefact_ID_cluster_df_", EDITORNAME, ".csv")))


# combine meta data with cluster information
meta_data_artefactID_cluster <- 
  dplyr::left_join(outlines_centered_scaled_meta_data_PCA$fac,
                   current_treecut,
                   by = "artefact_ID")

meta_data_artefactID_cluster$cluster <- factor(meta_data_artefactID_cluster$cluster)
# meta_data_artefactID_cluster$archaeological_period <- factor(meta_data_artefactID_cluster$archaeological_period)


# select the appropriate amount of colors
set.seed(1)
outlines_colors_wo_outliers <- RColorBrewer::brewer.pal(n = length(unique(meta_data_artefactID_cluster$cluster)),
                                                        "Paired")

#############################################


# custom bins based on period
grouping_disparity <- list()
for(i in unique(meta_data_artefactID_cluster$country)){
  grouping_disparity[[i]] <- as.character(subset(outlines_centered_scaled_meta_data_PCA$fac, country == i)$artefact_ID)
}

TS_subsets <- 
  dispRity::custom.subsets(outlines_centered_scaled_meta_data_PCA$x, 
                           group = grouping_disparity)

TS_boot <- dispRity::boot.matrix(TS_subsets, 
                                 bootstraps = 1000)
TS_disp <- dispRity::dispRity(TS_boot, 
                              metric = c(sum, dispRity::variances))
summary(TS_disp)

# Wilcox.test
dispRity::test.dispRity(TS_disp, 
                        test = wilcox.test, 
                        comparisons = "pairwise",
                        correction = "bonferroni")
# PERMANOVA
dispRity::test.dispRity(TS_disp, 
                        test = dispRity::adonis.dispRity, 
                        comparisons = "pairwise",
                        correction = "bonferroni")


TS_names <- names(TS_disp$disparity)
disparity_df_list <- list()
for(i in TS_names){
  disparity_df_list[[i]] <- data.frame(Period = paste0(i, 
                                                       "\n(n=",nrow(TS_disp$subsets[[i]]$elements),")"),
                                       disparity = as.vector(TS_disp$disparity[[i]][[2]]),
                                       nelements = nrow(TS_disp$subsets[[i]]$elements),
                                       TS = i)
}
disparity_df_TSdiscrete_armatureOutlines_perTShard <- do.call(rbind.data.frame, disparity_df_list)

library(ggplot2)

disparity_TSdiscrete_armatureOutlines_ggplot_perTShard <- 
  ggplot(data = disparity_df_TSdiscrete_armatureOutlines_perTShard, 
         aes(x = Period, 
             y = disparity)) +
  geom_violin(aes(fill = TS)) + 
  geom_boxplot(notch = T, 
               width = 0.1, 
               fill = "white", 
               color = "black") +
  theme_bw() +
  ggtitle(NULL) +
  xlab("") + 
  ylab("Disparity") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 14, 
                                  face = "bold"),
        axis.text=element_text(size=21), #,face="bold"
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5, 
                                   hjust = 0.5, 
                                   size=25),
        axis.title.y = element_text(vjust = 0),
        axis.title=element_text(size=25)) +
  # ggthemes::scale_fill_colorblind() +
  guides(color = FALSE, 
         fill = FALSE)

disparity_TSdiscrete_armatureOutlines_ggplot_perTShard


























