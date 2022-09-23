### Day 3
#### Step-by-step analysis of data
#### 03 hierarchical clustering

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

# load data; decide on one of the two data sets (with/without outliers)!!!!
outlines_centered_scaled_meta_data_PCA <- 
  readRDS(file = file.path(output_folder, 
                           paste0("outlines_centered_scaled_meta_data_wo_outliers_PCA_", EDITORNAME, ".RDS")))
# outlines_centered_scaled_meta_data_PCA <-
#   readRDS(file = file.path(output_folder, 
#                            paste0("outlines_centered_scaled_meta_data_PCA_", EDITORNAME, ".RDS")))


## Test how many PC axes describe `proportion_of_variance`% of the variance.
proportion_of_variance <- 0.95

minimum_no_of_pcs_wo_outliers <- 
  Momocs::scree_min(outlines_centered_scaled_meta_data_PCA,
                    prop = proportion_of_variance) 


############# hierarchical clustering


## distance matrix
outlines_centered_scaled_meta_data_PCA_as_matrix <- 
  as.matrix(outlines_centered_scaled_meta_data_PCA$x[,1:minimum_no_of_pcs_wo_outliers])

outlines_dist <- 
  dist(outlines_centered_scaled_meta_data_PCA_as_matrix,
       method = "euclidean")

## hierarchical clustering
outlines_wardD2 <- 
  hclust(outlines_dist,
         method = "ward.D2")

# plot with ggtree. potentially unreadable because of amount of artefacts
ggtree::ggtree(outlines_wardD2) +
  ggtree::geom_tiplab()


############# determine the number of clusters k on the basis of the average silhouette value (Rousseeuw, 1987). 
# The silhouette coefficient measures the average distance based
# on the observations of each point within a single own cluster compared to each point in the other clusters. 
# The coefficient can thus be calculated for each observation in the whole dataset and averaged for each number of possible clusters. 
# The highest average silhouette value indicates the optimal number of clusters k.

outlines_NbClust_ward <- 
  NbClust::NbClust(data = outlines_centered_scaled_meta_data_PCA$x[,1:minimum_no_of_pcs_wo_outliers],
                   distance = "euclidean", # euclean distance matrix
                   method = "ward.D2", # Ward's clustering algorithm
                   index = "silhouette") # silhouette value as method to indentify the ideal number of clusters.

# transform results into appropriate data frame
outlines_NbClust <- 
  outlines_NbClust_ward$All.index

outlines_NbClust_df <- 
  as.data.frame(outlines_NbClust)

outlines_NbClust_df$NClust <- 
  1:nrow(outlines_NbClust_df)+1 #+1 because the first observation is for a two-cluster situation

# silhouette plot
silhouette_plot <- 
  ggplot(outlines_NbClust_df, 
         aes(x = NClust, 
             y = outlines_NbClust)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() +
  scale_x_continuous(breaks = seq(1,
                                  max(outlines_NbClust_df$NClust),
                                  by = 1)) +
  xlab("Number of clusters") +
  ylab("Average silhouette value")

# inspect the plot
silhouette_plot

# chose the (local) optimum based on silhouette_plot
n_clusters_outlines_wo_outliers <- 5

# select the appropriate amount of colors
set.seed(1)
outlines_colors_wo_outliers <- RColorBrewer::brewer.pal(n = n_clusters_outlines_wo_outliers,
                                                            "Paired")



# we can now prune (cut) the tree at the appropriate height, to get the optimal cluster association of each artefact.
current_treecut <- 
  data.frame(artefact_ID = row.names(as.data.frame(cutree(outlines_wardD2,
                                                          k = n_clusters_outlines_wo_outliers))),
             cluster = as.factor(as.data.frame(cutree(outlines_wardD2, 
                                                      k = n_clusters_outlines_wo_outliers))[[1]]))
# inspect the data frame
head(current_treecut)

# save the artefact_ID-cluster information
readr::write_csv(current_treecut,
                 file = file.path(output_folder, paste0("artefact_ID_cluster_df_", EDITORNAME, ".csv")))

# prune the tree at the right hight to arrive at chosen number of cluster
outlines_wardD2_pruned_tree <- 
  maptree::clip.clust(outlines_wardD2, 
                      current_treecut, 
                      k=n_clusters_outlines_wo_outliers)

# plot the pruned tree
ggtree::ggtree(outlines_wardD2_pruned_tree) +
  ggtree::geom_tiplab()

# save the tree topology
ape::write.nexus(ape::as.phylo(outlines_wardD2_pruned_tree),
                 file = file.path(output_folder, paste0("outlines_wardD2_pruned_tree_", EDITORNAME, ".tre")))


# which artefacts are in which cluster?
# load the outlines + meta data file; decide on one of the two data sets (with/without outliers)!!!!
outlines_centered_scaled_meta_data <- 
  readRDS(file = file.path(output_folder, paste0("outlines_centered_scaled_meta_data_wo_outliers_", EDITORNAME, ".RDS")))
# outlines_centered_scaled_meta_data <- 
#   readRDS(file = file.path(output_folder, paste0("outlines_centered_scaled_meta_data_", EDITORNAME, ".RDS")))



## plot a panel of outline shapes for each of the cluster
# panels for each cluster. remove the "#" below to save each panel as a .png and .svg
for (i in 1:n_clusters_outlines_wo_outliers){
  
  # png(file=file.path(output_folder, paste0("outlines_cluster_panel_cluster_", i, ".png")),
  #     width = 800, height = 800, units = "px")
  Momocs::panel(Momocs::filter(outlines_centered_scaled_meta_data, 
                               artefact_ID %in% subset(current_treecut, cluster == i)$artefact_ID),
                main = paste("Cluster", i),
                col = outlines_colors_wo_outliers[i])
  # dev.off()
  
  # svg(file=file.path(output_folder, paste0("outlines_cluster_panel_cluster_", i, ".svg")),
  #       width = 8, height = 8,
  #       bg = "transparent")
  # Momocs::panel(Momocs::filter(outlines_centered_scaled_meta_data,
  #                              artefact_ID %in% subset(current_treecut, cluster == i)$artefact_ID),
  #               main = NULL,
  #               col = outlines_colors_wo_outliers[i])
  # dev.off()
}



# now, we can create mean shapes of the outlines for each cluster
# mean shapes
min_no_of_coordinates <- list()
for (cluster_index in 1:n_clusters_outlines_wo_outliers){
  
  current_shapes <- Momocs::filter(outlines_centered_scaled_meta_data, 
                                   artefact_ID %in% subset(current_treecut, cluster == cluster_index)$artefact_ID)
  
  min_no_of_coordinates[[cluster_index]] <- rep(NA, length(current_shapes))
  
  for (i in 1:length(current_shapes)){
    min_no_of_coordinates[[cluster_index]][i] <- nrow(current_shapes$coo[[i]])
  }
  
  min_no_of_coordinates[[cluster_index]] <- min(min_no_of_coordinates[[cluster_index]])
  
}

## shapes get INTERPOLATED to common number of landmarks (lowest number of landmarks per cluster)
mean_shapes_cluster_wo_outliers <- list()
for (cluster_index in 1:n_clusters_outlines_wo_outliers){
  mean_shapes_cluster_wo_outliers[[cluster_index]] <- 
    Momocs::MSHAPES(Momocs::coo_interpolate(Momocs::filter(outlines_centered_scaled_meta_data, 
                                                           artefact_ID %in% subset(current_treecut, cluster == cluster_index)$artefact_ID),
                                            n = min_no_of_coordinates[[cluster_index]])$coo)
}

outlines_centered_scaled_meta_data_PCA_mean_shapes_cluster_out <- Momocs::Out(mean_shapes_cluster_wo_outliers,
                                                                                   fac = data.frame(cluster = paste0("Cluster ", c(1:n_clusters_outlines_wo_outliers))))

# plot all the mean shapes
Momocs::panel(outlines_centered_scaled_meta_data_PCA_mean_shapes_cluster_out,
              main = NULL,
              col = outlines_colors_wo_outliers)

## plot each individual mean shape for each of the cluster
# remove the "#" below to save each panel as a .png and .svg
for (i in 1:n_clusters_outlines_wo_outliers){
  
  png(file=file.path(output_folder, paste0("outlines_cluster_meanshp_cluster_", i, ".png")),
      width = 800, height = 800, units = "px")
  Momocs::panel(Momocs::filter(outlines_centered_scaled_meta_data_PCA_mean_shapes_cluster_out, 
                               cluster %in% paste0("Cluster ", i)),
                main = paste0("Cluster ", i),
                col = outlines_colors_wo_outliers[i])
  dev.off()
  
  # svg(file=file.path(output_folder, paste0("outlines_cluster_meanshp_cluster_", i, ".svg")), 
  #     width = 8, height = 8, 
  #     bg = "transparent")
  # Momocs::panel(Momocs::filter(outlines_centered_scaled_meta_data_PCA_mean_shapes_cluster_out, 
  #                              cluster %in% paste0("Cluster ", i)),
  #               main = NULL,
  #               col = outlines_colors_wo_outliers[i]) 
  # dev.off()
}

####
# change tip labels of tree
new_labels <- 
  rep(NA, 
      length(outlines_wardD2_pruned_tree$labels))

for (i in 1:length(outlines_wardD2_pruned_tree$labels)){
  new_labels[i] <-
    paste0("Cluster ", 
           outlines_wardD2_pruned_tree$labels[i], 
           " (n=",colSums(table(current_treecut))[[i]], ")")
}

new_labels # the new labels for the pruned tree

# overwrite old names with new names containing the number of taxa included in each cluster.
outlines_wardD2_pruned_tree$labels <- new_labels

# plot the tree with the new labels
pruned_tree_plot <- 
  ggtree::ggtree(outlines_wardD2_pruned_tree, 
                 layout = "rectangular") + 
  xlim(NA,6) +
  # the following command allows for the placement of the mean shape images at the tips of the tree. buggy.
  # ggtree::geom_tiplab(aes(image=file.path(output_folder, paste0(label, ".png"))), 
  #             geom="image", 
  #             offset=0.5, 
  #             align=2, 
  #             hjust = 0.5, 
  #             size = 0.1) +
  ggtree::geom_tiplab(geom='label',
                      # offset=1, #adjust distance from text to tip
                      # hjust=-1, #horizontal adjustment. -1,0.5,1 (left, middle, right)
                      vjust = 0.5, #vertical adjustment. -1,0.5,1 (bottom, middle, top)
                      size = 6, 
                      fontface='italic', 
                      family="TT Times New Roman") + 
  ggtree::geom_tippoint() +
  ggtree::geom_treescale()

pruned_tree_plot

# save the tree
ggsave(filename = file.path(output_folder, paste0("pruned_tree_", EDITORNAME, ".svg")),
       plot = pruned_tree_plot,
       device = "svg",
       width = 25, #adjust accordingly
       height = 35, #adjust accordingly
       units = "cm")








