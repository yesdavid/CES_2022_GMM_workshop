### Day 3
#### Step-by-step analysis of data
#### 04 plotting clusters

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

### distribution map
world <- rgeos::gBuffer(rworldmap::getMap(resolution = "high"), byid=TRUE, width=0)

# create an extent which spans only the distribution of our samples
# potentially, the extents have to be manually in-/decreased
data_extent <- as(raster::extent(min(meta_data_artefactID_cluster$longitude_x, #minimum longitude
                                     na.rm = T)-1, 
                                 max(meta_data_artefactID_cluster$longitude_x, #maximum longitude
                                     na.rm = T)+1, 
                                 min(meta_data_artefactID_cluster$latitude_y, #minimum latitude
                                     na.rm = T)-1, 
                                 max(meta_data_artefactID_cluster$latitude_y, #maximum latidude
                                     na.rm = T)+1), # order: xmin, xmax, ymin, ymax
                  "SpatialPolygons")

sp::proj4string(data_extent) <- sp::CRS(sp::proj4string(world)) # set the coordinate reference system of the data to be the same as the world map.

world_clip <- raster::intersect(world, data_extent) # select only those parts of the world map within our bounding box/extent 

world_clip_f <- fortify(world_clip) # transforms it into a data frame

# plot
## base map
base_map <- 
  ggplot() +
  geom_polygon(data = world_clip_f, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA, 
               colour = "grey") +
  coord_fixed() +
  coord_quickmap() +  
  theme_classic() + 
  xlab("Longitude") +
  ylab("Latitude")  +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0))

base_map

## base map + artefacts as points colored by their cluster
clusters_mapped_plot <- 
base_map +
  geom_point(data = meta_data_artefactID_cluster,
             aes(x = longitude_x,
                 y = latitude_y,
                 color = cluster,
                 shape = country)) +
  labs(color = "Cluster",
       shape = "Country") + # change names to upper case 
theme(legend.position = "right", # or "none"
      text = element_text(size=20))

clusters_mapped_plot

## base map + artefacts as points colored by their cluster, separated by their cluster
clusters_mapped_plot +
  facet_wrap(~cluster)

## base map + artefacts as points colored by their cluster, separated by their cluster + country
clusters_mapped_plot +
  facet_grid(cluster~country) +
  theme(legend.position = "none", 
        text = element_text(size=20))

## base map + artefacts as points colored by their cluster, separated by the archaeological period
clusters_mapped_plot +
  facet_wrap(~archaeological_period)


#############################################

# PCA colored by cluster






