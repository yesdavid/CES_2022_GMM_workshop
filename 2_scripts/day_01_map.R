### Day 1
#### 02 map sites of data set

library(ggplot2)
#############################################
# your name/the name you gave to your data folder
# EDITORNAME <- "CNicolas2016" # for example
EDITORNAME <- "editor_name"
# set input folder
input_folder <- file.path(".",
                          "1_data",
                          EDITORNAME)

#############################################
# load meta data
outline_metadata <- readr::read_csv(file = file.path(input_folder, 
                                                     paste0("outline_metadata_", EDITORNAME, ".csv")))
## inspect meta data 
head(outline_metadata)
#############################################

### distribution map
world <- rgeos::gBuffer(rworldmap::getMap(resolution = "high"), byid=TRUE, width=0)

# create an extent which spans only the distribution of our samples
# potentially, the extents have to be manually in-/decreased
data_extent <- as(raster::extent(min(outline_metadata$longitude_x, #minimum longitude
                                        na.rm = T)-1, 
                                    max(outline_metadata$longitude_x, #maximum longitude
                                        na.rm = T)+1, 
                                    min(outline_metadata$latitude_y, #minimum latitude
                                        na.rm = T)-1, 
                                    max(outline_metadata$latitude_y, #maximum latidude
                                        na.rm = T)+1), # order: xmin, xmax, ymin, ymax
                     "SpatialPolygons")

sp::proj4string(data_extent) <- sp::CRS(sp::proj4string(world)) # set the coordinate reference system of the data to be the same as the world map.

world_clip <- raster::intersect(world, data_extent) # select only those parts of the world map within our bounding box/extent 

world_clip_f <- fortify(world_clip) # transforms it into a data frame

# plot
ggplot() +
  geom_polygon(data = world_clip_f, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA, 
               colour = "grey") +
  geom_point(data = outline_metadata,
             aes(x = longitude_x,
                 y = latitude_y,
                 color = country)) +
  coord_fixed() +
  coord_quickmap() +  
  theme_classic() + 
  xlab("Longitude") +
  ylab("Latitude")  +
  labs(color = "Country") + # capitalize 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  theme(legend.position = "right", # or "none"
        text = element_text(size=20))
