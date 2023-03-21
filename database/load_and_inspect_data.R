#######################
### load data
outlines_combined <- readRDS(file = file.path("database", "outlines_combined_with_metadata.RDS"))
#######################

#######################
### data inspection
# show a panel of all outlines
Momocs::panel(outlines_combined,
              cols= "black")

# all outlines stacked on top of each other
Momocs::stack(outlines_combined)

# inspection of each individual outline. Press "escape" to quit. 
inspect(outlines_combined)


### inspect the associated metadata

# get a glimpse of the first couple of rows
head(outlines_combined$fac)

# view the whole metadata
View(outlines_combined$fac)

# count the outlines per country
table(outlines_combined$fac$country)

# count the outlines per archaeological culture
table(outlines_combined$fac$archaeological_culture_name)

# count the outlines per archaeological period
table(outlines_combined$fac$archaeological_period)


### map the distribution of sites covered in this data set

unique_sites <- outlines_combined_metadata %>% 
  select(editor_name,
         latitude_y,
         longitude_x) %>% 
  unique() 


# create base world map
world <- rgeos::gBuffer(rworldmap::getMap(resolution = "high"), byid=TRUE, width=0)

# create an extent which spans only the distribution of our samples
# potentially, the extents have to be manually in-/decreased
data_extent <- as(raster::extent(min(unique_sites$longitude_x, #minimum longitude
                                     na.rm = T)-2, 
                                 max(unique_sites$longitude_x, #maximum longitude
                                     na.rm = T)+2, 
                                 min(unique_sites$latitude_y, #minimum latitude
                                     na.rm = T)-2, 
                                 max(unique_sites$latitude_y, #maximum latidude
                                     na.rm = T)+2), # order: xmin, xmax, ymin, ymax
                  "SpatialPolygons")

sp::proj4string(data_extent) <- sp::CRS(sp::proj4string(world)) # set the coordinate reference system of the data to be the same as the world map.

world_clip <- raster::intersect(world, data_extent) # select only those parts of the world map within our bounding box/extent 

world_clip_f <- fortify(world_clip) # transforms it into a data frame

# plot sites with editor names on top of world map
ggplot() +
  geom_polygon(data = world_clip_f, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA, 
               colour = "grey") +
  geom_point(data = unique_sites,
             aes(x = longitude_x,
                 y = latitude_y,
                 fill = editor_name),
             shape = 21,
             size = 4) +
  coord_fixed() +
  coord_quickmap() +  
  theme_classic() + 
  xlab("Longitude") +
  ylab("Latitude")  +
  labs(fill = "Expert Editor") + # capitalize 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  theme(legend.position = "bottom", # or "none"
        text = element_text(size=20),
        # panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", 
                                        size=1))
#######################










