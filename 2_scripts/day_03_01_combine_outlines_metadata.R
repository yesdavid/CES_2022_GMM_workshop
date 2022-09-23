### Day 3
#### Step-by-step analysis of data
#### 01 combine outlines with meta data

# your name/the name you gave to your data folder
# EDITORNAME <- "CNicolas2016" # for example
EDITORNAME <- "editor_name"

#############################################
# set/create output folder + path

output_folder <- file.path(".", 
                           "3_output", 
                           EDITORNAME)
dir.create(output_folder,
           recursive = T)


# set input folder
input_folder <- file.path(".",
                          "1_data",
                          EDITORNAME)


#############################################
# DATA 

# load outlines
outlines <- readRDS(file = file.path(input_folder, 
                                     paste0("outlines_combined_", EDITORNAME, ".RDS")))

## centre, scale, define first coordinate
outlines_centered <- Momocs::coo_centre(outlines) # Returns a shape centered on the origin.
outlines_centered_scaled <- Momocs::coo_scale(outlines_centered) # Scales the coordinates by the centroid size.
outlines_centered_scaled <- Momocs::coo_slidedirection(outlines_centered_scaled, 
                                                       direction = "up") # Sets the "first" coordinate to be on the top.
## inspect artefact shapes
outlines_centered_scaled
names(outlines_centered_scaled)

Momocs::panel(outlines_centered_scaled) # shows all shapes side-by-side
stack(outlines_centered_scaled) # stacks all shapes on top of each other
# Momocs::inspect(outlines_centered_scaled) # shows each single shape individually. Press ESCAPE to quit this mode.

## remove fragmented outliers by their names
# outlines_centered_scaled <- Momocs::filter(outlines_centered_scaled, 
#                                            !ID_artefact %in% c("UK_60_XX_pseudo_no_10", 
#                                                                "UK_15_XX_pseudo_no_4")) 


# load meta data
outline_metadata <- readr::read_csv(file = file.path(input_folder, 
                                                     paste0("outline_metadata_", EDITORNAME, ".csv")))
## inspect meta data 
head(outline_metadata)


# combine meta data with outlines
## the name of each outline is the "image_ID" from the outline_metadata file with a "pseudo number" attached by the outlineR package
## in order to combine both data with another, they have to have the same IDs. 
## Therefore, the outlines data has to be modified
outlines_names <- names(outlines_centered_scaled)
outlines_names_split <- strsplit(outlines_names, split = "_")

ID_and_artefact_ID_list <- list()
for (name_index in 1:length(outlines_names)){
  
  ID_and_artefact_ID_interrim_df <- data.frame(image_ID = paste0(outlines_names_split[[name_index]][1], "_", outlines_names_split[[name_index]][2]),
                                               artefact_ID = outlines_names[[name_index]])
  ID_and_artefact_ID_list[[name_index]] <- ID_and_artefact_ID_interrim_df
  
}
image_ID_and_artefact_ID_df <- do.call("rbind", ID_and_artefact_ID_list)


# join image_ID_and_artefact_ID_df with outline_metadata to get the artefact_ID as a column in the meta data.
outline_metadata_artefact_ID <- dplyr::left_join(image_ID_and_artefact_ID_df, 
                                                 outline_metadata, 
                                                  by = "image_ID")

# attach the meta data to the outlines
outlines_centered_scaled_meta_data <- Momocs::Out(outlines_centered_scaled$coo, 
                                                              fac = outline_metadata_artefact_ID)

# optional procedures
## extract certain artefacts
# outlines_centered_scaled_meta_data_DK <- Momocs::filter(outlines_centered_scaled_meta_data, 
#                                                         country %in% c("Denmark"))


# save the outlines + meta data file
saveRDS(outlines_centered_scaled_meta_data,
        file = file.path(output_folder, paste0("outlines_centered_scaled_meta_data_", EDITORNAME, ".RDS")))
















