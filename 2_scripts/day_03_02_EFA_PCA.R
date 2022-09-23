### Day 3
#### Step-by-step analysis of data
#### 02 Elliptic Fourier Analysis (EFA) + Principal Components Analysis (PCA)

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
# load the outlines + meta data file
outlines_centered_scaled_meta_data <- readRDS(file = file.path(output_folder, paste0("outlines_centered_scaled_meta_data_", EDITORNAME, ".RDS")))

#############################################

# harmonic calibration
## Estimates the number of harmonics required for the Fourier methods implemented in Momocs.
outlines_centered_scaled_meta_data_harmonics <- Momocs::calibrate_harmonicpower_efourier(outlines_centered_scaled_meta_data, 
                                                                                                     plot = F)  
outlines_centered_scaled_meta_data_harmonics$minh

# Elliptic Fourier Analysis (EFA)
outlines_centered_scaled_meta_data_efourier <- Momocs::efourier(outlines_centered_scaled_meta_data,
                                                                            nb.h = as.matrix(outlines_centered_scaled_meta_data_harmonics[["minh"]])[[4,1]], # 4: harmonics for 99.9%
                                                                            norm = F) 

# Principal Components Analysis (PCA) on the EFA
outlines_centered_scaled_meta_data_PCA <- Momocs::PCA(outlines_centered_scaled_meta_data_efourier) # PCA on Coe objects, using prcomp.

outlines_centered_scaled_meta_data_PCA$x # where the PCA results are stored
ncol(outlines_centered_scaled_meta_data_PCA$x) # number of PC axes retrieved

## Test how many PC axes describe `proportion_of_variance`% of the variance.
proportion_of_variance <- 0.95

minimum_no_of_pcs <- Momocs::scree_min(outlines_centered_scaled_meta_data_PCA,
                                               prop = proportion_of_variance) 
minimum_no_of_pcs # minimum number of axis to use to retain a given proportion (i.e. prop = 0.99 to describe 99% of the variation)

Momocs::scree_plot(outlines_centered_scaled_meta_data_PCA) # visualise the proportion of variance captured by each PC axis.

## check which PC axis represents what part of the shapespace
Momocs::PCcontrib(outlines_centered_scaled_meta_data_PCA,
                                nax = 1:5,
                                sd.r = c(-2,-1,0,1,2))




## plot PCA 

###the old school-way

# # Create groups; if applicable, remove # in front of the code below and modify accordingly
# pch.group <- c(rep(21, times=length(which(outlines_centered_scaled_meta_data_PCA$fac$country == "Denmark"))),
#                rep(22, times=length(which(outlines_centered_scaled_meta_data_PCA$fac$country == "France"))),
#                rep(23, times=length(which(outlines_centered_scaled_meta_data_PCA$fac$country == "United Kingdom"))))
# col.group <- c(rep("skyblue2", times=length(which(outlines_centered_scaled_meta_data_PCA$fac$country == "Denmark"))),
#                rep("gold", times=length(which(outlines_centered_scaled_meta_data_PCA$fac$country == "France"))),
#                rep("green2", times=length(which(outlines_centered_scaled_meta_data_PCA$fac$country == "United Kingdom"))))

plot(outlines_centered_scaled_meta_data_PCA$x[,1],
     outlines_centered_scaled_meta_data_PCA$x[,2],
     xlab=paste("PC 1 (", round(summary(outlines_centered_scaled_meta_data_PCA)$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PC 2 (", round(summary(outlines_centered_scaled_meta_data_PCA)$importance[5]*100, 1), "%)", sep = ""),
     # pch=pch.group, # shape of the data points, here: grouped by country
     col="black",
     # bg=col.group, # color of the data points, here: grouped by country
     cex=1,
     las=1,
     asp=1,
     main = "PCA of Bell Beaker arrowhead outline shapes (data from Nicolas outlines)")

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")

# # Add legend; if applicable
# legend("topright",
#        legend=c("Denmark", "France", "United Kingdom"),
#        col="black",
#        pt.bg=c("skyblue2", "gold", "green2"),
#        pch=c(21, 22, 24),
#        pt.cex=1.5)


### the ggplot-way
library(ggplot2)

# create a data frame with the meta data
meta_data <- outlines_centered_scaled_meta_data_PCA$fac

# create a data frame with the relevant PC axes
PCA_data <- as.data.frame(outlines_centered_scaled_meta_data_PCA$x[,1:minimum_no_of_pcs])
PCA_data$artefact_ID <- rownames(PCA_data) # there, create a column which contains the names of the artefacts/artefact_ID to be abel to merge both files.

meta_data_PCA <- dplyr::left_join(meta_data,
                                  PCA_data)

# plot!
# just the points
ggplot(data = meta_data_PCA) +
  geom_point(aes(x = PC1,
                 y = PC2)) +
  theme_bw()

# color and shape grouped by country
ggplot(data = meta_data_PCA) +
  geom_point(aes(x = PC1,
                 y = PC2,
                 shape  = country,
                 color = country)) +
  theme_bw()

# color = archaeological period, shape = country
ggplot(data = meta_data_PCA) +
  geom_point(aes(x = PC1,
                 y = PC2,
                 shape  = country,
                 color = archaeological_period)) +
  theme_bw()


# save data
saveRDS(outlines_centered_scaled_meta_data_PCA,
        file = file.path(output_folder, paste0("outlines_centered_scaled_meta_data_PCA_", EDITORNAME, ".RDS")))


# if there are outliers in the plot, identify them
## using density based clustering
outliers_db <- fpc::dbscan(outlines_centered_scaled_meta_data_PCA$x, 
                           eps = 0.3, 
                           MinPts = 3)
plot(outliers_db, outlines_centered_scaled_meta_data_PCA$x, main = "DBSCAN", frame = FALSE)

outliers_cluster <- data.frame(name = row.names(outlines_centered_scaled_meta_data_PCA$x), 
                                       value = outliers_db$cluster, 
                                       row.names = NULL)

outliers_cluster_outlier_names <- subset(outliers_cluster, value != 1) # which outlines are NOT in cluster 1?

# outlines_centered_scaled_meta_data_PCA$fac$outlier_names <- NA
# outlines_centered_scaled_meta_data$fac$cluster <- as.factor(outliers_cluster$value)

# for (vector_index in 1:length(match(outliers_cluster_outlier_names$name, outlines_centered_scaled_meta_data_PCA$fac$artefact_ID))){
#   
#   current_index <- match(outliers_cluster_outlier_names$name, outlines_centered_scaled_meta_data_PCA$fac$artefact_ID)[vector_index]
#   
#   outlines_centered_scaled_meta_data_PCA$fac$outlier_names[current_index] <- outliers_cluster_outlier_names$name[vector_index]
# }

# cut out the outliers
outlines_with_outliers <- Momocs::slice(outlines_centered_scaled_meta_data, 
                                            match(outliers_cluster_outlier_names$name, 
                                                  outlines_centered_scaled_meta_data_PCA$fac$artefact_ID))
# look at the shape of the outliers, are they "legitimate" outliers (i.e., broken, etc.)?
Momocs::panel(outlines_with_outliers,
              fac = "country_code",
              names = T,
              col = "grey")


############################
# if necessary, remove these outliers from the whole data set and repeat
outlines_centered_scaled_meta_data_wo_outliers <- Momocs::slice(outlines_centered_scaled_meta_data, 
                                                                -match(outliers_cluster_outlier_names$name, # or specify names manually!
                                                                       outlines_centered_scaled_meta_data_PCA$fac$artefact_ID))

# harmonic calibration
outlines_centered_scaled_meta_data_wo_outliers_harmonics <- Momocs::calibrate_harmonicpower_efourier(outlines_centered_scaled_meta_data_wo_outliers, 
                                                                                                     plot = F)  # Estimates the number of harmonics required for the Fourier methods implemented in Momocs. This is the only step in this section that produces data we need in the subsequent step.

# EFA
outlines_centered_scaled_meta_data_wo_outliers_efourier <- Momocs::efourier(outlines_centered_scaled_meta_data_wo_outliers,
                                                                            nb.h = as.matrix(outlines_centered_scaled_meta_data_wo_outliers_harmonics[["minh"]])[[4,1]], # harmonics for 99.9%
                                                                            norm = F) 
# PCA
outlines_centered_scaled_meta_data_wo_outliers_PCA <- Momocs::PCA(outlines_centered_scaled_meta_data_wo_outliers_efourier) # PCA on Coe objects, using prcomp.

minimum_no_of_pcs_wo_outliers <- Momocs::scree_min(outlines_centered_scaled_meta_data_wo_outliers_PCA,
                                                   prop = proportion_of_variance) 
minimum_no_of_pcs_wo_outliers # minimum number of axis to use to retain a given proportion (i.e. prop = 0.99 to describe 99% of the variation)

Momocs::scree_plot(outlines_centered_scaled_meta_data_wo_outliers_PCA) # visualise the proportion of variance captured by each PC axis.

## check which PC axis represents what part of the shapespace
Momocs::PCcontrib(outlines_centered_scaled_meta_data_wo_outliers_PCA,
                  nax = 1:5,
                  sd.r = c(-2,-1,0,1,2))
## plot
# create a data frame with the meta data
meta_data_wo_outlier <- outlines_centered_scaled_meta_data_wo_outliers_PCA$fac

# create a data frame with the relevant PC axes
PCA_data_wo_outlier <- as.data.frame(outlines_centered_scaled_meta_data_wo_outliers_PCA$x[,1:minimum_no_of_pcs_wo_outliers])
PCA_data_wo_outlier$artefact_ID <- rownames(PCA_data_wo_outlier) # there, create a column which contains the names of the artefacts/artefact_ID to be abel to merge both files.

meta_data_PCA_wo_outlier <- dplyr::left_join(meta_data_wo_outlier,
                                  PCA_data_wo_outlier)

ggplot(data = meta_data_PCA_wo_outlier) +
  geom_point(aes(x = PC1,
                 y = PC2)) +
  theme_bw()

# color and shape grouped by country
ggplot(data = meta_data_PCA_wo_outlier) +
  geom_point(aes(x = PC1,
                 y = PC2,
                 shape  = country,
                 color = country)) +
  theme_bw()

# color = archaeological period, shape = country
ggplot(data = meta_data_PCA_wo_outlier) +
  geom_point(aes(x = PC1,
                 y = PC2,
                 shape  = country,
                 color = archaeological_period)) +
  theme_bw()

# save PCA data without outliers!
saveRDS(outlines_centered_scaled_meta_data_wo_outliers_PCA,
        file = file.path(output_folder, paste0("outlines_centered_scaled_meta_data_wo_outliers_PCA_", EDITORNAME, ".RDS")))
# save outlines without outliers!
saveRDS(outlines_centered_scaled_meta_data_wo_outliers,
        file = file.path(output_folder, paste0("outlines_centered_scaled_meta_data_wo_outliers_", EDITORNAME, ".RDS")))
