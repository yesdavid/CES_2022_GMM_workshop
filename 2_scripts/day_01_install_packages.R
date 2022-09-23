### Day 1
#### 01 install necessary packages

#############################################
# install/load packages
packages <- c("Momocs",
              "ggplot2",
              "cluster",
              "NbClust",
              "fpc",
              "readr",
              "dplyr",
              "maptree",
              # "ggimage",
              "phangorn",
              "rworldmap",
              "raster",
              "rgeos",
              "dispRity")

for (p in packages) {
  if (!(p %in% rownames(installed.packages()))) {
    install.packages(p)
  }
}


# install ggtree, which is dependent on BiocManager
if (!requireNamespace("ggtree", quietly = TRUE)){
  if (!requireNamespace("BiocManager", quietly = TRUE)){
    install.packages("BiocManager")
    BiocManager::install("ggtree")
  } else {
    BiocManager::install("ggtree")
  }
}

#############################################