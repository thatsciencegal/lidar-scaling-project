## Size matters: scaling effects on LiDAR interpretation in a neotropical rainforest
## Code written by Christine Swanson
## June 28, 2017

# load appropriate libraries
library(lidR)
library(ggplot2)
library(dplyr)

# set data paths
in.path = "../Data/raw_las"
out.path <- "../Data/output"

# list all of the lidar files to be used
file.names <- dir(in.path, pattern = ".las", full.names = TRUE)

# vector of grid sizes for lidar metrics analysis
raster_sizes <- c(1, 5, 10, 15, 30, 60, 120, 250, 500, 1000)

# empty data frame to store means of the lidar data
lidar.data.matrix <- matrix(ncol = 7, nrow = 600)

lidar.data.names <- c(tile, resolution, max_height, mean_height, rugosity, vertical_diversity, openness)                                  

for(i in seq_along(file.names)){
  # read in the current file
  start.time <- Sys.time()
  current.file <- file.names[i]
  print(paste0("Working on: ", current.file))
  lidar_in <- readLAS(current.file)
  
  for(j in length(raster_sizes)){
    # calculate the lidar metrics for each spatial resolution
    print(paste0("Working on grid size: ", raster_sizes[j]))
    dtm = grid_terrain(lidar_in, res = 5, method = "knnidw")
    las_norm <- lasnormalize(lidar_in, dtm)
    las_norm@data$Z[las_norm@data$Z <0] <- 0
    normalized_metrics <- las_norm %>% grid_metrics(.stdmetrics, res = raster_sizes[j])
    plot(las_norm)
    
    # write out the raw data as a csv
    write.csv(normalized_metrics, out.path, file=paste(current.file,raster_sizes[j], sep = "_"))
    
    # add data to the analysis csv
    lidar.data.matrix[j,1] <- current.file
    lidar.data.matrix[j,2] <- raster_sizes[j]
    lidar.data.matrix[j,3] <- mean(normalized_metrics$zmax, na.rm=T)
    lidar.data.matrix[j,4] <- mean(normalized_metrics$zmean, na.rm=T)
    lidar.data.matrix[j,5] <- sd(normalized_metrics$zmax, na.rm=T)
    lidar.data.matrix[j,6] <- mean(normalized_metrics$zentropy, na.rm=T)
    lidar.data.matrix[j,7] <- mean(normalized_metrics$pground, na.rm=T)
  }
  
  # get total run time for each iteration
  end.time <- Sys.time()
  run.time <- end.time-start.time
  print(paste0("Total run time: ", run.time))
  
}

#Coerce the matrix into a data frame
collated.lidar.data <- data.frame(lidar.data.matrix, row.names = lidar.data.names)
write.csv(collated.lidar.data, out.path, file = "collated.lidar.metrics")