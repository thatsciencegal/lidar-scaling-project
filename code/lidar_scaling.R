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
collated.lidar.data <- data.frame(tile = character(),
                                  resolution = numeric(),
                                  max_height = numeric(),
                                  mean_height = numeric(),
                                  rugosity = numeric(),
                                  vertical_diversity = numeric(),
                                  openness = numeric())                                  )

for(i in seq_along(file.names)){
  # read in the current file
  start.time <- Sys.time()
  current.file <- file.names[i]
  print(paste0("Working on: ", current.file))
  lidar_in <- readLAS(current.file)
  
  for(j in length(raster_sizes)){
    # calculate the lidar metrics for each spatial resolution
    print(paste0("Working on grid size: ", raster_sizes[j])
    dtm = grid_terrain(lidar_in, res = 5, method = "knnidw")
    las_norm <- lasnormalize(lidar_in, dtm)
    las_norm@data$Z[las_norm@data$Z <0] <- 0
    normalized_metrics <- las_norm %>% grid_metrics(.stdmetrics, res = raster_sizes[j])
    plot(las_norm)
    
    # write out the raw data as a csv
    write.csv(normalized_metrics, out.path, file=paste(current.file,raster_sizes[j], sep = "_"))
    
    # add data to the analysis csv
    collated.lidar.data$tile[j] <- current.file
    collated.lidar.data$resolution[j] <- raster_sizes[j]
    collated.lidar.data$max_height[j] <- mean(normalized_metrics$MAXHEIGHTGOESHERE)
    collated.lidar.data$mean_height[j] <- mean(normalized_metrics$MEANHEIGHTGOESHERE)
    collated.lidar.data$rugosity[j] <- mean(normalized_metrics$RUGOSITYGOESHERE)
    collated.lidar.data$vertical_diversity[j] <- mean(normalized_metrics$VERTICALDIVERSITYGOESHERE)
    collated.lidar.data$openness[j] <- mean(normalized_metrics$OPENNESSGOESHERE)
  }
  
  # get total run time for each iteration
  end.time <- Sys.time()
  run.time <- end.time-start.time
  print(paste0("Total run time: ", run.time))
  
}

write.csv(collated.lidar.data, out.path, file = "collated.lidar.metrics")