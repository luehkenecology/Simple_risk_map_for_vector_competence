# Set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load required libraries
library(terra)
library(plyr)
library(RcppRoll)

# Load temperature rasters for years 2020–2024
temperature_list <- list()
years <- 2020:2024

# Loop through each year to read temperature data
for(j in 1:length(years)){
  temperature_list[[j]] <- values(rast(paste("G:/NeuAll/00001-projects/00070-eobs/output/tg_ens_mean_0.1deg_reg_v31.0e_", years[j], ".tif", sep = "")))
  print(j)  # Print progress
}

# Reference raster for saving results
save_rast <- rast(paste("G:/NeuAll/00001-projects/00070-eobs/output/tg_ens_mean_0.1deg_reg_v31.0e_", 2024, ".tif", sep = ""))[[1]]

# Degree-Day Unit function: calculates number of days where rolling mean temp exceeds threshold
DDU <- function(x, eip, temperature){
  DDU <- roll_mean(x, n = eip, fill = 0, align = "right")  # Rolling average over EIP days
  days <- ifelse(DDU >= temperature, 1, 0)  # 1 if above threshold, else 0
  sum(days)  # Sum of development days
}

# Define thresholds and extrinsic incubation periods
v_temp <- c(15, 18, 21, 24, 27)  # Temperature thresholds
eip <- c(7, 14, 21)  # EIP durations in days

# Prepare containers for results
temp_list <- list()

# Loop over temperature thresholds
for(i in 1:length(v_temp)){
  temporary_list_temp <- list()
  
  # Loop over EIP durations
  for(j in 1:length(eip)){
    temporary_list_eip <- list()
    
    # Loop over years
    for(k in 1:length(temperature_list)){
      
      # Apply DDU function to each pixel time series
      res <- apply(temperature_list[[k]], 1, function(x) DDU(x, eip = eip[j], v_temp[i]))
      
      # Save result as raster
      temporary_list_eip[[k]] <- setValues(save_rast, res)
    }
    
    # Store EIP iteration result
    temporary_list_temp[[j]] <- temporary_list_eip
  }
  
  # Store temperature iteration result
  temp_list[[i]] <- temporary_list_temp
  print(i)  # Print progress
}

# Generate 1) mean rasters and 2) masked rasters based on presence
for(i in 1:length(v_temp)){
  for(j in 1:length(eip)){
    
    # Stack rasters and compute mean
    ne <- rast(lapply(temp_list[[i]], `[[`, j))
    mean_vals <- mean(ne, na.rm = TRUE)
    
    # Save results
    writeRaster(mean_vals, paste("output/ddu_", v_temp[i], "_", eip[j], ".tif", sep = ""), overwrite = TRUE)
  }
}