# Set the working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load required libraries
library(plyr)
library(scales)
require(lubridate)
library(zoo)
require(fields)
library(raster)
library(spatstat) 
library(RNetCDF)
library(fields)
library(colorRamps) 
library(rworldmap)
library(ncdf4)

# Function to convert NetCDF file to raster
nc_to_raster <- function(nc_file, start_dataset = '1950-01-01',
                         year_start, day_start = "-01-01",
                         year_end = year_start, day_end = "-12-31",
                         crop_extent = 0,
                         variable = "tg"){
  
  # Set time range
  start_date <- paste(year_start, day_start, sep = "")
  end_date <- paste(year_end, day_end, sep = "")
  time_start <- as.POSIXct(start_date, tz='UTC')
  time_end <- as.POSIXct(end_date, tz='UTC')
  time_sequence <- seq(time_start, time_end, by='24 hours')
  
  # Extract time values from NetCDF file
  nc_times <- as.POSIXct(nc_file$dim$time$vals*86400, origin=start_dataset, tz='UTC')
  start_index <- which(nc_times == time_start)
  end_index <- which(nc_times == time_end)
  time_range <- end_index - start_index + 1
  
  # Extract data from NetCDF file
  data_array <- ncvar_get(nc_file, variable, start=c(1, 1, start_index), count=c(-1, -1, time_range))
  
  # Get longitude and latitude values
  longitudes <- nc_file$dim$longitude$vals
  latitudes <- nc_file$dim$latitude$vals
  
  # Create raster layers
  raster_layers <- lapply(1:time_range, function(x) {
    layer_data <- t((data_array[,,x]))
    layer_data_flipped <- layer_data[nrow(layer_data):1,]
    raster(layer_data_flipped, xmn=min(longitudes), xmx=max(longitudes), ymn=min(latitudes), ymx=max(latitudes))
  })
  
  # Crop raster if extent is provided
  if(sum(crop_extent) > 0 | sum(crop_extent) < 0){
    cropped_layers <- lapply(raster_layers, function(x) crop(x, crop_extent))
    return(brick(unlist(cropped_layers)))
  } else {
    return(brick(unlist(raster_layers)))
  }
}

# Convert NetCDF to raster for multiple years
for(year in 2020:2024){
  # Convert NetCDF to raster
  raster_data <- nc_to_raster(nc_open("data/tg_ens_mean_0.1deg_reg_v31.0e.nc"),
                              year_start = year, variable = "tg")
  
  # Save raster
  output_filename <- paste("output/tg_ens_mean_0.1deg_reg_v31.0e_", year, ".tif", sep = "")
  writeRaster(raster_data, output_filename, overwrite = TRUE)
  
  print(year)
}

data <- temp_func(nc_open("data/tg_0.1deg_day_2021_grid_ensmean.nc"),
                  year_start = 2021, var = "tg")