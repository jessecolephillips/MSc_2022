# Central script for functions of MEXTEF project
# 09/02/2023
# Jesse Phillips


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(heatwaveR)
library(doParallel)
library(ggpubr)
library(tidync)
library(data.table)


# Functions ---------------------------------------------------------------

# ts2clm() function from heatwaveR
  # Update function to accept first diffs of temp as input
  # Clim will therefor be based on change in temp instead of temp (for MHWs/MCSs)


# A function to compile the number of MEXTEFs occuring over a wide area (with the intention of making an atlas)
  # Use detect_event() on a pixel, then store number of observations
  # Do this for each pixel in an area, then you'll have 'frequency' of events over an area 

  # Data - plain Rds data [lon, lat, t, temp]
create_atlas <- function(data, n_diff = 1){
  # Calculate 1st/2nd/3rd/etc. differences
  sst_diff <- data |> mutate(diff = temp - lag(temp, n = n_diff))
  
  # Add 0 to rows with NA 
  
  
  # Calculate Climatologies
  clim_diff <- ts2clm(data = sst_diff, x = t, y = diff, climatologyPeriod = c("1991-01-02", "2022-12-31"), pctile = 10)
  
  # Detect Events
  event <- detect_event(data = clim_diff, x = t, y = diff, minDuration = n_diff, coldSpells = T)
  
  # Return df of events only
  return(event$event)
}


