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

# NOTE
  # - diff = h2c mextef
  # + diff = c2h mextef

  # Data - plain Rds data [lon, lat, t, temp]
detect_mextefs <- function(data, n_diff = 1){
  # Calculate 1st/2nd/3rd/etc. differences
  sst_diff <- data |> mutate(diff = temp - lag(temp, n = n_diff))
  
  # Add 0 to rows with NA 
  
  
  # Calculate Climatologies (both h2c and c2h)
  clim_diff_c2h <- ts2clm(data = sst_diff, x = t, y = diff, climatologyPeriod = c("1991-01-01", "2021-12-31"), pctile = 95)
  clim_diff_h2c <- ts2clm(data = sst_diff, x = t, y = diff, climatologyPeriod = c("1991-01-01", "2021-12-31"), pctile = 5)
  
  # Detect Events (both h2c and c2h)
  event_c2h <- detect_event(data = clim_diff_c2h, x = t, y = diff, minDuration = n_diff)
  event_h2c <- detect_event(data = clim_diff_h2c, x = t, y = diff, minDuration = n_diff, coldSpells = T)
  
  # Combine dfs of events only into a list
  events_list <- list("c2h_events" = event_c2h$event, "h2c_events" = event_h2c$event)
  
  # Return list
  return(events_list)
  rm(clim_diff_h2c, clim_diff_c2h, event_h2c, event_c2h)
}


