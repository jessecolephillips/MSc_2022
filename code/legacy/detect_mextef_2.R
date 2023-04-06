# Detect mextef with lag function
# 17/02/2023
# Jesse Phillips 


# Load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(heatwaveR)
library(profvis) # For profiling visualisations

# Load Data ---------------------------------------------------------------

data <- readRDS("OISST_False_Bay_March_2021.Rds")

# Use heatwaveR data
data <- heatwaveR::sst_WA


# Algorithm time ----------------------------------------------------------

# calculate first differences for the 'climatology'
sst_diff <- data %>% 
  mutate(diff = temp - lag(temp, n = 1))

# Plot the daily change at a certain pixel
sst_diff %>% 
  #filter(lon == 17.625 & lat == -34.875) %>% 
  ggplot(aes(x = t, y = diff)) + 
  geom_point()

# Now lets calculate a climatology 
  # Select pixel
sst_diff_clim <- sst_diff %>% 
  #filter(lon == 17.625 & lat == -34.875) %>% 
  ts2clm(x = t, y = diff, climatologyPeriod = c("1982-01-02", "2013-12-31"))

  # and plot
ggplot(sst_diff_clim, aes(x = doy)) +
  geom_line(aes(y = seas)) + 
  geom_line(aes(y = thresh), colour = "indianred")

# Use detect_event() on this new climatology
sst_diff_events <- detect_event(sst_diff_clim, x = t, y = diff, minDuration = 3)
    # 34 events found using 3 day minimum duration 

# Events between MHWs and MCSs --------------------------------------------

# Let's see if we can detect events that occur between MHWs and MCSs using
  # some old Combined Events data

# Read in data
Combined_Events_Intensity <- readRDS("data/Combined_Events_Intensity.Rds")

# Filter for instances - at the same location - where there is fewer than
  # 3(?) days between a MHW and MCS

mextefs_between_events <- Combined_Events_Intensity %>% 
  group_by(lon, lat) %>% 
  filter(event_type != lead(event_type))
# lat and lon to be same
# date_start and lag(date_end) to be < 3
# event_type to be different

# Profiling ---------------------------------------------------------------

# Let's see where the code spends its time
  # Using heatwaveR data
profvis(detect_event(ts2clm(sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"))))

