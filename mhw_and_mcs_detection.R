# MHW and MCS Combination Detection
# 21/04/2022
# Jesse Phillips

# This script can be used to detect both MHW and MCS events
# Specific use: to determine the frequency distribution of days between MHW and MCS
# Use OISST data downloaded using OISST_preparation.R script


# Load Packages -----------------------------------------------------------

library(dplyr) # For basic data manipulation
library(ggplot2) # For visualising data
library(heatwaveR) # For detecting MHWs and MCSs
library(tidync) # For easily dealing with NetCDF data
library(doParallel) # For parallel processing


# Load Data ---------------------------------------------------------------

OISST <- readRDS("OISST_vignette.Rds")


# Detect MHWs -------------------------------------------------------------

# Initial wrapper function to combine calculations
mhw_event_only <- function(df){
  # First calculate the climatologies
  clim <- ts2clm(data = df, climatologyPeriod = c("1990-01-01", "2022-04-05"))
  # Then the events
  event <- detect_event(data = clim)
  # Return only the event metric dataframe of results
  return(event$event)
}

# The plyr technique
# NB: One should never use ALL available cores, save at least 1 for other essential tasks
# My laptop has 4 cores, so I use 3 here
registerDoParallel(cores = 3)
# Detect events
system.time(
  MHW_plyr <- plyr::ddply(.data = OISST, .variables = c("lon", "lat"), .fun = mhw_event_only, .parallel = TRUE)
)

# Save for later use as desired
saveRDS(MHW_plyr, "MHW_S_Africa_Apr_2022.Rds")


# Detect MCSs -------------------------------------------------------------

# Initial wrapper function to combine calculations
mcs_event_only <- function(df){
  # First calculate the climatologies
  clim <- ts2clm(data = df, climatologyPeriod = c("1990-01-01", "2022-04-05"), pctile = 10)
  # Then the events
  event <- detect_event(data = clim, coldSpells = T)
  # Return only the event metric dataframe of results
  return(event$event)
}

# The plyr technique
# NB: One should never use ALL available cores, save at least 1 for other essential tasks
# My laptop has 4 cores, so I use 3 here
registerDoParallel(cores = 3)
# Detect events
system.time(
  MCS_plyr <- plyr::ddply(.data = OISST, .variables = c("lon", "lat"), .fun = mcs_event_only, .parallel = TRUE)
)

# Save for later use as desired
saveRDS(MCS_plyr, "MCS_S_Africa_Apr_2022.Rds")


# Combine Dataframes ------------------------------------------------------

# Add a column to each to specify whether its a MHW or MCS
MHW_plyr <- MHW_plyr %>% 
  mutate("event_type" = "MHW")

MCS_plyr <- MCS_plyr %>% 
  mutate("event_type" = "MCS")

# Combine into one dataframe 
Combined_Events_df <- rbind(MHW_plyr, MCS_plyr)

# Select only the variables useful for our analysis
Combined_Events_sub <- Combined_Events_df %>% 
  select(lon, lat, event_no, index_start, index_end, duration, date_start, date_end, event_type)

# Save for later use as desired
saveRDS(Combined_Events_sub, "Combined_Events.Rds")


# Frequency Calculation ---------------------------------------------------

# Calculate duration of time between MHWs and MCSs
# Create frequency distribution to display this data

# Order the dataset
Combined_Events_sub <- Combined_Events_sub[order(Combined_Events_sub$lat, Combined_Events_sub$lon, Combined_Events_sub$date_start),]
 

# Calculate number of days between a MHW(MCS) and it's nearest MCS(MHW) at a set latitude and longitude

diff_days <- vector() # create empty vector to hold difference in days

# Create for loop for length of dataframe
# Make logical comparisons between points that have same lat and lon and different event types
# Calculate number of days in between and append it to vector
# Return difference in days vector

for(i in 1:nrow(Combined_Events_sub)) {
  if (Combined_Events_sub[i,1] == Combined_Events_sub[i+1,1] & Combined_Events_sub[i,2] == Combined_Events_sub[i+1,2] & Combined_Events_sub[i,9] != Combined_Events_sub[i+1,9]) {
    val <- difftime(Combined_Events_sub[i+1,7], Combined_Events_sub[i,8], units = c("days"))
    diff_days <- c(diff_days, val)  
  }
}


# Frequency Distribution --------------------------------------------------

diff_df <- as.data.frame(diff_days) # convert vector to dataframe 

diff_df <- diff_df %>% 
  filter(diff_days <= 15)

ggplot(data = diff_df, aes(x = diff_days)) +
  stat_count(col = "black", fill = "skyblue4") +
  #geom_histogram(aes(y = ..density..), bins = 15, col = "black", fill = "skyblue4") +
  theme_bw() + 
  labs(x  = "Number of days between MHW/MCS")



# Same Analysis, Just MHWs ------------------------------------------------

# Read MHW data
mhws <- readRDS("MHW_S_Africa_Apr_2022.Rds")

#Select only the variables useful for our analysis
mhws_sub <- mhws %>% 
  select(lon, lat, event_no, index_start, index_end, duration, date_start, date_end)

# Order the dataset
mhws_sub <- mhws_sub[order(mhws_sub$lat, mhws_sub$lon, mhws_sub$date_start),]

# Calculate number of days between MHWs 
mhws_diff_days <- vector() # create empty vector to hold difference in days

for(i in 1:nrow(mhws_sub)) {
  if (mhws_sub[i,1] == mhws_sub[i+1,1] & mhws_sub[i,2] == mhws_sub[i+1,2]) {
    val <- difftime(mhws_sub[i+1,7], mhws_sub[i,8], units = c("days"))
    mhws_diff_days <- c(mhws_diff_days, val)  
  }
}

# Plot
mhws_diff_df <- as.data.frame(mhws_diff_days) # convert vector to dataframe 

mhws_diff_df <- mhws_diff_df %>% 
  filter(mhws_diff_days <= 15)

ggplot(data = mhws_diff_df, aes(x = mhws_diff_days)) +
  stat_count(col = "black", fill = "darkred") +
  #geom_histogram(aes(y = ..density..), bins = 15, col = "black", fill = "skyblue4") +
  theme_bw() + 
  labs(x  = "Number of days between MHWs")



# Extremes ----------------------------------------------------------------

# Find how frequently a very extreme MHW/MCS occurs before another very extreme MCS/MHW 

# Load data
MHW_plyr <- readRDS("MHW_S_Africa_Apr_2022.Rds")

MCS_plyr <- readRDS("MCS_S_Africa_Apr_2022.Rds")


# Add a column to each to specify whether its a MHW or MCS
MHW_plyr <- MHW_plyr %>% 
  mutate("event_type" = "MHW")

MCS_plyr <- MCS_plyr %>% 
  mutate("event_type" = "MCS")

# Combine into one dataframe - Specific to studying the most extreme intensities
Intense_Events_df <- rbind(MHW_plyr, MCS_plyr)

# Select important variables
Intense_Events_sub <- Intense_Events_df %>% 
  select(lon, lat, duration, date_start, date_end, intensity_mean, intensity_max, intensity_var, intensity_cumulative, rate_onset, rate_decline, event_type)

# Save Rds
saveRDS(Intense_Events_sub, "Combined_Events_Intensity.Rds")


# Calculations
# Filter for events that are particularly intense (mean intensity > 2)
Intense_sub <- Intense_Events_sub %>% 
  filter(abs(intensity_mean) >= 2)

# Do that same frequency plots, with just these events

Intense_sub <- Intense_sub[order(Intense_sub$lat, Intense_sub$lon, Intense_sub$date_start),]

diff_days_intense <- vector() # create empty vector to hold difference in days

for(i in 1:nrow(Intense_sub)) {
  if (Intense_sub[i,1] == Intense_sub[i+1,1] & Intense_sub[i,2] == Intense_sub[i+1,2] & Intense_sub[i,12] != Intense_sub[i+1,12]) {
    val <- difftime(Intense_sub[i+1,4], Intense_sub[i,5], units = c("days"))
    diff_days_intense <- c(diff_days_intense, val)  
  }
}

diff_intense_df <- as.data.frame(diff_days_intense) # convert vector to dataframe 

diff_intense_df <- diff_intense_df %>% 
  filter(diff_days_intense <= 15)

ggplot(data = diff_intense_df, aes(x = diff_days_intense)) +
  stat_count(col = "black", fill = "darkgrey") +
  #geom_histogram(aes(y = ..density..), bins = 15, col = "black", fill = "skyblue4") +
  theme_bw() + 
  labs(x  = "Number of days between intense MHW/MCS")


# Plot frequency distribution with INTENSE overlay -------------------------

ggplot(data = diff_df, aes(x = diff_days)) +
  stat_count(col = "black", fill = "grey50") +
  #geom_histogram(aes(y = ..density..), bins = 15, col = "black", fill = "skyblue4") + 
  stat_count(data = diff_intense_df, aes(x = diff_days_intense), col = "black", fill = "grey25") +
  theme_bw() + 
  labs(x  = "Number of days between MHW/MCS")

