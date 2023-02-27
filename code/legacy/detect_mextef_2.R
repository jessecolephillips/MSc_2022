# Detect mextef with lag function
# 17/02/2023
# Jesse Phillips 


# Load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(heatwaveR)

# Load Data ---------------------------------------------------------------

data <- readRDS("OISST_False_Bay_March_2021.Rds")


# Algorithm time ----------------------------------------------------------

# calculate first differences for the 'climatology'
sst_diff <- data %>% 
  group_by(lon, lat) %>% 
  mutate(diff = temp - lag(temp, n = 1))

# Plot the daily change at a certain pixel
sst_diff %>% 
  filter(lon == 17.625 & lat == -34.875) %>% 
  ggplot(aes(x = t, y = diff)) + 
  geom_line()

# Now lets calculate a climatology 
  # Select pixel
sst_diff_clim_1p <- sst_diff %>% 
  filter(lon == 17.625 & lat == -34.875) %>% 
  ts2clm(x = t, y = diff, climatologyPeriod = c("1991-01-01", "2021-12-31"), clmOnly = T)

  # and plot
ggplot(sst_diff_clim_1p, aes(x = doy)) +
  geom_line(aes(y = seas)) + 
  geom_line(aes(y = thresh), colour = "indianred")

