# Detecting MEXTEFs
# 15/06/2022
# Jesse Phillips
# The function that will take a temperature time series and detect MEXTEFs


# Library -----------------------------------------------------------------

library(dplyr)
library(heatwaveR)
library(lubridate)
library(ggplot2)
library(tibbletime)
library(tidyverse)
library(broom)
library(purrr)
library(RcppRoll)
library(ggpubr)
library(NCmisc) #pctile()

# Parrelel CPU usage


# Load Data ---------------------------------------------------------------

NW_Atl <- heatwaveR::sst_NW_Atl # eg. dataset from Northwest Atlantic 
WA <- heatwaveR::sst_WA # Western Australia
Med <- heatwaveR::sst_Med # Mediterranean


# Create Climatology ------------------------------------------------------

create_clim <- function(data, slidingWindow = 5){
  # ADD:
    # Climatology period
  
  # Create rolling function of linear regressions across sliding window period
  lm_roll <- rollify(~lm(.x ~ .y), window = slidingWindow, unlist = F)
  
  data_lm <- data %>% 
    mutate(rate_lm = lm_roll(temp, t)) # Creates list-column of linear model 
  
  # Pull just slope (rate) from linear models
  delta_temp_df <- data_lm %>% 
    slice(slidingWindow:nrow(data_lm)) %>% 
    mutate(delta_temp_lst = map(rate_lm, "coefficients")) %>% 
    mutate(rate = map_dbl(delta_temp_lst, ".y")) %>% 
    select(t, temp, rate) 
  
  # Create df with day of year col
  ts_doy <- delta_temp_df %>% 
    mutate(doy = yday(t))
  
  rm(data_lm, delta_temp_df) # remove excess
  
  # Group df by doy, then calc mean delta_temp for each doy (this forms "climatology")
  clim <- ts_doy %>% 
    group_by(doy) %>% 
    mutate(mean_rate = mean(rate, na.rm = T),
           sd_rate = sd(rate, na.rm = T))
  
  rm(ts_doy)
  
  # Create threshold (90 pctile) col for extreme delta_temps
  clim_thresh <- clim %>% 
    group_by(doy) %>% 
    mutate(ninety_pc = mean_rate + (1.282*sd_rate),
           ten_pc = mean_rate - (1.282*sd_rate))
  
  # NB - function pctile() -> on time series (NOT mean) 
  
  rm(clim)
  
  # Add boolean col for when delta_temp exceeds thresh
  clim_final <- clim_thresh %>% 
    mutate(thresh_exceed = dplyr::if_else(rate < ten_pc | rate > ninety_pc, T, F)) 
  
  rm(clim_thresh)
    
  return(clim_final)
}

NW_Atl_clim <- create_clim(NW_Atl, slidingWindow = 5)

W_Aus_clim <- create_clim(WA, slidingWindow = 5)

Med_clim <- create_clim(Med, slidingWindow = 5)


# Detect Event ------------------------------------------------------------

detect_mextef <- function(data){
  # filter rows where thresh.exceed = T
  
  # Calculate:
    # number of days of event
    # rate of temperature change between min and max
    # amount of temperature change between min and max
    # + any other important metrics
  
  # Categorize 
    # by rate: 90th, 95th or 99th pctile exceeded
    # type? (visual cue?)
  
  # Data frame:
    # Event number
    # start and end date
    # peak date? day of highest delta_temp
 }



# Testing -----------------------------------------------------------------

# Use all 3 datasets
data <- heatwaveR::sst_NW_Atl

# Set up data first to not group

# Create rolling function of linear regressions across sliding window period
lm_roll <- rollify(~lm(.x ~ .y), window = 5, unlist = F)

data_lm <- data %>% 
  mutate(rate_lm = lm_roll(temp, t)) # Creates list-column of linear model 

# data_lm$rate_lm[[5]]$coefficients[2] # This is the direct index for the slope (rate)

delta_temp_df <- data_lm %>% 
  slice(5:nrow(data_lm)) %>% 
  mutate(delta_temp_lst = map(rate_lm, "coefficients")) %>% 
  mutate(rate = map_dbl(delta_temp_lst, ".y")) %>% 
  select(t, temp, rate)


# Create df with day of year col
ts_doy <- delta_temp_df %>% 
  mutate(doy = yday(t))

rm(data_lm, delta_temp_df) # remove excess


# smoothing using RcppRoll
smooth_rate <- RcppRoll::roll_mean(as.numeric(unlist(ts_doy[,3])), n = 31, na.rm = F)
# The rate is used directly to calculated a 31-day smoothed mean
# This smoothes the rate, to use in a mean calc


# Group df by doy, then calc mean delta_temp + thresholds for each doy (this forms "climatology")
clim <- ts_doy %>% 
  group_by(doy) %>% 
  mutate(mean_rate = mean(rate, na.rm = T), 
         thresh_top = pctile(rate, pc = 0.9)[1], 
         thresh_bottom = pctile(rate, pc = 0.9)[2])


rm(ts_doy)



# Smoothing of mean and threshold rates
# Wrap clim with a month before and after (Smoothing window is 31 days by default)
# Create rolling mean function
clim_smooth <- rbind(tail(clim_thresh, 31),
                     clim_thresh,
                     head(clim_thresh, 31))

# Rolling 31 day mean
rolling_mean <- rollify(mean, window = 5, unlist = T)

clim_smooth <- clim_smooth %>% 
 mutate(mean_smooth = rolling_mean(clim_smooth[,5]))


rm(clim_thresh)

# Add boolean col for when delta_temp exceeds thresh
clim_final <- clim_thresh %>% 
  mutate(thresh_exceed = dplyr::if_else(rate < ten_pc | rate > ninety_pc, T, F)) 

rm(clim_thresh)


# DETECT MEXTEF

# INPUT -> "clim" object from create_clim() function
data <- NW_Atl_clim

# Use rle() function to tally consecutive days with TRUE values under thresh.exceed
ex1 <- rle(data[,9])
ex1

# Rearrange df where there is 1 event per row (with cols for start and end date)
  # Cols: event_no, start_date, end_date, delta_temp, max_T, min_T, intensity

# rle() 
  # checks series of true/false values
  # in heatwaveR code (hidden function)


# Calculate:
# number of days of event
# rate of temperature change between min and max temp
# amount of temperature change between min and max
# + any other important metrics

# Categorize 
# by rate: 90th, 95th or 99th pctile exceeded
# type? (temp - around extreme rate - above 90 and/or below 10)

# Data frame:
# Event number
# start and end date
# peak date? day of highest delta_temp



# Create visualisations!
ggplot(clim_final, aes(x = doy, y = ninety_pc)) +
  geom_line()

ggplot(clim_final, aes(x = doy, y = ten_pc)) +
  geom_line()

ggplot(clim_final, aes(x = doy, y = mean_rate)) +
  geom_line()

ggplot(clim, aes(x = doy, y = rate)) +
  geom_line() + 
  geom_smooth(method = "loess") +
  labs(y = "Rate of Temperature Change (°C/day)", x = "Day of Year")

# One complete plot to display everything 

ggplot(clim) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_smooth(aes(x = doy, y = rate), method = "gam") +
  geom_line(aes(x = doy, y = mean_rate), col = "black", size = 1.1) +
  geom_line(aes(x = doy, y = thresh_top), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = thresh_bottom), col = "blue", size = 1.1) + 
  labs(y = "Rate of Temperature Change (°C/day)", x = "Day of Year", title = "Northwest Atlantic") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 


# Same plot for W Aus
ggplot(W_Aus_clim) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_smooth(aes(x = doy, y = rate), method = "gam") +
  geom_line(aes(x = doy, y = mean_rate), col = "black", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  labs(y = "Rate of Temperature Change (°C/day)", x = "Day of Year", title = "Western Australia") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 


# Same plot for Med
ggplot(Med_clim) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_smooth(aes(x = doy, y = rate), method = "gam") +
  geom_line(aes(x = doy, y = mean_rate), col = "black", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  labs(y = "Rate of Temperature Change (°C/day)", x = "Day of Year", title = "Mediterranean") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 


# Smoothing for 90th and 10th pctiles
ggplot(Med_clim) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_line(aes(x = doy, y = mean_rate), col = "grey40", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  geom_smooth(aes(x = doy, y = rate), method = "gam", col = "white", size = 0.6) +
  geom_smooth(aes(x = doy, y = ninety_pc), method = "gam", col = "yellow", size = 0.6) +
  geom_smooth(aes(x = doy, y = ten_pc), method = "gam", col = "yellow", size = 0.6) +
  labs(y = "Rate of Temperature Change (°C/day)", x = "Day of Year", title = "Mediterranean") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 

# Smoothing
# two pass (heatwaveR code)



# Number of Events --------------------------------------------------------

# Number of events detected for each of the 3 levels of window size
Atl_events_3 <- NW_Atl_clim_3 %>% 
  filter(thresh_exceed == T) # 2478

Atl_events_5 <- NW_Atl_clim_5 %>% 
  filter(thresh_exceed == T) # 2638

Atl_events_7 <- NW_Atl_clim_7 %>% 
  filter(thresh_exceed == T) # 2676

Aus_events_3 <- W_Aus_clim_3 %>% 
  filter(thresh_exceed == T) # 2547

Aus_events_5 <- W_Aus_clim_5 %>% 
  filter(thresh_exceed == T) # 2626

Aus_events_7 <- W_Aus_clim_7 %>% 
  filter(thresh_exceed == T) # 2717

Med_events_3 <- Med_clim_3 %>% 
  filter(thresh_exceed == T) # 2451

Med_events_5 <- Med_clim_5 %>% 
  filter(thresh_exceed == T) # 2580

Med_events_7 <- Med_clim_7 %>% 
  filter(thresh_exceed == T) # 2674

# ARRANGE PLOTS -----------------------------------------------------------

# 3x3 square of plots displaying
  # The 3 data sets along with
  # The 3 levels of window width (3, 5 and 7 days)

NW_3_plt <- ggplot(NW_Atl_clim_3) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_line(aes(x = doy, y = mean_rate), col = "grey40", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  geom_smooth(aes(x = doy, y = rate), method = "gam", col = "white", size = 0.6) +
  geom_smooth(aes(x = doy, y = ninety_pc), method = "gam", col = "yellow", size = 0.6) +
  geom_smooth(aes(x = doy, y = ten_pc), method = "gam", col = "yellow", size = 0.6) +
  labs(y = "Rate (°C/day)", x = "Day of Year", title = "Northwest Atlantic [3 day window]") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 

NW_5_plt <- ggplot(NW_Atl_clim) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_line(aes(x = doy, y = mean_rate), col = "grey40", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  geom_smooth(aes(x = doy, y = rate), method = "gam", col = "white", size = 0.6) +
  geom_smooth(aes(x = doy, y = ninety_pc), method = "gam", col = "yellow", size = 0.6) +
  geom_smooth(aes(x = doy, y = ten_pc), method = "gam", col = "yellow", size = 0.6) +
  labs(y = "Rate (°C/day)", x = "Day of Year", title = "Northwest Atlantic [5 day window]") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 

NW_7_plt <- ggplot(NW_Atl_clim_7) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_line(aes(x = doy, y = mean_rate), col = "grey40", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  geom_smooth(aes(x = doy, y = rate), method = "gam", col = "white", size = 0.6) +
  geom_smooth(aes(x = doy, y = ninety_pc), method = "gam", col = "yellow", size = 0.6) +
  geom_smooth(aes(x = doy, y = ten_pc), method = "gam", col = "yellow", size = 0.6) +
  labs(y = "Rate (°C/day)", x = "Day of Year", title = "Northwest Atlantic [7 day window]") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 

Med_3_plt <- ggplot(Med_clim_3) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_line(aes(x = doy, y = mean_rate), col = "grey40", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  geom_smooth(aes(x = doy, y = rate), method = "gam", col = "white", size = 0.6) +
  geom_smooth(aes(x = doy, y = ninety_pc), method = "gam", col = "yellow", size = 0.6) +
  geom_smooth(aes(x = doy, y = ten_pc), method = "gam", col = "yellow", size = 0.6) +
  labs(y = "Rate (°C/day)", x = "Day of Year", title = "Mediterranean [3 day window]") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 

Med_5_plt <- ggplot(Med_clim) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_line(aes(x = doy, y = mean_rate), col = "grey40", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  geom_smooth(aes(x = doy, y = rate), method = "gam", col = "white", size = 0.6) +
  geom_smooth(aes(x = doy, y = ninety_pc), method = "gam", col = "yellow", size = 0.6) +
  geom_smooth(aes(x = doy, y = ten_pc), method = "gam", col = "yellow", size = 0.6) +
  labs(y = "Rate (°C/day)", x = "Day of Year", title = "Mediterranean [5 day window]") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 

Med_7_plt <- ggplot(Med_clim_7) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_line(aes(x = doy, y = mean_rate), col = "grey40", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  geom_smooth(aes(x = doy, y = rate), method = "gam", col = "white", size = 0.6) +
  geom_smooth(aes(x = doy, y = ninety_pc), method = "gam", col = "yellow", size = 0.6) +
  geom_smooth(aes(x = doy, y = ten_pc), method = "gam", col = "yellow", size = 0.6) +
  labs(y = "Rate (°C/day)", x = "Day of Year", title = "Mediterranean [7 day window]") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 

Aus_3_plt <- ggplot(W_Aus_clim_3) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_line(aes(x = doy, y = mean_rate), col = "grey40", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  geom_smooth(aes(x = doy, y = rate), method = "gam", col = "white", size = 0.6) +
  geom_smooth(aes(x = doy, y = ninety_pc), method = "gam", col = "yellow", size = 0.6) +
  geom_smooth(aes(x = doy, y = ten_pc), method = "gam", col = "yellow", size = 0.6) +
  labs(y = "Rate (°C/day)", x = "Day of Year", title = "Western Australia [3 day window]") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 

Aus_5_plt <- ggplot(W_Aus_clim) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_line(aes(x = doy, y = mean_rate), col = "grey40", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  geom_smooth(aes(x = doy, y = rate), method = "gam", col = "white", size = 0.6) +
  geom_smooth(aes(x = doy, y = ninety_pc), method = "gam", col = "yellow", size = 0.6) +
  geom_smooth(aes(x = doy, y = ten_pc), method = "gam", col = "yellow", size = 0.6) +
  labs(y = "Rate (°C/day)", x = "Day of Year", title = "Western Australia [5 day window]") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 

Aus_7_plt <- ggplot(W_Aus_clim_7) +
  geom_point(aes(x = doy, y = rate), alpha = 0.05) +
  geom_line(aes(x = doy, y = mean_rate), col = "grey40", size = 1.1) +
  geom_line(aes(x = doy, y = ninety_pc), col = "red", size = 1.1) +
  geom_line(aes(x = doy, y = ten_pc), col = "blue", size = 1.1) + 
  geom_smooth(aes(x = doy, y = rate), method = "gam", col = "white", size = 0.6) +
  geom_smooth(aes(x = doy, y = ninety_pc), method = "gam", col = "yellow", size = 0.6) +
  geom_smooth(aes(x = doy, y = ten_pc), method = "gam", col = "yellow", size = 0.6) +
  labs(y = "Rate (°C/day)", x = "Day of Year", title = "Western Australia [7 day window]") + 
  theme_linedraw() +
  theme(plot.title = element_text(size = rel(1.4), lineheight = .9, face = "bold")) 


ggarrange(NW_5_plt,
          Med_5_plt,
          Aus_5_plt)


# Plot TRUE ---------------------------------------------------------------

# Plot >5 days of consecutive threshold exceedence
  
# 10/08/2018 - 15/08/2018 NW Atl
NW_Atl_slice <- NW_Atl_clim[13352:13380,] # isolate period of time with extreme rates

ggplot(NW_Atl_slice, aes(x = t, y = temp)) +
  geom_line() 

# 05/07/2020 - 13/07/2020
NW_Atl_slice2 <- NW_Atl_clim[14056:14079,]

ggplot(NW_Atl_slice2, aes(x = t, y = temp)) +
  geom_line() 

# 20/07/2019 - 25/07/2019
NW_Atl_slice3 <- NW_Atl_clim[13700:13728,]

ggplot(NW_Atl_slice3, aes(x = t, y = temp)) +
  geom_line() 

# 05/04/2017 - 12/04-2017
NW_Atl_slice4 <- NW_Atl_clim[12861:12889,]

ggplot(NW_Atl_slice4, aes(x = t, y = temp)) +
  geom_line() 


# NOTES -------------------------------------------------------------------

# Speed over convinience (data table over tidyverse) (for final version!)
# profiling -> timings (Rstudio)


# Creating R packages


# For detect event 
  # slide linear regression across entire climatology
  # detect events where slope exceeds percentiles

# For your console
heatwaveR:::clim_spread() 
heatwaveR:::clim_calc() 