# Demonstration of the different categories of thermal fluctuations using heatwaveR package
# 12 May 2022
# Jesse Phillips
# Example data sets in heatwaveR will be used to produce figures that demonstrate the various categories of fluctuations
# Fictitious data will be used when specific examples of events cannot be found


# Load Packages -----------------------------------------------------------

library("dplyr")
library("heatwaveR")
library("ggplot2")


# Data --------------------------------------------------------------------

heatwaveR::sst_WA # dataset containing SST in Western Australia region from 1982-01-01 to 2020-12-31

head(heatwaveR::sst_WA)


# Calc MHWs and MCSs ------------------------------------------------------

MHW_ts <- ts2clm(heatwaveR::sst_WA, climatologyPeriod = c('1982-01-01', '2020-12-31'))
MHW_clim_events <- detect_event(MHW_ts)
MHW_event <- MHW_clim_events$event

MCS_ts <- ts2clm(heatwaveR::sst_WA, climatologyPeriod = c('1982-01-01', '2020-12-31'), pctile = 10)
MCS_clim_events <- detect_event(MCS_ts, coldSpells = T)
MCS_event <- MCS_clim_events$event


# Find Fluctuations -------------------------------------------------------

# Now lets find some examples of fluctuations between heatwaves and coldspells

# Add a column to each to specify whether its a MHW or MCS
MHW_event <- MHW_event %>% 
  mutate("event_type" = "MHW")

MCS_event <- MCS_event %>% 
  mutate("event_type" = "MCS")

# Combine into one dataframe 
Combined_Events_df <- rbind(MHW_event, MCS_event)

# Order the dataset
Combined_Events_df <- Combined_Events_df[order(Combined_Events_df$date_start),]

# Calculate number of days between events in dataframe 
Combined_Events_df <- Combined_Events_df %>% 
  mutate("days_between" = difftime(date_start, lag(date_end), units = c("days")))

# Caluculate rates of transition
Combined_Events_df <- Combined_Events_df %>% 
  mutate("rate_transition" = ((intensity_max - lag(intensity_max)) / as.numeric(days_between)))

# Plot rates of transition between events
ggplot(Combined_Events_df, aes(x = rate_transition)) +
  geom_histogram(stat = "bin") +
  theme_bw() +
  labs(x = "Rate of transition between extreme events (°C/day)")

# BUT
# These are just rates of transition between extreme events (MHWs and MCSs)
# What about rates of transition that ARENT between extreme events?

# Combine climatology dfs 
MCS_clim_events$climatology <- MCS_clim_events$climatology %>% 
  rename("thresh_low" = thresh) 

MHW_clim_events$climatology <- MHW_clim_events$climatology %>% 
  rename("thresh_high" = thresh) 

clim <- MHW_clim_events$climatology %>% 
  select(doy, t, temp, seas, thresh_high)

clim2 <- MCS_clim_events$climatology %>% 
  select(thresh_low)

clim <- as.data.frame(c(clim, clim2))

# Daily Change in Temperature
daily_change <- clim %>% 
  mutate("delta_temp" = temp - lag(temp))

ggplot(daily_change, aes(x = delta_temp)) +
  geom_histogram(stat = "bin") +
  theme_bw() +
  labs(x = "Daily change in temperature (°C)")


# Weekly Change in Temperature
weekly_change <- clim %>% 
  mutate("delta_temp" = (temp - lag(temp, n = 7)) / 7)

ggplot(weekly_change, aes(x = delta_temp)) +
  geom_histogram(stat = "bin") +
  theme_bw() +
  labs(x = "Weekly change in temperature (°C/day)")


# Plot times of highest rates of weekly change
clim_sub <- weekly_change %>% 
  slice(10267:10293)

rate <- (clim_sub$temp[20] - clim_sub$temp[12]) / (clim_sub$doy[20] - clim_sub$doy[12])
  
ggplot(clim_sub) +
  geom_line(aes(x = t, y = seas), col = "grey") +
  geom_line(aes(x = t, y = thresh_low), col = "darkgreen") +
  geom_line(aes(x = t, y = thresh_high), col = "darkgreen") +
  geom_line(aes(x = t, y = temp)) +
  geom_text(aes(x = as.Date('2010-03-02'), y = 20), label = "r = 0.625", col = "black") +
  theme_bw() +
  labs(x = "", y = "Temperature (°C)")


# Cool, lets do another
clim_sub_2 <- weekly_change %>% 
  slice(180:215)

ggplot(clim_sub_2) +
  geom_line(aes(x = t, y = seas), col = "grey") +
  geom_line(aes(x = t, y = thresh_low), col = "darkgreen") +
  geom_line(aes(x = t, y = thresh_high), col = "darkgreen") +
  geom_line(aes(x = t, y = temp)) +
  #geom_text(aes(x = as.Date('2010-03-02'), y = 20), label = "r = 0.625", col = "black") +
  theme_bw()


# Lets do like 3 more
clim_sub_3 <- weekly_change %>% 
  slice(12990:13020)

rate_3 <- (clim_sub_3$temp[27] - clim_sub_3$temp[11]) / (clim_sub_3$doy[27] - clim_sub_3$doy[11])

ggplot(clim_sub_3) +
  geom_line(aes(x = t, y = seas), col = "grey") +
  geom_line(aes(x = t, y = thresh_low), col = "darkgreen") +
  geom_line(aes(x = t, y = thresh_high), col = "darkgreen") +
  geom_line(aes(x = t, y = temp)) +
  geom_text(aes(x = as.Date('2017-07-29'), y = 18), label = "r = -0.234", col = "black") +
  theme_bw()


clim_sub_4 <- weekly_change %>% 
  slice(8815:8840)

rate_4 <- (clim_sub_4$temp[18] - clim_sub_4$temp[6]) / (clim_sub_4$doy[18] - clim_sub_4$doy[6])

ggplot(clim_sub_4) +
  geom_line(aes(x = t, y = seas), col = "grey") +
  geom_line(aes(x = t, y = thresh_low), col = "darkgreen") +
  geom_line(aes(x = t, y = thresh_high), col = "darkgreen") +
  geom_line(aes(x = t, y = temp)) +
  geom_text(aes(x = as.Date('2006-03-11'), y = 21), label = "r = 0.352", col = "black") +
  theme_bw() +
  labs(x = "", y = "Temperature (°C)")



clim_sub_5 <- weekly_change %>% 
  slice(9918:9938)

rate_5 <- (clim_sub_5$temp[14] - clim_sub_5$temp[6]) / (clim_sub_5$doy[14] - clim_sub_5$doy[6])

ggplot(clim_sub_5) +
  geom_line(aes(x = t, y = seas), col = "grey") +
  geom_line(aes(x = t, y = thresh_low), col = "darkgreen") +
  geom_line(aes(x = t, y = thresh_high), col = "darkgreen") +
  geom_line(aes(x = t, y = temp)) +
  geom_text(aes(x = as.Date('2009-03-10'), y = 21.5), label = "r = 0.446", col = "black") +
  theme_bw() +
  labs(x = "", y = "Temperature (°C)")



clim_sub_6 <- weekly_change %>% 
  slice(7640:7660)

rate_6 <- (clim_sub_6$temp[15] - clim_sub_6$temp[8]) / (clim_sub_6$doy[15] - clim_sub_6$doy[8])

ggplot(clim_sub_6) +
  geom_line(aes(x = t, y = seas), col = "grey") +
  geom_line(aes(x = t, y = thresh_low), col = "darkgreen") +
  geom_line(aes(x = t, y = thresh_high), col = "darkgreen") +
  geom_line(aes(x = t, y = temp)) +
  geom_text(aes(x = as.Date('2002-12-18'), y = 19.5), label = "r = 0.406", col = "black") +
  theme_bw() + 
  labs(x = "", y = "Temperature (°C)")



clim_sub_7 <- weekly_change %>% 
  slice(6780:6806)

rate_7 <- (clim_sub_7$temp[19] - clim_sub_7$temp[7]) / (clim_sub_7$doy[19] - clim_sub_7$doy[7])

ggplot(clim_sub_7) +
  geom_line(aes(x = t, y = seas), col = "grey") +
  geom_line(aes(x = t, y = thresh_low), col = "darkgreen") +
  geom_line(aes(x = t, y = thresh_high), col = "darkgreen") +
  geom_line(aes(x = t, y = temp)) +
  geom_text(aes(x = as.Date('2000-07-28'), y = 18.5), label = "r = -0.283", col = "black") +
  theme_bw() + 
  labs(x = "", y = "Temperature (°C)")



clim_sub_8 <- weekly_change %>% 
  slice(10680:10750)

rate_7 <- (clim_sub_7$temp[19] - clim_sub_7$temp[7]) / (clim_sub_7$doy[19] - clim_sub_7$doy[7])

ggplot(clim_sub_8) +
  geom_line(aes(x = t, y = seas), col = "grey") +
  geom_line(aes(x = t, y = thresh_low), col = "darkgreen") +
  geom_line(aes(x = t, y = thresh_high), col = "darkgreen") +
  geom_line(aes(x = t, y = temp)) +
  #geom_text(aes(x = as.Date('2000-07-28'), y = 18.5), label = "r = -0.283", col = "black") +
  theme_bw() + 
  labs(x = "", y = "Temperature (°C)")
