# Northern Namibia/Angola SST heating
# Extreme Thermal Fluctuation Practice
# 09/03/2022
# Jesse Phillips


# Load Packages -----------------------------------------------------------

library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(ggpubr)
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing


# Download OISST Data -----------------------------------------------------

# File Information
rerddap::info(datasetid = "ncdcOisst21Agg", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# Download Function
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(x = "ncdcOisst21Agg", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(-30, -10),
                       longitude = c(5, 17),
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}

# Date Range
# (Remember, increments of 7 years)
dl_years <- data.frame(date_index = 1:6,
                       start = as.Date(c("1982-01-01", "1990-01-01", 
                                         "1998-01-01", "2006-01-01", 
                                         "2014-01-01", "2020-01-01")),
                       end = as.Date(c("1989-12-31", "1997-12-31", 
                                       "2005-12-31", "2013-12-31", 
                                       "2019-12-31", "2022-02-21")))

# Download/prep Data
doParallel::registerDoParallel(cores = 3)

system.time(
  OISST_data <- dl_years %>% 
    group_by(date_index) %>% 
    group_modify(~OISST_sub_dl(.x)) %>% 
    ungroup() %>% 
    select(lon, lat, t, temp)
) # 259 seconds elapsed 

# Visualise Data (good practice)
OISST_data %>% 
  filter(t == "2022-02-21") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  borders() + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")

# Save Data
saveRDS(OISST_data, file = "~/Documents/MHW_2022/Nam_Angola_Heating_March_2022/OISST_data/OISST_2022_02_21.Rds")
  # 81MB


# Detect Events -----------------------------------------------------------

# Initial wrapper function to combine calculations
event_only <- function(df){
  # First calculate the climatologies
  clim <- ts2clm(data = df, climatologyPeriod = c("1982-01-01", "2022-01-01"))
  # Then the events
  event <- detect_event(data = clim)
  # Return only the event metric dataframe of results
  return(event$event)
}

# Harmonious OPTION 3 for detecting events efficiently
  # Combines plyr and dplyr methods
for(i in 1:length(unique(OISST_data$lon))){
  OISST_sub <- OISST_data %>% 
    filter(lon == unique(lon)[i])
  saveRDS(object = OISST_sub, file = paste0("~/Documents/MHW_2022/Nam_Angola_Heating_March_2022/OISST_data/OISST_lon_",i,".Rds"))
}
# The 'dplyr' wrapper function to pass to 'plyr'
dplyr_wraper <- function(file_name){
  MHW_dplyr <- readRDS(file_name) %>% 
    group_by(lon, lat) %>% 
    group_modify(~event_only(.x))
}
# Create a vector of the files we want to use
OISST_files <- dir("~/Documents/MHW_2022/Nam_Angola_Heating_March_2022/OISST_data", pattern = "OISST_lon_*", full.names = T)
# Use 'plyr' technique to run 'dplyr' technique with multiple cores
system.time(
  MHW_result <- plyr::ldply(OISST_files, .fun = dplyr_wraper, .parallel = T)
) # a lot of seconds

# Save for later use as desired
saveRDS(MHW_result, file = "~/Documents/MHW2022/Nam_Angola_Heating_March_2022/MHW_result.Rds")
  # Above is straight up not working, so...
fil <- tempfile("MHW_result", fileext = ".Rds")
saveRDS(MHW_result, fil)


## REPEAT ABOVE STEPS FOR MCS

event_only_MCS <- function(df){
  # First calculate the climatologies
  clim <- ts2clm(data = df, climatologyPeriod = c("1982-01-01", "2022-01-01"), pctile = 10)
  # Then the events
  event <- detect_event(data = clim, coldSpells = T)
  # Return only the event metric dataframe of results
  return(event$event)
}

dplyr_wraper_MCS <- function(file_name){
  MCS_dplyr <- readRDS(file_name) %>% 
    group_by(lon, lat) %>% 
    group_modify(~event_only_MCS(.x))
}

system.time(
  MCS_result <- plyr::ldply(OISST_files, .fun = dplyr_wraper_MCS, .parallel = T)
) # a lot of seconds

# Save for later use as desired
saveRDS(MCS_result, file = "~/Documents/MHW2022/Nam_Angola_Heating_March_2022/MCS_result.Rds")
# Above is straight up not working, so...
fil <- tempfile("MCS_result", fileext = ".Rds")
saveRDS(MCS_result, fil)


# Find Extreme Thermal Fluctuation ----------------------------------------

# Find MHW, then find MCS within 2 days (before or after)
MHW_result%>% 
  ungroup() %>% 
  filter(date_start >= '2022-01-01') %>% 
  filter(lat == -18.375 & lon == 11.875) %>% 
  select(event_no, duration, date_start, date_end, intensity_max, intensity_cumulative, lat, lon) %>% 
  arrange(-event_no) %>% 
  head(15)

MCS_result%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(date_start >= '2022-01-01') %>% 
  filter(event_no == 102) %>% 
  dplyr::select(event_no, duration, date_start, date_end, intensity_max, intensity_cumulative, lat, lon) %>% 
  dplyr::arrange(-event_no) %>% 
  head(15)

  # MHW event 70 - 3 day gap -> MCS event 111
  # MHW event 95 - 5 day gap -> MCS event 102 - 5 day gap -> MHW event 96
  # MCS event 97 - 8 day gap -> MHW event 108 [ongoing]
  # MHW event 76 - 1 day gap -> MCS event 102
  # MCS102 - 6d -> MHW103


# Visualise Events --------------------------------------------------------

# Lets plot the event line MHW[95] - 5d gap -> MCS[102] - 5d gap -> MHW[96]
  # lat = -18.375 and lon = 11.875

  # Create time series at specified co-ords
plot1_ts <- OISST_data %>% 
  filter(lat == -18.375 & lon == 11.875) %>% 
  select(t, temp)

  # Calculate climatology and threshold values for both MHWs and MCSs
plot1_mhw_clim <- ts2clm(plot1_ts, climatologyPeriod = c('1982-01-01', '2022-02-21'))
plot1_mcs_clim <- ts2clm(plot1_ts, climatologyPeriod = c('1982-01-01', '2022-02-21'), pctile = 10)

  # Detect events separately 
plot1_mhw_event <- detect_event(plot1_mhw_clim)
plot1_mcs_event <- detect_event(plot1_mcs_clim, coldSpells = T)

  # Create geoms separately to place onto one graph
    # Select region of time series 
plot1_mhw_slice <- plot1_mhw_event$climatology %>% 
  slice(14611:14662)
plot1_mcs_slice <- plot1_mcs_event$climatology %>% 
  slice(14611:14662)

    # ggplot
ggplot(data = plot1_mhw_slice, aes(x = t, y = temp, y2 = thresh)) + 
  geom_flame(fill = 'red') +
  geom_flame(data = plot1_mcs_slice, aes(x = t, y = thresh, y2 = temp), fill = 'navy') +
  geom_line(aes(y = temp)) +
  geom_line(aes(y = seas), colour = 'grey80') + 
  geom_line(aes(y = thresh), colour = 'forestgreen') +
  geom_line(data = plot1_mcs_slice, aes(y = thresh), colour = 'forestgreen') +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL) +
  theme_bw()


# MHW event 76 - 1d gap -> MCS event 102
  # lat == -26.125 & lon == 14.625

  # Create time series at specified co-ords
plot2_ts <- OISST_data %>% 
  filter(lat == -26.125 & lon == 14.625) %>% 
  select(t, temp)

  # Calculate climatology and threshold values for both MHWs and MCSs
plot2_mhw_clim <- ts2clm(plot2_ts, climatologyPeriod = c('1982-01-01', '2022-02-21'))
plot2_mcs_clim <- ts2clm(plot2_ts, climatologyPeriod = c('1982-01-01', '2022-02-21'), pctile = 10)

  # Detect events separately 
plot2_mhw_event <- detect_event(plot2_mhw_clim)
plot2_mcs_event <- detect_event(plot2_mcs_clim, coldSpells = T)

  # Create geoms separately to place onto one graph
    # Select region of time series 
plot2_mhw_slice <- plot2_mhw_event$climatology %>% 
  slice(14610:14655)
plot2_mcs_slice <- plot2_mcs_event$climatology %>% 
  slice(14610:14655)

    # ggplot
ggplot(data = plot2_mhw_slice, aes(x = t, y = temp, y2 = thresh)) + 
  geom_flame(fill = 'salmon') +
  geom_flame(n = 5, n_gap = 2, fill = 'red') +
  geom_flame(data = plot2_mcs_slice, aes(x = t, y = thresh, y2 = temp), fill = 'steelblue3') +
  geom_flame(data = plot2_mcs_slice, aes(x = t, y = thresh, y2 = temp), n = 5, n_gap = 2, fill = 'navy') +
  geom_line(aes(y = temp)) +
  geom_line(aes(y = seas), colour = 'grey80') + 
  geom_line(aes(y = thresh), colour = 'forestgreen') +
  geom_line(data = plot2_mcs_slice, aes(y = thresh), colour = 'forestgreen') +
  geom_text(colour = 'black', aes(x = as.Date('2022-02-12'), y = 13.8, label = 'lat = -26.125\nlon = 14.625')) +
  scale_x_date(date_breaks = '1 week', date_minor_breaks = '1 day', date_labels = '%b %d') +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL) +
  theme_bw()
