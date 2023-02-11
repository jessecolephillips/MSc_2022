# MEXTEFs in Coastal Biogeographical Realms
# 15/06/2022
# Jesse Phillips
# Script to download temperature time series from 12 coastal realms and calculate frequency of extreme temperature fluctuations

# Load Libraries
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(ggpubr) # For arranging multiple ggplots
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing

# File Information
# The information for the NOAA OISST data
rerddap::info(datasetid = "ncdcOisst21Agg", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# Download Function
# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(x = "ncdcOisst21Agg", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(70, 75),
                       longitude = c(30, 65),
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}

# Date Range
# Date download range by start and end dates per year
dl_years <- data.frame(date_index = 1:4,
                       start = as.Date(c("1990-01-01", 
                                         "1998-01-01", "2006-01-01", 
                                         "2014-01-01")),
                       end = as.Date(c("1997-12-31", 
                                       "2005-12-31", "2013-12-31", 
                                       "2021-12-31")))

# Download/prep Data
# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
system.time(
  OISST_data <- dl_years %>% 
    group_by(date_index) %>% 
    group_modify(~OISST_sub_dl(.x)) %>% 
    ungroup() %>% 
    select(lon, lat, t, temp)
) 

# Save Data
saveRDS(OISST_data, file = "MEOW/Arctic.Rds")


# Temperate Southern Africa -----------------------------------------------
  # lat -35 -30
  # lon 14 25

# Read Rds data
temp_S_Africa <- readRDS("MEOW/temp_S_Africa.Rds")

# Visualise Data
temp_S_Africa %>% 
  filter(t == "2020-04-05") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  borders() + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")

# Weekly Change in Temperature
temp_S_Africa <- temp_S_Africa %>% 
  mutate("delta_temp" = (temp - lag(temp, n = 7)) / 7)

plt1 <- ggplot(temp_S_Africa, aes(x = delta_temp)) +
  geom_histogram(stat = "bin") +
  theme_bw() +
  labs(x = "Weekly change in temperature (°C/day)", y = "", title = "Temperate Southern Africa") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Western Indo-Pacific ----------------------------------------------------
  # lat 10 20
  # lon 73 82

# Read Rds data
W_Indo_Pacific <- readRDS("MEOW/W_Indo_Pacific.Rds")

# Visualise Data
W_Indo_Pacific %>% 
  filter(t == "2020-04-05") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  borders() + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")

# Weekly Change in Temperature
W_Indo_Pacific <- W_Indo_Pacific %>% 
  mutate("delta_temp" = (temp - lag(temp, n = 7)) / 7)

plt2 <- ggplot(W_Indo_Pacific, aes(x = delta_temp)) +
  geom_histogram(stat = "bin") +
  theme_bw() +
  labs(x = "Weekly change in temperature (°C/day)", y = "", title = "Western Indo-Pacific") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Central Indo-Pacific ----------------------------------------------------
  # lat 0 10
  # lon 120 130

# Read Rds data
C_Indo_Pacific <- readRDS("MEOW/C_Indo_Pacific.Rds")

# Visualise Data
C_Indo_Pacific %>% 
  filter(t == "2020-04-05") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  borders() + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")

# Weekly Change in Temperature
C_Indo_Pacific <- C_Indo_Pacific %>% 
  mutate("delta_temp" = (temp - lag(temp, n = 7)) / 7)

plt3 <- ggplot(C_Indo_Pacific, aes(x = delta_temp)) +
  geom_histogram(stat = "bin") +
  theme_bw() +
  labs(x = "Weekly change in temperature (°C/day)", y = "", title = "Central Indo-Pacific") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Temperate Australasia ---------------------------------------------------
  # lat -35 -30
  # lon 150 155

# Read Rds data
temp_Aus <- readRDS("MEOW/temp_Aus.Rds")

# Visualise Data
temp_Aus %>% 
  filter(t == "2020-04-05") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  borders() + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")

# Weekly Change in Temperature
temp_Aus <- temp_Aus %>% 
  mutate("delta_temp" = (temp - lag(temp, n = 7)) / 7)

plt4 <- ggplot(temp_Aus, aes(x = delta_temp)) +
  geom_histogram(stat = "bin") +
  theme_bw() +
  labs(x = "Weekly change in temperature (°C/day)", y = "", title = "Temperate Australasia") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Tropical Atlantic -------------------------------------------------------
  # lat 0 10
  # lon 300 310

# Read Rds data
trop_Atlantic <- readRDS("MEOW/trop_Atlantic.Rds")

# Visualise Data
trop_Atlantic %>% 
  filter(t == "2020-04-05") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  borders() + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")

# Weekly Change in Temperature
trop_Atlantic <- trop_Atlantic %>% 
  mutate("delta_temp" = (temp - lag(temp, n = 7)) / 7)

plt5 <- ggplot(trop_Atlantic, aes(x = delta_temp)) +
  geom_histogram(stat = "bin") +
  theme_bw() +
  labs(x = "Weekly change in temperature (°C/day)", y = "", title = "Tropical Atlantic") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Temperate South America -------------------------------------------------
  # lat -30 -35
  # lon -72 -77

# Eastern Indo-Pacific ----------------------------------------------------
  # lat 0 5
  # lon -160 -165

# Tropical Eastern Pacific ------------------------------------------------
  # lat 
  # lon


# Temperate Northern Pacific ----------------------------------------------
  # lat 
  # lon

# Temperate Northern Atlantic ---------------------------------------------
  # lat 
  # lon

# Arctic ------------------------------------------------------------------
  # lat 70 75
  # lon 35 65

# Read Rds data
Arctic <- readRDS("MEOW/Arctic.Rds")

# Visualise Data
Arctic %>% 
  filter(t == "2020-04-05") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  borders() + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")

# Weekly Change in Temperature
Arctic <- Arctic %>% 
  mutate("delta_temp" = (temp - lag(temp, n = 7)) / 7)

plt6 <- ggplot(Arctic, aes(x = delta_temp)) +
  geom_histogram(stat = "bin") +
  theme_bw() +
  labs(x = "Weekly change in temperature (°C/day)", y = "", title = "Arctic") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Southern Ocean ----------------------------------------------------------
  # lat 
  # lon




# ggarrange ---------------------------------------------------------------

ggarrange(plt1,plt2,plt3,plt4)
ggarrange(plt5,plt6)
