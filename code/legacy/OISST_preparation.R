# Downloading and Preparing OISST data
# 07/03/2022
# Jesse Phillips 

# Setup -------------------------------------------------------------------
# The packages we will need
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("tidync")
# install.packages("doParallel")
# install.packages("rerddap")
# install.packages("plyr") # Note that this library should never be loaded, only installed

# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing


# Downloading Subsetted Data ----------------------------------------------
# File Information
  # The information for the NOAA OISST data
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

  # Note that there is also a version with lon values from 0 to 360
rerddap::info(datasetid = "ncdcOisst21Agg", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# Download Function
  # This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(datasetx = "ncdcOisst21Agg", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(-37, -28),
                       longitude = c(15, 34), 
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst, lon = longitude, lat = latitude) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}

# Date Range
  # Date download range by start and end dates per year
dl_years <- data.frame(date_index = 1:4,
                       start = as.Date(c("1991-01-01", 
                                         "1999-01-01", "2007-01-01", 
                                         "2015-01-01")),
                       end = as.Date(c("1998-12-31", 
                                       "2006-12-31", "2014-12-31", 
                                       "2022-12-31")))

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

# Visualise Data
OISST_data %>% 
  filter(t == "2021-03-28") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  #borders() + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")

# Save Data
# Save the data as an .Rds file because it has a much better compression rate than .RData
saveRDS(OISST_data, file = "data/Southern_African_coastline.Rds")



# Downloading Global Data -------------------------------------------------
# The method outlined above should be used for all applications other than
  # downloading the entire globes data set.

# File Information
  # First we tell R where the data are on the interwebs
OISST_base_url <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
  # Note that one may go to this URL in any web browser to manually inspect the files

  # Now we create a data.frame that contains all of the dates we want to download
  # NB: In order to change the dates download changes the dates in the following line
OISST_dates <- data.frame(t = seq(as.Date("2023-01-01"), as.Date("2023-01-07"), by = "day"))

  # To finish up this step we add some text to those dates so they match the OISST file names
OISST_files <- OISST_dates %>% 
  mutate(t_day = gsub("-", "", t),
         t_month = substr(t_day, 1, 6),
         t_year = year(t),
         file_name = paste0(OISST_base_url, t_month, "/", "oisst-avhrr-v02r01.", t_day ,".nc"))

# Download Data
  # This function will go about downloading each day of data as a NetCDF file
  # Note that this will download files into a 'data/OISST' folder in the root directory
    # If this folder does not exist it will create it
    # If it does not automatically create the folder it will need to be done manually
    # The folder that is created must be a new folder with no other files in it
    # A possible bug with netCDF files in R is they won't load correctly from 
    # existing folders with other file types in them
  # This function will also check if the file has been previously downloaded
    # If it has it will not download it again
OISST_url_daily_dl <- function(target_URL){
  dir.create("~/data/OISST", showWarnings = F)
  file_name <- paste0("~/data/OISST/",sapply(strsplit(target_URL, split = "/"), "[[", 10))
  if(!file.exists(file_name)) download.file(url = target_URL, method = "libcurl", destfile = file_name)
}

  # The more cores used, the faster the data may be downloaded
    # It is best practice to not use all of the cores on one's machine
    # The laptop on which I am running this code has 4 cores, so I use 3 here
doParallel::registerDoParallel(cores = 2)

  # And with that we are clear for take off
system.time(plyr::l_ply(OISST_files$file_name, .fun = OISST_url_daily_dl, .parallel = T)) # ~15 seconds

  # In roughly 15 seconds a user may have a full month of global data downloaded
  # This scales well into years and decades, and is much faster with more cores
  # Download speeds will also depend on the speed of the users internet connection

# Load Data
  # This function will load and subset daily data into one data.frame
  # Note that the subsetting by lon/lat is done before the data are loaded
  # This means it will use much less RAM and is viable for use on most laptops
  # Assuming one's study area is not too large
OISST_load <- function(file_name, lon1, lon2, lat1, lat2){
  OISST_dat <- tidync(file_name) %>%
    hyper_filter(lon = between(lon, lon1, lon2),
                 lat = between(lat, lat1, lat2)) %>% 
    hyper_tibble() %>% 
    select(lon, lat, time, sst) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    mutate(t = as.Date(t, origin = "1978-01-01"))
  return(OISST_dat)
}

  # Locate the files that will be loaded
OISST_files <- dir("~/data/OISST", full.names = T)

  # Load the data in parallel
OISST_dat <- plyr::ldply(.data = OISST_files, .fun = OISST_load, .parallel = T,
                         lon1 = 0, lon2 = 360, lat1 = -90, lat2 = 90)

  # It should only take a few seconds to load one month of data depending on the size of the lon/lat extent chosen

# Visualise Data
OISST_dat %>% 
  filter(t == "2023-01-01") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")
