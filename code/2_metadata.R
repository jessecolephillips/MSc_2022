# Metadata for MEXTEF project
# 13/02/2023
# Jesse Phillips

# Setup -------------------------------------------------------------------

source("code/1_functions.R")


# Metadata ----------------------------------------------------------------

# Coordinate bounds for southern Africa
  #xlim_SA <- c(15, 34); ylim_SA <- c(-37, -28)


# NetCDF data -------------------------------------------------------------

# Uber NetCDF (downloaded)
  #oisst <- tidync("~/data/OISST/OISST_combined.nc")


# False Bay - March 2021 [Alternative data source]
  #False_Bay_data <- oisst |> 
    #hyper_filter(lon = between(lon, 18, 19),
                #lat = between(lat, -35, -34), 
                #time = between(time, 15765, 15796)) |>  # 01 March 2021 - 01 April 2021
    #hyper_tibble() # No sst data??
False_Bay_data <- readRDS("data/False_Bay_time_temp_raw.Rds")

# Cape Cod - November 2022 [Alternative data source]
  #Cape_Cod_data <- oisst |> 
    #hyper_filter(lon = between(lon, 290, 291),
                #lat = between(lat, 41, 42), 
                #time = between(time, 16375, 16405)) |> # 01 Nov 2022 - 01 Dec 2022
    #hyper_tibble(select_var = "sst") # again, no sst data
Cape_Cod_data <- readRDS("data/Cape_Cod_time_temp_raw.Rds")

# All my SST data (from the Uber NetCDF) seems to stop working after 2019ish...
