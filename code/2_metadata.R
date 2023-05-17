# Metadata for MEXTEF project
# 13/02/2023
# Jesse Phillips

# Setup -------------------------------------------------------------------

source("code/1_functions.R")


# Metadata ----------------------------------------------------------------

# Coordinate bounds for southern Africa
xlim_SA <- c(15, 34); ylim_SA <- c(-37, -28)


# NetCDF data -------------------------------------------------------------

# Uber NetCDF (downloaded)
oisst <- tidync("~/data/OISST/OISST_combined_FULL.nc")


# False Bay - March 2021
False_Bay_data <- oisst |> 
  hyper_filter(lon = between(lon, 18, 19),
               lat = between(lat, -35, -34), 
               time = between(time, 15765, 15796)) |>  # 01 March 2021 - 01 April 2021
  hyper_tibble(select_var = "sst") # No sst data??


# Cape Cod - November 2022
Cape_Cod_data <- oisst |> 
  hyper_filter(lon = between(lon, 290, 291),
               lat = between(lat, 41, 42), 
               time = between(time, 16375, 16405)) |> # 01 Nov 2022 - 01 Dec 2022
  hyper_tibble(select_var = "sst") # again, no sst data

# All my SST data seems to stop working after 2019ish...
