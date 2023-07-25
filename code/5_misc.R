# Miscellaneous tests and analyses for the mextef project
# Jesse Phillips
# May 2023

# Setup -------------------------------------------------------------------

source("code/1_functions.R")
source("code/2_metadata.R")


# MHW & MCS Analyses ------------------------------------------------------

# This section is for analysis and plotting of MHWs and MCSs that are associated with MEXTEFs

# False Bay MCS search in mid-late March 2021
event_only <- function(df){
  # First calculate the climatologies
  clim <- ts2clm(data = df, climatologyPeriod = c("1990-01-01", "2021-12-31"), pctile = 10)
  # Then the events
  event <- detect_event(data = clim, coldSpells = T)
  # Return only the event metric dataframe of results
  return(event$event)
}

system.time(
  # First we start by choosing the 'OISST' dataframe
  MCS_dplyr <- False_Bay_data %>% 
    # Then we group the data by the 'lon' and 'lat' columns
    group_by(lon, lat) %>% 
    # Then we run our MHW detecting function on each group
    group_modify(~event_only(.x))
) 

# NO MCS FOUND at time and date after MEXTEF occurs


# Quick Mapping -----------------------------------------------------------

# This section is for mapping out areas where you need exact co-ordinates

  # Map for Cape Cod Bay
Cape_Cod_data %>% 
  filter(t == "2021-03-28") %>% 
  ggplot(aes(x = (lon-360), y = lat)) +
  geom_tile(aes(fill = temp)) +
  borders(xlim = c(-80, -65), ylim = c(35, 50)) + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")


# General SST trends ------------------------------------------------------

# Plot the general SST trend in Cape Cod Bay for the month of November (doy = c(306, 335))
  # Use heatwaveR code
CC_clim <- Cape_Cod_data |> 
  filter(lon == 289.625 & lat == 41.875) |>
  ts2clm(climatologyPeriod = c("1991-01-01", "2022-01-01"))

CC_clim |> 
  filter(between(doy, 306, 335)) |> 
  ggplot() + 
    geom_jitter(aes(x = doy, y = temp), alpha = 0.3) + 
    geom_line(aes(x = doy, y = seas), colour = "navy", linewidth = 2) +
    labs(x = "", y = expression(paste("Temperature (", degree, "C)"))) + 
    scale_x_continuous(breaks = c(306, 313, 320, 327, 334),
                       labels = c("01 Nov", "08 Nov", "15 Nov", "22 Nov", "29 Nov")) +
    theme_pubr()
  
  # Gradual drop from 12.31 degrees C to 8.87 degrees C on average over the course of the month
    # -3.44 degrees C over 30 days
