# Use this to detect events after downloading files using OISST_preparation.R
# 07/03/2022
# Jesse Phillips 


# Load Packages -----------------------------------------------------------
library(dplyr) # For basic data manipulation
library(ggplot2) # For visualising data
library(heatwaveR) # For detecting MHWs
library(tidync) # For easily dealing with NetCDF data
library(doParallel) # For parallel processing


# Load Data ---------------------------------------------------------------
OISST <- readRDS("~/Documents/MHW_2022/OISST_vignette.Rds")


# Event Detection ---------------------------------------------------------

# Initial wrapper function to combine calculations
event_only <- function(df){
  # First calculate the climatologies
  clim <- ts2clm(data = df, climatologyPeriod = c("1982-01-01", "2011-01-01"))
  # Then the events
  event <- detect_event(data = clim)
  # Return only the event metric dataframe of results
  return(event$event)
}

# The dplyr method
system.time(
  # First we start by choosing the 'OISST' dataframe
  MHW_dplyr <- OISST %>% 
    # Then we group the data by the 'lon' and 'lat' columns
    group_by(lon, lat) %>% 
    # Then we run our MHW detecting function on each group
    group_modify(~event_only(.x))
) # ~125 seconds

# The plyr technique
# NB: One should never use ALL available cores, save at least 1 for other essential tasks
# My laptop has 4 cores, so I use 3 here
registerDoParallel(cores = 3)
# Detect events
system.time(
  MHW_plyr <- plyr::ddply(.data = OISST, .variables = c("lon", "lat"), .fun = event_only, .parallel = TRUE)
) # ~84 seconds


# A harmonious thrid option
  # Combines plyr and dplyr methods
for(i in 1:length(unique(OISST$lon))){
  OISST_sub <- OISST %>% 
    filter(lon == unique(lon)[i])
  saveRDS(object = OISST_sub, file = paste0("~/Documents/MHW_2022/OISST_lon_",i,".Rds"))
}
  # The 'dplyr' wrapper function to pass to 'plyr'
dplyr_wraper <- function(file_name){
  MHW_dplyr <- readRDS(file_name) %>% 
    group_by(lon, lat) %>% 
    group_modify(~event_only(.x))
}
  # Create a vector of the files we want to use
OISST_files <- dir("~/Documents/MHW_2022", pattern = "OISST_lon_*", full.names = T)
  # Use 'plyr' technique to run 'dplyr' technique with multiple cores
system.time(
  MHW_result <- plyr::ldply(OISST_files, .fun = dplyr_wraper, .parallel = T)
) # ~74 seconds

# Save for later use as desired
saveRDS(MHW_result, "~/Documents/MHW2022/MHW_result.Rds")



# Case Study --------------------------------------------------------------
# After detecting events, we can fit a GLM to each pixel to calculate rates
  # of change in some MHW metrics, and plot estimated trends

# Trend Detection
  # summarise number of unique lon, lat and year combinations
OISST_n <- MHW_result %>% 
  mutate(year = lubridate::year(date_start)) %>% 
  group_by(lon, lat, year) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(lon, lat) %>% 
  tidyr::complete(year = c(1982:2019)) %>% # Note that these dates may differ
  mutate(n = ifelse(is.na(n), 0, n))
head(OISST_n)

  # Specify particulars of GLM
lin_fun <- function(ev){
  mod1 <- glm(n ~ year, family = poisson(link = 'log'), data = ev)
  # extract slope coefficient and p-value
  tr <- data.frame(slope = summary(mod1)$coefficients[2,1],
                   p = summary(mod1)$coefficients[2,4])
  return(tr)
}

  # Lastly, we make the calculations
OISST_nTrend <- plyr::ddply(OISST_n, c('lon', 'lat'), lin_fun, .parallel = T)
OISST_nTrend$pval <- cut(OISST_nTrend$p, breaks = c(0, 0.001, 0.01, 0.05, 0.1))
head(OISST_nTrend)

# Visualising the results
  # First, grab the base map from the maps package
map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long)

  # Then create two maps that will stick together using ggpubr
    # The first map will show the slope of the count of events detected per year over time as shades of red
    # The second map will show the significance (p-value) of these trends in shades of grey
map_slope <- ggplot(OISST_nTrend, aes(x = lon, y = lat)) +
  geom_rect(size = 0.2, fill = NA,
            aes(xmin = lon - 0.1, xmax = lon + 0.1, ymin = lat - 0.1, ymax = lat + 0.1,
                colour = pval)) +
  geom_raster(aes(fill = slope), interpolate = FALSE, alpha = 0.9) +
  scale_fill_gradient2(name = "count/year (slope)", high = "red", mid = "white",
                       low = "darkblue", midpoint = 0,
                       guide = guide_colourbar(direction = "horizontal",
                                               title.position = "top")) +
  scale_colour_manual(breaks = c("(0,0.001]", "(0.001,0.01]", "(0.01,0.05]", "(0.05,1]"),
                      values = c("firebrick1", "firebrick2", "firebrick3", "white"),
                      name = "p-value", guide = FALSE) +
  geom_polygon(data = map_base, aes(group = group), 
               colour = NA, fill = "grey80") +
  coord_fixed(ratio = 1, xlim = c(13.0, 23.0), ylim = c(-33, -42), expand = TRUE) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom")

map_p <- ggplot(OISST_nTrend, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = pval), interpolate = FALSE) +
  scale_fill_manual(breaks = c("(0,0.001]", "(0.001,0.01]", "(0.01,0.05]",
                               "(0.05,0.1]", "(0.1,0.5]", "(0.5,1]"),
                    values = c("black", "grey20", "grey40",
                               "grey80", "grey90", "white"),
                    name = "p-value",
                    guide = guide_legend(direction = "horizontal",
                                         title.position = "top")) +
  geom_polygon(data = map_base, aes(group = group), 
               colour = NA, fill = "grey80") +
  coord_fixed(ratio = 1, xlim = c(13.0, 23.0), ylim = c(-33, -42), expand = TRUE) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom")

map_both <- ggpubr::ggarrange(map_slope, map_p, align = "hv")
map_both



# TESTING -----------------------------------------------------------------

MHW_plyr%>% 
  dplyr::ungroup() %>% 
  dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative, lat, lon) %>% 
  dplyr::arrange(-intensity_max) %>% 
  head(5)

OISST_ts <- OISST %>% 
  filter(lat == -39.375 & lon == 16.125) %>% 
  select(t, temp)

clim <- ts2clm(OISST_ts, climatologyPeriod = c('1982-01-01', '2019-12-31'))
mhw <- detect_event(clim)

event_line(mhw, spread = 180, metric = 'intensity_max', 
           start_date = '2008-01-01', end_date = '2019-12-31')
