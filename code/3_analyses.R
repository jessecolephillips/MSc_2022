# Analyses for MEXTEF project
# 13/02/2023
# Jesse Phillips  


# Setup -------------------------------------------------------------------

source("code/1_functions.R")
source("code/2_metadata.R")


# False Bay Analysis ------------------------------------------------------

# Analyse the False Bay data for the puffer washup in late March 2021

  # Run Rds data through detect_mextefs() function at 1 day difference
system.time(
  # First we start by choosing the False Bay dataframe
  FB_mextefs <- False_Bay_data %>% 
    # Then we group the data by the 'lon' and 'lat' columns
    group_by(lon, lat) %>% 
    # Then we run our MEXTEF detecting function on each group
    group_map(~detect_mextefs(.x))
)

  # Slice mextefs in mid March 2021
FB_mextef_event_df <- FB_mextefs[[4]]$h2c_events |> 
  filter(between(date_peak, as.Date('2021-03-10'), as.Date('2021-03-18'))) |> 
  select(event_no, date_peak, intensity_max)

  # 18.125 -33.875 (h2c of -2.9 in 1d on March 14/15)


# Cape Cod Analysis -------------------------------------------------------

# Analyse Cape Cod dataset after cold-stunned sea turtles were rescued off of Cape Cod, Massachusetts in Nov 2022

  # Run Rds data through detect_mextefs() function at 1 day difference
system.time(
  # First we start by choosing the False Bay dataframe
  CC_mextefs <- Cape_Cod_data %>% 
    # Then we group the data by the 'lon' and 'lat' columns
    group_by(lon, lat) %>% 
    # Then we run our MEXTEF detecting function on each group
    group_map(~detect_mextefs(.x))
)

  # Slice h2c mextefs in late Nov 2022 
for (i in 1:length(CC_mextefs)) {
  CC_mextefs[[i]]$h2c_events |> 
    filter(between(date_peak, as.Date('2019-10-01'), as.Date('2019-12-01'))) |> 
    select(event_no, date_peak, intensity_max) |> 
    print(colnames(i))
}


# East London Analysis ----------------------------------------------------

system.time(
  # First we start by choosing the False Bay dataframe
  EL_mextefs <- East_London_data %>% 
    # Then we group the data by the 'lon' and 'lat' columns
    group_by(lon, lat) %>% 
    # Then we run our MEXTEF detecting function on each group
    group_map(~detect_mextefs(.x))
)

# Slice h2c mextefs in late Nov 2022 
for (i in 1:length(EL_mextefs)) {
  EL_mextefs[[i]]$h2c_events |> 
    filter(between(date_peak, as.Date('2021-02-27'), as.Date('2021-03-07'))) |> 
    select(event_no, date_peak, intensity_max) |> 
    print(colnames(i))
}

# MEXTEF Atlas - 1 --------------------------------------------------------

# load data
SA_data <- readRDS("data/Southern_African_coastline.Rds")

# Select pixel
SA_data_pnt <- SA_data %>% 
  filter(lon == 17.625,
         lat == -33.125)

# Calculate first differences
sst_diff <- SA_data_pnt %>% 
  mutate(diff = temp - lag(temp, n = 1))

# Create climatology
clim_diff <- ts2clm(data = sst_diff, x = t, y = diff, climatologyPeriod = c("1991-01-02", "2022-12-31"))
  # and another - for the lower threshold
  clim_diff_inv <- ts2clm(data = sst_diff, x = t, y = diff, climatologyPeriod = c("1991-01-02", "2022-12-31"), pctile = 10)
  

# Use detect_event() to determine number of events that occurred in that pixel in the last 31 years
event_upper <- detect_event(data = clim_diff, x = t, y = diff, minDuration = 1)
  # and for the lower threshold 
  event_lower <- detect_event(data = clim_diff, x = t, y = diff, minDuration = 1, coldSpells = T)

# SO - since 1991 at these coords, 858 MEXTEFs have occurred
  # 834 with a positive first difference (cold to hot)
  # 24 with a negative first difference (hot to cold)
  
# Or - use a function to do the entire area 
  
# MEXTEF Atlas - EXPANDED -----------------------------------------------
  
# split dataframe
SA_data_WC <- SA_data %>% 
  filter(between(lon, 15, 26),
         between(lat, -37, -28))
  
# Here's an Rob function that will work on the full dataset:
# The plyr technique
# NB: One should never use ALL available cores, save at least 1 for other essential tasks
# My laptop has 4 cores, so I use 3 here
registerDoParallel(cores = 3)
# Detect events
system.time(
  SA_WC_h2c <- plyr::ddply(.data = SA_data_WC, .variables = c("lon", "lat"), .fun = create_atlas, .parallel = TRUE)
) # 6M obs. -> 420s -> 7m
  
# Save it
saveRDS(SA_WC_h2c, file = "SA_WC_h2c")
  
# Now lets set up the data table for the atlas
WC_h2c_num <- SA_WC_h2c |> 
  group_by(lon, lat) |> 
  summarise(n = n())
  
# Combine all datasets to plot together
# Join WC
WC_combined_num <- left_join(WC_c2h_num, WC_h2c_num, by = c("lon", "lat")) |> 
  rename(num_c2h = n.x,
         num_h2c = n.y)
  
# Join EC
EC_combined_num <- left_join(EC_c2h_num, EC_h2c_num, by = c("lon", "lat")) |> 
  rename(num_c2h = n.x,
         num_h2c = n.y)
  
# Combine Datasets
SA_mextef_atlas_full <- bind_rows(WC_combined_num, EC_combined_num) |> 
  mutate(num_combined = num_c2h + num_h2c)
  
# And save
saveRDS(SA_mextef_atlas_full, file = "SA_mextef_atlas")
  