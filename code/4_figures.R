# Figures for MEXTEF project 
# 13/02/2023
# Jesse Phillips 

# Setup -------------------------------------------------------------------

source("code/1_functions.R")
source("code/2_metadata.R")
source("code/3_analyses.R")


# Visualisations ----------------------------------------------------------

# Graph depicting frequency of mextefs at each pixel over the last 30 years (COLD TO HOT)
plt_1 <- ggplot(SA_mextef_atlas_full, aes(x = lon, y  = lat)) + 
  geom_tile(aes(fill = num_c2h)) +
  scale_fill_viridis_c(option = "B") + 
  coord_quickmap(expand = F) +
  theme(legend.position = "bottom") +
  labs(title = "Number of COLD TO HOT MEXTEFs in the last 30 years")

# Now the HOT TO COLD graph
plt_2 <- ggplot(SA_mextef_atlas_full, aes(x = lon, y  = lat)) + 
  geom_tile(aes(fill = num_h2c)) +
  scale_fill_viridis_c(option = "B") + 
  coord_quickmap(expand = F) +
  theme(legend.position = "bottom") +
  labs(title = "Number of HOT TO COLD MEXTEFs in the last 30 years")

# Final plot 
plt_3 <- ggarrange(plt_1, plt_2)
plt_3

# Combined Plot
ggplot(SA_mextef_atlas_full, aes(x = lon, y  = lat)) + 
  geom_tile(aes(fill = num_combined)) +
  scale_fill_viridis_c(option = "B") + 
  coord_quickmap(expand = F) +
  theme(legend.position = "bottom") +
  labs(title = "Number of MEXTEFs (combined) in the last 30 years")
