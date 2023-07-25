# Figures for MEXTEF project 
# 13/02/2023
# Jesse Phillips 

# Setup -------------------------------------------------------------------

source("code/1_functions.R")
source("code/2_metadata.R")


# False Bay MEXTEF --------------------------------------------------------

# March 2021 h2c mextef that caused mass pufferfish washup 

  # Subset data AND mextef to highlight
FB_plot <- False_Bay_data |> 
  filter(lon == 18.125 & lat == -33.875) |>
  filter(between(t, as.Date('2021-03-12'), as.Date('2021-03-23')))

FB_mextef_plot <- FB_plot |> 
  filter(between(t, as.Date('2021-03-14'), as.Date('2021-03-15')))

ggplot() +
  geom_line(data = FB_plot, aes(x = t, y = temp)) + 
  geom_line(data = FB_mextef_plot, aes(x = t, y = temp), colour = "deepskyblue4", linewidth = 2) +
  geom_vline(data = FB_plot, aes(xintercept = as.Date('2021-03-20')), lty = 2, linewidth = 1.5, colour = "honeydew4") +
  geom_text(data = FB_plot, x = as.Date('2021-03-15'), y = 21, label = "r = -2.3", colour = "deepskyblue4") +
  theme_pubr() + 
  labs(x = "", y = expression(paste("Temperature (", degree, "C)")))
  # 18.125 -33.875 (h2c of -2.9 in 1d on March 14/15)


# Cape Cod MEXTEF ---------------------------------------------------------

# 08 Nov 22, 23/27 Nov 21, 12/19/23 Nov 20, 09/13/17 Nov 19

# Late Nov is 'turtle stranding season' in Cape Cod, Massachusetts 

  # Subset data and mextef to highlight for all 4 years

  # 2022
CC_plot_22 <- Cape_Cod_data |> 
  filter(lon == 289.625 & lat == 41.875) |>
  filter(between(t, as.Date('2022-11-02'), as.Date('2022-11-14')))

CC_mextef_plot_22 <- CC_plot_22 |> 
  filter(between(t, as.Date('2022-11-07'), as.Date('2022-11-08'))) # -0.58

plt_2022 <- ggplot() +
  geom_line(data = CC_plot_22, aes(x = t, y = temp)) + 
  geom_line(data = CC_mextef_plot_22, aes(x = t, y = temp), colour = "deepskyblue4", linewidth = 2) +
  geom_text(data = CC_plot_22, x = as.Date('2022-11-09'), y = 14.9, label = "r = -0.58", colour = "deepskyblue4") +
  theme_pubr() + 
  labs(x = "", y = expression(paste("Temperature (", degree, "C)")))

  # 2021
CC_plot_21 <- Cape_Cod_data |> 
  filter(lon == 289.625 & lat == 41.875) |>
  filter(between(t, as.Date('2021-10-25'), as.Date('2021-11-06')))

CC_mextef_plot_21 <- CC_plot_21 |> 
  filter(between(t, as.Date('2021-10-26'), as.Date('2021-10-27'))) #-0.88

plt_2021 <- ggplot() +
  geom_line(data = CC_plot_21, aes(x = t, y = temp)) + 
  geom_line(data = CC_mextef_plot_21, aes(x = t, y = temp), colour = "deepskyblue4", linewidth = 2) +
  geom_text(data = CC_plot_21, x = as.Date('2021-10-28'), y = 15.7, label = "r = -0.88", colour = "deepskyblue4") +
  theme_pubr() + 
  labs(x = "", y = expression(paste("Temperature (", degree, "C)")))

test <- Cape_Cod_data |> 
  filter(lon == 289.625 & lat == 41.875) |>
  filter(between(t, as.Date('2019-10-01'), as.Date('2019-11-17'))) |> 
  mutate(diff = temp - lag(temp, n = 1))
  
  # 2020
CC_plot_20 <- Cape_Cod_data |> 
  filter(lon == 289.625 & lat == 41.875) |>
  filter(between(t, as.Date('2020-10-31'), as.Date('2020-11-12')))

CC_mextef_plot_20 <- CC_plot_20 |> 
  filter(between(t, as.Date('2020-11-02'), as.Date('2020-11-03'))) #-0.50

plt_2020 <- ggplot() +
  geom_line(data = CC_plot_20, aes(x = t, y = temp)) + 
  geom_line(data = CC_mextef_plot_20, aes(x = t, y = temp), colour = "deepskyblue4", linewidth = 2) +
  geom_text(data = CC_plot_20, x = as.Date('2020-11-04'), y = 12.4, label = "r = -0.50", colour = "deepskyblue4") +
  theme_pubr() + 
  labs(x = "", y = expression(paste("Temperature (", degree, "C)")))

  # 2019
CC_plot_19 <- Cape_Cod_data |> 
  filter(lon == 289.625 & lat == 41.875) |>
  filter(between(t, as.Date('2019-11-07'), as.Date('2019-11-19')))

CC_mextef_plot_19a <- CC_plot_19 |> 
  filter(between(t, as.Date('2019-11-08'), as.Date('2019-11-09'))) #-0.72 

CC_mextef_plot_19b <- CC_plot_19 |> 
  filter(between(t, as.Date('2019-11-12'), as.Date('2019-11-13'))) #-0.63 

CC_mextef_plot_19c <- CC_plot_19 |> 
  filter(between(t, as.Date('2019-11-16'), as.Date('2019-11-17'))) #-0.52

plt_2019 <- ggplot() +
  geom_line(data = CC_plot_19, aes(x = t, y = temp)) + 
  geom_line(data = CC_mextef_plot_19a, aes(x = t, y = temp), colour = "deepskyblue4", linewidth = 2) +
  geom_line(data = CC_mextef_plot_19b, aes(x = t, y = temp), colour = "deepskyblue4", linewidth = 2) +
  geom_text(data = CC_plot_19, x = as.Date('2019-11-10'), y = 12, label = "r = -0.72", colour = "deepskyblue4") +
  geom_text(data = CC_plot_19, x = as.Date('2019-11-14'), y = 10.8, label = "r = -0.63", colour = "deepskyblue4") +
  theme_pubr() + 
  labs(x = "", y = expression(paste("Temperature (", degree, "C)")))

# Arrange the plots together
ggarrange(plt_2019, plt_2020, plt_2021, plt_2022, labels = "AUTO")


# Let's try one more famous event from the literature -> 2014 (Griffin et al. 2019)
CC_plot_14 <- Cape_Cod_data |> 
  filter(lon == 289.625 & lat == 41.875) |>
  filter(between(t, as.Date('2014-10-30'), as.Date('2014-11-10')))

CC_mextef_plot_14a <- CC_plot_14 |> 
  filter(between(t, as.Date('2014-11-01'), as.Date('2014-11-02'))) #-0.93

CC_mextef_plot_14b <- CC_plot_14 |> 
  filter(between(t, as.Date('2014-11-05'), as.Date('2014-11-06'))) #-0.70

plt_2014 <- ggplot() +
  geom_line(data = CC_plot_14, aes(x = t, y = temp)) + 
  geom_line(data = CC_mextef_plot_14a, aes(x = t, y = temp), colour = "deepskyblue4", linewidth = 1.5) +
  geom_line(data = CC_mextef_plot_14b, aes(x = t, y = temp), colour = "deepskyblue4", linewidth = 1.5) +
  geom_text(data = CC_plot_14, x = as.Date('2014-11-02')+0.2, y = 12.6, label = "r = -0.93", colour = "deepskyblue4") +
  geom_text(data = CC_plot_14, x = as.Date('2014-11-06')+0.2, y = 11.3, label = "r = -0.70", colour = "deepskyblue4") +
  theme_pubr() + 
  labs(x = "", y = expression(paste("Temperature (", degree, "C)")))



# East London MEXTEF ------------------------------------------------------

  # test plots
East_London_data |> 
  filter(lon == 27.875 & lat == -33.125) |>
  filter(between(t, as.Date('2021-02-20'), as.Date('2021-03-14'))) |> 
  ggplot() +
  geom_line(aes(x = t, y = temp))


# Global SST --------------------------------------------------------------
Jan_2016 <- oisst |> 
  hyper_filter(time = between(time, 13880, 13881)) |> 
  hyper_tibble(select_var = "sst")

Jan_2016 |> 
  filter(time == 13880) |> 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = sst)) +
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")


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



# Pufferfish Event Plot ---------------------------------------------------

# Using temperature data -> find MEXTEF at:
  # False Bay (Muizenberg + Fish Hoek)
  # Late March 2021
