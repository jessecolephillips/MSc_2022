# Plot your mextef stuff with the heatwave package


# Load Packages -----------------------------------------------------------

library(heatwaveR)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)


# Data --------------------------------------------------------------------

heatwaveR::sst_NW_Atl # The same as in the other Rscript


# Calculating Clim --------------------------------------------------------

ts <- ts2clm(heatwaveR::sst_NW_Atl, climatologyPeriod = c('1982-01-01', '2011-12-31'))

# Plot same area as before
# 10/08/2018 - 15/08/2018 NW Atl
ts_slice <- ts[13356:13384,]

p1 <- ggplot(data = ts_slice, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = 'all'), show.legend = F) +
  #geom_flame(data = mhw_top, aes(y = temp, y2 = thresh, fill = 'top'), show.legend = T) +
  geom_line(aes(y = temp, colour = 'temp')) +
  geom_line(aes(y = thresh, colour = 'thresh')) +
  geom_line(aes(y = seas, colour = 'seas')) +
  scale_colour_manual(name = "Line Colour",
                      values = c('temp' = 'black',
                                 'thresh' = 'forestgreen',
                                 'seas' = 'grey80')) +
  scale_fill_manual(name = "Event Colour",
                    values = c('all' = 'salmon',
                               'top' = 'red')) +
  #scale_x_date(date_labels = '%b %Y') +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)


# 05/07/2020 - 13/07/2020 Nw Atl
ts_slice2 <- ts[14057:14085,]

p2 <- ggplot(data = ts_slice2, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = 'all'), show.legend = F) +
  #geom_flame(data = mhw_top, aes(y = temp, y2 = thresh, fill = 'top'), show.legend = T) +
  geom_line(aes(y = temp, colour = 'temp')) +
  geom_line(aes(y = thresh, colour = 'thresh')) +
  geom_line(aes(y = seas, colour = 'seas')) +
  scale_colour_manual(name = "Line Colour",
                      values = c('temp' = 'black',
                                 'thresh' = 'forestgreen',
                                 'seas' = 'grey80')) +
  scale_fill_manual(name = "Event Colour",
                    values = c('all' = 'salmon',
                               'top' = 'red')) +
  #scale_x_date(date_labels = '%b %Y') +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)


# 20/07/2019 - 25/07/2019 NW Atl
ts_slice3 <- ts[13705:13733,]

p3 <- ggplot(data = ts_slice3, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = 'all'), show.legend = F) +
  #geom_flame(data = mhw_top, aes(y = temp, y2 = thresh, fill = 'top'), show.legend = T) +
  geom_line(aes(y = temp, colour = 'temp')) +
  geom_line(aes(y = thresh, colour = 'thresh')) +
  geom_line(aes(y = seas, colour = 'seas')) +
  scale_colour_manual(name = "Line Colour",
                      values = c('temp' = 'black',
                                 'thresh' = 'forestgreen',
                                 'seas' = 'grey80')) +
  scale_fill_manual(name = "Event Colour",
                    values = c('all' = 'salmon',
                               'top' = 'red')) +
  #scale_x_date(date_labels = '%b %Y') +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)



# But also MCS ------------------------------------------------------------

ts_cold <- ts2clm(heatwaveR::sst_NW_Atl, climatologyPeriod = c('1982-01-01', '2011-12-31'), pctile = 10)

# 05/04/2017 - 12/04-2017 NW Atl
ts_slice4 <- ts_cold[12865:12893,]


p4 <- ggplot(data = ts_slice4, aes(x = t)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = 'all'), show.legend = F) + 
  #geom_flame(data = mcs_top, aes(y = thresh, y2 = temp, fill = 'top'), show.legend = T) +
  geom_line(aes(y = temp, colour = 'temp')) + 
  geom_line(aes(y = thresh, colour = 'thresh'), size = 1.0) + 
  geom_line(aes(y = seas, colour = 'seas'), size = 1.2) + 
  scale_colour_manual(name = 'Line Colour',
                      values = c('temp' = 'black','thresh' = 'forestgreen','seas' = 'grey80')) +
  scale_fill_manual(name = 'Event Colour',
                    values = c('all' = 'steelblue3', 'top' = 'navy')) + 
  #scale_x_date(date_labels = '%b %Y') + 
  guides(colour = guide_legend(override.aes = list(fill = NA))) + 
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)


# Arrange some plots ------------------------------------------------------

ggarrange(p1, p2, p3, p4, common.legend = T)
