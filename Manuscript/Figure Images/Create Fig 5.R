## Create Figure 5 for manuscript


#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(tidyverse) #installed using install.packages("tidyverse")
library(lubridate) #installed as part of the tidyverse but needs to be loaded manually
library(here) #for reproducible filepaths
library(rnaturalearth) # for plotting maps
library(marmap) # for plotting amps
library(ggpubr)


#--------------------------#
##0. Pre-flight checks  ####
#--------------------------#

## Data must contain individual ID, timestamp, and coordinates
## Here, we work on data formatted and filtered in the main processing workflow
## The data frame we read in is df_filtered produced in step 4

#---------------------------#
##1. Read in data files  ####
#---------------------------#

#--------------------#
##USER INPUT START##
#--------------------#

filepath <- "DataOutputs/WorkingDataFrames/GWFG_filtered.csv"

#-----------------#
##USER INPUT END##
#-----------------#

df_diagnostic <- read_csv(filepath)
Samp <- df_diagnostic %>% filter(ID == 17824)
NotSamp <- df_diagnostic %>% filter(ID == 17769)


#---------------------#
##2. Sub-sampling  ####
#---------------------#


## might want to sub-sample the tracking data to a regular time interval
## often required to compare movement metrics from tags with different sampling intervals
## NOTE: if sub-sampling to an interval greater then 60 minutes, e.g 2 hours,
##       then change the unit to "hours" and resolution to 2. Do not use 120 "minutes". 


## set time unit for sub-sampling

subsampling_unit <- "hours"

## set the resolution to subsample to

subsampling_res <- 2


## pipe to subsample tracking data to a defined interval
## need to group by bird to subsample each individual separately

df_subsampled <- Samp %>%
  group_by(ID) %>%
  mutate(subsample = round_date(DateTime, unit = period(num = subsampling_res, units = subsampling_unit)), #round to nearest value of subsampling unit
         accuracy = as.period(interval(DateTime, subsample)/ seconds(1)),.after=DateTime, #accuracy of round (Date-subsample) in secs
         accuracy = abs(accuracy)) %>% #convert to absolute values (no negatives)
  group_by(ID, subsample) %>% #group by ID and subsample for removing duplicates
  slice_min(accuracy) %>% #slice out subsample duplicates with highest accuracy
  ungroup() %>% 
  mutate(difftime = difftime(DateTime, lag(DateTime), units="secs")) %>% 
  select(-c(subsample, accuracy))




#-----------------------------#
##3. Combine two data sets ####
#-----------------------------#

## Add on the individual that was not resampled
df_comb <- rbind(df_subsampled, NotSamp)



#---------------------------------------------#
##4. Create Histogram of sampling interval ####
#---------------------------------------------#


## maximum sampling rate
maxint <- as.numeric(max(as.numeric(df_comb$difftime), na.rm = T))

## plot sampling intervals
SubSamphist <- ggplot(df_comb, aes(x = as.numeric((difftime/60), na.rm=T)))+
                geom_histogram(bins = 30, fill = "#979A9A")+ # each bar = 5 minutes
                #scale_x_continuous(n.breaks = 20)+ # major axis tick = 1 hour
                theme_light()+
                xlim(30, 140) +
                geom_vline(xintercept = 120, linetype = "dashed", size = 1.5)+
                theme(axis.title = element_text(size = 14), axis.text = element_text(size =12, colour = "#424949")) +
                xlab("Sampling Interval (mins)") + ylab("Frequency")

SubSamphist



#----------------------------------------#
##5. Resample all new data to 2 hours ####
#----------------------------------------#

## resample the 2 hour and 1 hour data to 2 hours
df_subsamp2 <- df_comb %>%
  group_by(ID) %>%
  mutate(subsample = round_date(DateTime, unit = period(num = subsampling_res, units = subsampling_unit)), #round to nearest value of subsampling unit
         accuracy = as.period(interval(DateTime, subsample)/ seconds(1)),.after=DateTime, #accuracy of round (Date-subsample) in secs
         accuracy = abs(accuracy)) %>% #convert to absolute values (no negatives)
  group_by(ID, subsample) %>% #group by ID and subsample for removing duplicates
  slice_min(accuracy) %>% #slice out subsample duplicates with highest accuracy
  select(ID, DateTime) %>% 
  ungroup() %>%
  select(-subsample) %>% 
  mutate(subsamp = 1)


## Now add to the main data set to work out which point were rmeoevd and which ones kept
df_plot <- left_join(df_comb, df_subsamp2, by = c("ID", "DateTime")) %>% 
                      mutate(subsamp = ifelse(is.na(subsamp) == T, 0, subsamp))

## crop the data so we only get fixes up to Icleland
df_plot2 <- df_plot %>% filter(Lon > -25)




#-------------------------------------#
##6. Create plot of resampled data ####
#-------------------------------------#

## Define parameters for reading out plots
## Define device to read plots out as e.g. tiff/jpeg
device <- "tiff"

## define units for plot size - usually mm
units <- "mm"

## define plot resolution in dpi - 300 usually minimum
dpi <- 300

## define filepath to read out plots 
out_path <- here("DataOutputs","Figures")

## Define species code for figure names
species_code <- "GWFG"

## We plot maps over a topography base-layer
## topography data include terrestrial (elevation) and marine (bathymetry/water depth) data
## set legend label for topography data, relevant to your data:
topo_label = "Elevation (m)"

## version of data for plotting
## transform required columns to numeric
## create time elapsed columns

df_plotting <- df_plot2 %>%
  group_by(ID) %>%
  mutate(diffsecs = as.numeric(difftime),
         secs_elapsed = cumsum(replace_na(diffsecs, 0)),
         time_elapsed = as.duration(secs_elapsed),
         days_elapsed = as.numeric(time_elapsed, "days")) %>%
  mutate(across(c(dist,speed, Lat, Lon), as.numeric))

## create a map of all points
## set the plot limits as the max and min lat/longs as the tracking data

## first set up a basemap to plot over

## use rnatural earth low res countries basemap
## co-ordinates in lat/long
## matches other spatial data
## probably improve this later!!!
countries <- ne_countries(scale = "medium", returnclass = "sf")

## define min and max co-ordinates based on extent of tracking data
minlon <- min(df_plotting$Lon)
maxlon <- max(df_plotting$Lon)

minlat <- min(df_plotting$Lat)
maxlat <- max(df_plotting$Lat)

## load in topography basemap
## set limits slightly beyond tracking data to make a buffer
## so no gaps when plotting
base_topography_map <- getNOAA.bathy(lon1 = minlon - 3, lon2 = maxlon + 3,
                                     lat1 = minlat - 2, lat2 = maxlat + 2, resolution = 1)

## fortify for plotting
base_topography_fort = fortify(base_topography_map)

## create base map with correct extent, topography, country outlines, etc.,
map_base <- ggplot() + 
  geom_raster(data = base_topography_fort, aes(x=x, y=y, fill=z), alpha = 0.9) +
  # add colour scheme for the fill
  scale_fill_viridis_c(option="mako", name = topo_label) + 
  # add map of countries over the top
  geom_sf(data = countries, aes(geometry = geometry), fill = NA) + 
  # set plot limits
  coord_sf(xlim = c(minlon- 3, maxlon+ 3), 
           ylim = c(minlat- 2, maxlat+ 2), crs = 4326, expand = F) +
  # add labels
  labs(x = "Longitude", y = "Latitude") +
  theme_light() +
  theme(axis.text=element_text(colour="black"),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(hjust=0.7, size = 12, colour = "#424949"),
        axis.title.y = element_text(angle=90, vjust = 0.4, size = 16),
        axis.text.y = element_text(hjust=0.7, angle=90, vjust=0.3, size = 12,  colour = "#424949"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))
  


## map all tracking locations
df_plotting$subsamp <- ifelse(df_plotting$subsamp == 1, "Retained", "Removed")
df_plotting$subsamp <- as.factor(df_plotting$subsamp)

map_alllocs <- map_base + 
  # add GPS points
  geom_path(data = df_plotting, aes(x = Lon, y = Lat, group = ID), colour = "white", alpha = 0.8, size = 0.5) +
  geom_point(data = df_plotting, aes(x = Lon, y = Lat, colour = subsamp, alpha = subsamp), size = 2.5) + 
  scale_color_manual(values = c("#BDC3C7", "#CB4335")) +
  scale_alpha_manual(values = c(0.6, 1)) +
  labs(colour="Resampling") +
  guides(colour = "legend", alpha = "none")
#map_alllocs


## Now arrange the map and the histogram together
Comboplot <- ggarrange(SubSamphist, map_alllocs, labels="auto", widths = c(80, 100))
Comboplot

ggsave("C:/Users/lo288/OneDrive - University of Exeter/Documents/MoveExplore/Code-workshop/Manuscript/Figure Images/Figure_5.png",
       width = 32, height = 15, units = "cm")







