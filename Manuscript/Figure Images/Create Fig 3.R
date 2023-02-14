## Create figure 3 for the manuscript


#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(tidyverse) #installed using install.packages("tidyverse")
library(lubridate) #installed as part of the tidyverse but needs to be loaded manually
library(sf)
library(here)
library(data.table)
library(rnaturalearth)
library(marmap)
library(ggpubr)
library(cowplot)


#-----------------------------#
##1. Read in RFB data set  ####
#-----------------------------#

## read in data
filepath <- here("DataOutputs","WorkingDataFrames","RFB_filtered.csv")
df_diagnostic <- read_csv(filepath)



#-----------------------------------------------#
## 2. define trips using distance threshold  ####
#-----------------------------------------------#

## Here, we define trips using a threshold of distance from the CP
## This code will work based on distance calculated in the main workflow
## This works with data from multiple populations with different CPs if specified in main workflow
## skip this step if using Option B or C

threshold_dist <- 1000 #distance in meters

## Define points at the CP using the distance threshold and CPdist column
df_CP <- df_diagnostic %>%
  mutate(atCP = ifelse(CPdist < threshold_dist, "Yes", "No")) # using distance threshold




#----------------------------------------------------------#
## 3. Use time threshold to define foraging trips, only ####
#----------------------------------------------------------#

## add a threshold for time
threshold_time <- 15 #time in minutes


## Now, re-make df_trips with foraging trips, only
## re-do the trip numbering to avoid confusion
df_trips <- df_CP %>%
  group_by(ID) %>%
  mutate(same_lag = ifelse(atCP == lag(atCP), "TRUE", "FALSE"), # is the point still at or away from CP?
         same_lead = ifelse(atCP == lead(atCP), "TRUE", "FALSE"), # is the next point leaving/returning to the CP?
         label = case_when(atCP == "Yes" & same_lead == "FALSE" ~ "first", # label last point at the CP as the first point of the 'trip'
                           atCP == "Yes" & same_lag == "FALSE" ~ "last"), # and the first point back at the CP as the last point of the 'trip'
         trip_spatial = case_when(atCP == "No" | !is.na(label) ~ "trip")) %>% # label last point at CP -> first point back as 'trip'
  filter(!is.na(trip_spatial)) %>% # remove non-trip points, as defined by distance
  mutate(trip_spatial_num = ifelse(label == "first", cur_group_rows(), NA)) %>% # assign group number based on row number for first points of trip only
  fill(trip_spatial_num) %>% # fill NAs with group number
  group_by(trip_spatial_num) %>%
  mutate(trip_dur = difftime(max(DateTime), min(DateTime), units="mins")) %>% # calculate trip duration
  filter(trip_dur >= threshold_time) %>% # only keep trips longer than threshold duration
  ungroup() %>%
  group_split(ID) %>%
  purrr::map_df(~.x %>% group_by(trip_spatial_num) %>% mutate(trip_num = cur_group_id())) %>% # assign sequential trip number
  ungroup() %>% select(-c(same_lag, same_lead, label, trip_spatial, trip_spatial_num, trip_dur)) %>% # remove intermediate columns
  mutate(TripID = paste0(ID, "_", trip_num)) %>% # assign unique trip ID based on ID and trip number
  st_drop_geometry() # remove the geometry column

## let's plot trips over time again
## we should see no short trips, and trips are coloured by number
# ggplot(df_trips, aes(x = DateTime, y = CPdist, col = as.factor(trip_num)))+
#   geom_point(cex = 0.5)+
#   facet_wrap(ID~., scales = "free", ncol = 1)+
#   theme_light() +
#   labs(col="Trip number")



#----------------------------------#
## 4. Make plot of foaring trip ####
#----------------------------------#

## define units for plot size - usually mm
units <- "mm"

## define plot resolution in dpi - 300 usually minimum
dpi <- 300

## We plot maps over a topography base-layer
## topography data include terrestrial (elevation) and marine (bathymetry/water depth) data
## set legend label for topography data, relevant to your data:
topo_label = "Depth (m)"


## Filter the data that I want to plot
df_tripsub <- filter(df_trips, TripID == "GV37734_4")
df_tripsub <- df_tripsub %>% filter(row_number() %% 2 == 1) ## Select even rows


## create a map of all points
## set the plot limits as the max and min lat/longs as the tracking data

## first set up a basemap to plot over
## use rnatural earth low res countries basemap
countries <- ne_countries(scale = "medium", returnclass = "sf")

## define min and max co-ordinates based on extent of tracking data
minlon <- min(df_tripsub$Lon)
maxlon <- max(df_tripsub$Lon)
minlat <- min(df_tripsub$Lat)
maxlat <- max(df_tripsub$Lat)

## load in topography basemap
## set limits slightly beyond tracking data to make a buffer
base_topography_map <- getNOAA.bathy(lon1 = minlon - 0.05, lon2 = maxlon + 0.05,
                                     lat1 = minlat - 0.05, lat2 = maxlat + 0.05, resolution = 1)

## fortify for plotting
base_topography_fort = fortify(base_topography_map)


## create base map with correct extent, topography, country outlines, etc.,
map_base <- ggplot() + 
  geom_raster(data = base_topography_fort, aes(x=x, y=y, fill=z), alpha = 0.9) +
  # add colour scheme for the fill
  scale_fill_viridis_c(option="mako", name = topo_label) + 
  # add map of countries over the top
  geom_sf(data = countries, aes(geometry = geometry), fill = NA) + 
  # add central place points
  geom_point(data = df_tripsub, aes(x = CPLon, y = CPLat),
             colour = "black", fill ="#979A9A", shape = 23, size = 5) +
  # set plot limits
  coord_sf(xlim = c(minlon-0.05, maxlon+0.05), ylim = c(minlat-0.05, maxlat+0.05), crs = 4326) +
  # add labels
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text=element_text(colour="black"),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(hjust=0.7),
        axis.title.y = element_text(angle=90, vjust = 0.4, size = 16),
        axis.text.y = element_text(hjust=0.7,angle=90,vjust=0.3))+
  theme_light()

## map individual locations, colour points by trip number, facet by ID
## Create data set for line between 
map_trips <- map_base + 
  # add GPS points and paths between them
  geom_line(aes(x = c(df_tripsub$Lon[6], df_tripsub$Lon[6]), y = c(df_tripsub$Lat[6], df_tripsub$Lat[6]+0.05)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "open")) +
  geom_curve(aes(xend = df_tripsub$Lon[6], yend = df_tripsub$Lat[6]+0.02, x = 72.45, y = -5.603), colour = "black", curvature = 1, size = 0.8, alpha = 0.6) +
  geom_path(data = df_tripsub, aes(x = Lon, y = Lat), colour = "#979A9A", alpha = 0.6, size = 1) +
  geom_point(data = df_tripsub, aes(x = Lon, y = Lat), col = "#CB4335", alpha = 0.9, size = 1.5) +
  geom_line(aes(x = c(df_tripsub$Lon[6], df_tripsub$CPLon[6]), y = c(df_tripsub$Lat[6], df_tripsub$CPLat[6])),
            colour = "black", alpha = 0.6, size = 1, arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "open")) +
  annotate("text", x = 72.48, y = -5.615, label = "v)", size = 5) +
  annotate("text", x = 72.425, y = -5.64, label = "iv)", size = 5)
map_trips




## Now make a plot for cumulative distance and one for distance from central place ##

## First make a column for cumulative distance
df_tripsub <- df_tripsub %>% mutate(CumDist = (cumsum(dist))/1000)
# glimpse(df_tripsub)

## Cumulative distance over time plot
Cumdist_time_plot <- df_tripsub %>% #step length over time
  ggplot(data=., aes(x=DateTime, y=as.numeric(CumDist))) +
  geom_line(size = 1.5, alpha = 0.5, colour = "#979A9A") +
  geom_point(size = 1.5) +
  xlab("Time") + ylab("Cumulative distance/km") +
  theme_light() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size =10, colour = "#424949"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  theme(axis.text=element_text(colour="black"))
  
# Cumdist_time_plot

## Distance from centrla place over time plot
CPdist_time_plot <- df_tripsub %>% #step length over time
  ggplot(data=., aes(x=DateTime, y=as.numeric(CPdist)/1000)) +
  geom_line(size = 1.5, alpha = 0.5, colour = "#979A9A") +
  geom_point(size = 1.5) +
  xlab("Time") + ylab("Distance to colony/km") +
  theme_light() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size =10, colour = "#424949"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
theme(axis.text=element_text(colour="black"))

# CPdist_time_plot


## arrange the plot
Pt2 <- ggdraw() +
  draw_plot(CPdist_time_plot, x = 0, y = 0, width = 0.4, height = 0.3) +
  draw_plot(Cumdist_time_plot, x = .5, y = 0, width = 0.4, height = 0.3) +
  draw_plot(map_trips, x = 0, y = 0.3, width = 1, height = 0.7) +
  draw_plot_label(label = c("D", "F", "E"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 0.38, 0.38))
## Save the plot
ggsave(plot = Pt2 , 
       filename = "C:/Users/lo288/OneDrive - University of Exeter/Documents/MoveExplore/Code-workshop/Manuscript/Figure Images/Figure_3_pt2.png",
       units = units, width = 200, height = 175, dpi = dpi,   
)





#-----------------------------------#
## 5. Read in GWFG tracking data ####
#-----------------------------------#

## Read in the data
filepath <- "DataOutputs/WorkingDataFrames/GWFG_filtered.csv"
df_GW <- read_csv(filepath)
df_GW <- df_GW %>% filter(ID == 17766) %>% filter(Lon > -32 & Date >= "2019-03-25" & Date <= "2019-03-27")
df_GW <- df_GW %>% filter(row_number() %% 2 == 1) 
# NotSamp <- df_diagnostic %>% filter(ID == 17769)



#-------------------------------------------#
## 6. Create map of the chosen locations ####
#-------------------------------------------#

## define units for plot size - usually mm
units <- "mm"

## define plot resolution in dpi - 300 usually minimum
dpi <- 300

## We plot maps over a topography base-layer
## topography data include terrestrial (elevation) and marine (bathymetry/water depth) data
## set legend label for topography data, relevant to your data:
topo_label = "Elevation (m)"

## version of data for plotting, transform required columns to numeric
df_plotting <- df_GW %>%
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
           ylim = c(minlat- 0.5, maxlat+ 0.45), crs = 4326, expand = F) +
  # add labels
  labs(x = "Longitude", y = "Latitude") +
  theme_light() +
  theme(axis.text=element_text(colour="black"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(hjust=0.7, size = 12, colour = "#424949"),
        axis.title.y = element_text(angle=90, vjust = 0.4, size = 14),
        axis.text.y = element_text(hjust=0.7, angle=90, vjust=0.3, size = 12,  colour = "#424949"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))


## map all tracking locations
## Define locations of the point to annotate with metrics
# step length point
SL <- 10
TA <- 14
BA <- 17

## calcualte curvature for bearing
BearingX <- ((df_plotting$Lon[BA]-df_plotting$Lon[BA+1])/2)+df_plotting$Lon[BA+1]
BearingY <- ((df_plotting$Lat[BA+1]-df_plotting$Lat[BA])/2)+ df_plotting$Lat[BA]

## curve for the turning angle
TA_X1 <- ((df_plotting$Lon[TA]-df_plotting$Lon[TA+1])/2)+df_plotting$Lon[TA+1]
TA_Y1 <- ((df_plotting$Lat[TA+1]-df_plotting$Lat[TA])/2)+ df_plotting$Lat[TA]

TA_X2 <- ((df_plotting$Lon[TA]-df_plotting$Lon[TA-1])/2)+df_plotting$Lon[TA-1]
TA_Y2 <- ((df_plotting$Lat[TA]-df_plotting$Lat[TA-1])/2)+ df_plotting$Lat[TA-1]

map_alllocs <- map_base + 
  # add GPS points
  geom_path(data = df_plotting, aes(x = Lon, y = Lat, group = ID), colour = "#979A9A", alpha = 0.6, size = 1.5) +
  geom_point(data = df_plotting, aes(x = Lon, y = Lat), col = "#CB4335", alpha = 0.9, size = 2) +
  ## add the bearing lines
  geom_line(aes(x = c(df_plotting$Lon[BA], df_plotting$Lon[BA]), y = c(df_plotting$Lat[BA], df_plotting$Lat[BA]+0.8)),
                      colour = "black", alpha = 0.6, size = 1.2, arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "open")) +
  geom_curve(aes(xend = df_plotting$Lon[BA], yend = df_plotting$Lat[BA]+0.25, x = BearingX, y = BearingY), colour = "black", curvature = 0.5, size = 1.2, alpha = 0.6)+ 
  annotate("text", x = -4.6, y = df_plotting$Lat[BA], label = "iii)", size = 5) +
  ## add the curve for the turning angle
  geom_curve(aes(x = TA_X2, y = TA_Y2, xend = TA_X1, yend = TA_Y1), colour = "black", curvature = 0.4, size = 1.2, alpha = 0.6) +
  annotate("text", x = -6.45, y = df_plotting$Lat[TA], label = "ii)", size = 5) +
  ## add the line for the step length
  geom_line(aes(x = c(df_plotting$Lon[SL], df_plotting$Lon[SL+1]), y = c(df_plotting$Lat[SL], df_plotting$Lat[SL+1])),
            colour = "black", alpha = 0.6, size = 1.2) +
  annotate("text", x = -6.3, y = ((df_plotting$Lat[SL]-df_plotting$Lat[SL+1])/2.8+df_plotting$Lat[SL+1]), label = "i)", size = 5)
map_alllocs



## Now make the smaller plots with metrics over time shown

## Cumulative distance over time plot
Netdisp_time_plot <- df_plotting %>% #step length over time
  ggplot(data=., aes(x=DateTime, y=as.numeric(netdisp)/1000)) +
  geom_line(size = 1.5, alpha = 0.5, colour = "#979A9A") +
  geom_point(size = 1.5) +
  xlab("Date") + ylab("Net Disp/km") +
  theme_light() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size =10, colour = "#424949"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
theme(axis.text=element_text(colour="black"))

# Netdisp_time_plot

## Distance from centrla place over time plot
Speed_time_plot <- df_plotting %>% #step length over time
  ggplot(data=., aes(x=DateTime, y=as.numeric(speed))) +
  geom_line(size = 1.5, alpha = 0.5, colour = "#979A9A") +
  geom_point(size = 1.5) +
  xlab("Date") + ylab(expression(Speed/ms^-1)) +
  theme_light() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size =10, colour = "#424949"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
theme(axis.text=element_text(colour="black"))

# Speed_time_plot



## Combine the plots
Pt1 <- ggdraw() +
  draw_plot(Netdisp_time_plot, x = 0, y = 0, width = 0.4, height = 0.3) +
  draw_plot(Speed_time_plot, x = .5, y = 0, width = 0.4, height = 0.3) +
  draw_plot(map_alllocs, x = 0, y = 0.3, width = 1, height = 0.7) +
  draw_plot_label(label = c("A", "C", "B"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 0.32, 0.32))
# Pt1



## Save the plot
ggsave(plot = Pt1 , 
       filename = "C:/Users/lo288/OneDrive - University of Exeter/Documents/MoveExplore/Code-workshop/Manuscript/Figure Images/Figure_3_pt1.png",
       units = units, width = 200, height = 175, dpi = dpi,   
)