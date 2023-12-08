#' ----------------------------------------------------------------------- #
#' PROJECT: Define Central Place Foraging trips
#' CONTENTS: 
#'  - Load libraries
#'  - Read in data
#'  - Option A = define trips using distance threshold from known CP location
#'  - Option B = define trips using shape file around CP
#'  - Visualise trip classification
#'  - Use time threshold to define foraging trips, only
#'  - Calculate & summarise trip metrics
#'  - Visualise trips
#'  - Save data
#'  DEPENDENCIES:
#'  - Requires tidyverse to be installed
#'  - Source data: df_filtered from data processing scripts
#' AUTHORS: Alice Trevail, Stephen Lang, Luke Ozsanlav-Harris, Liam Langley
#' #----------------------------------------------------------------------- #


#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(tidyverse) #installed using install.packages("tidyverse")
library(lubridate) #installed as part of the tidyverse but needs to be loaded manually
library(sf)
library(here)
library(data.table)


#--------------------------#
##0. Pre-flight checks  ####
#--------------------------#

## Data must contain individual ID, timestamp, and coordinates
## Here, we work on data formatted in the main workflow (df_filtered or df_filtered)
## If data include multiple populations, "Population" must be included as a column

## Acronym definition: 
## CP = central place
## CPLon & CPLat = Coordinates for the central place
## CPdist = Distance to colony in metres



#----------------------------------------#
##0. How to define the central place  ####
#----------------------------------------#

## In this code, there are different options depending on:
## (a) your knowledge of the central place location, and
## (b) where the central place location data are stored

## If your central place coordinates are known for each population or individual,
## use Option A = define trips using distance threshold from known CP location

## Within Option A, there are two ways to define the coordinates of the CP
## A.1. From within the metadata (e.g., individual / population CP)
##      You may have already added these to the data in the main workflow
## A.2. From known population CP coordinates, not stored in the metadata file
##      We'll add these by creating a new CP location dataframe, and merging it with the tracking data

## If the central place is defined using a shape file delineating the boundaries of the CP,
## use Option B = define trips using shape file around CP


#---------------------------#
##1. Read in data files  ####
#---------------------------#

#--------------------#
##USER INPUT START##
#--------------------#

## This script runs using df_filtered as an example
## If you have run other optional processes (e.g., df_resampled) then use your most recent data here

filepath <- here("DataOutputs","WorkingDataFrames","RFB_filtered.csv")

#-----------------#
##USER INPUT END##
#-----------------#

df_filtered <- read_csv(filepath)
names(df_filtered)

#-------------------------------------------------------------------------------------------#
## 2. Option A = define trips using distance threshold from known central place location ####
#-------------------------------------------------------------------------------------------#

## Here, we define trips using a threshold of distance from known CP coordinates
## For this, we need to know coordinates of central place for each population or individual
## This works with data from multiple populations/individual with different CPs
## skip this step if using Option B

## First, we'll add the CP locations to the tracking data
## If your data frame already containes columns CPLon and CPLat, 
## move ahead to defining distance threshold

#----------------------------------------------#
## 2. Option A.1 = CP Locations in metadata ####
#----------------------------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

## set file path to metadata
filepath_meta <- here("Data","RFB_Metadata.csv")

#------------------#
## USER INPUT END ##
#------------------#

## Read in metadata file
df_metadata <- readr::read_csv(filepath_meta)
names(df_metadata)

#--------------------#
## USER INPUT START ##
#--------------------#

## Define CP columns & coerce column names:
## CPLat = Central place latitude
## CPLon = Central place longitude

df_CPlocs_optionA1 <- data.frame(ID = df_metadata$BirdID,
                                 CPLat = df_metadata$NestLat,
                                 CPLon = df_metadata$NestLong)

#------------------#
## USER INPUT END ##
#------------------#

#--------------------------------------------------#
## 2. Option A.2 = CP Locations not in metadata ####
#--------------------------------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

# create a dataframe of population CPs.
df_CPlocs_optionA2 <- tribble(
  ~Population,  ~CPLat,    ~CPLon,
  "DG",         -7.24,     72.43,
  "NI",         -5.68,     71.24
)

#------------------#
## USER INPUT END ##
#------------------#

#------------------------------------------------------------#
## 2. Options A.1 & A2 = merge CP locs with tracking data ####
#------------------------------------------------------------#


#--------------------#
## USER INPUT START ##
#--------------------#

# Specify the column used to merge CP locations with the tracking data
# i.e., ID / Population
ID_type <- "ID"

# Specify which CPlocs dataframe to use = df_CPlocs_optionA1 or df_CPlocs_optionA2
df_CPlocs <- df_CPlocs_optionA1

#------------------#
## USER INPUT END ##
#------------------#

# This will overwrite "CPLat" and "CPLon" if already present in df_filtered
df_filtered_CPlocs  <- df_filtered %>%
  select(!any_of(c("CPLat", "CPLon", "CPdist"))) %>%
  left_join(., df_CPlocs) 


#------------------------------------------------------------------------------------------#
## 2. Options A.1 & A2 = calulate CPdist and define CP attendance by distance threshold ####
#------------------------------------------------------------------------------------------#


#--------------------#
##USER INPUT START##
#--------------------#

## Specify co-ordinate projection systems for the tracking data and CP locations
## Default here is lon/lat for both tracking data & metadata, for which the EPSG code is 4326. 
## Look online for alternative EPSG codes, e.g., https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/

tracking_crs <- 4326 # Only change if data are in a different coordinate system
CP_crs <- 4326 # Only change if data are in a different coordinate system

## Specify metric co-ordinate projection system to transform the data into for distance calculations with units in metres
## WE STRONGLY RECCOMEND CHANGING THAT YOU CHANGE THIS FOR YOUR STUDY SYSTEM/LOCATION
## Here, we use Spherical Mercator projection — aka 'WGS' (crs = 3857), used by google maps
## this can be used worldwide, and so we chose it to ensure that this workflow will work on any data
## However, distance calculations can be over-estimated
## Consider the location and scale of your data (e.g., equatorial/polar/local scale/global scale) when choosing a projection system
## Other options include (but are not limited to) UTM, Lambert azimuthal equal-area (LAEA)
## For information about choosing a projection system, see the FAQ doc in documentation folder

transform_crs <- 3857

# define distance threshold for defining CP attendance
# locations < threshold_dist will be defined as 'at CP'
# locations > threshold_dist will be defined as a trip

threshold_dist <- 1000 #distance in meters


#-----------------#
##USER INPUT END##
#-----------------#


df_filtered_CPlocs <-  df_filtered_CPlocs %>%
  ungroup() %>% #need to ungroup to extract geometry of the whole dataset
  mutate(geometry_GPS = st_transform( #assign geometry and transform to WGS for dist calcs
    st_as_sf(., coords=c("Lon","Lat"), crs=tracking_crs), crs = transform_crs)$geometry,
    geometry_CP = st_transform( #assign geometry and transform to WGS for dist calcs
      st_as_sf(., coords=c("CPLon","CPLat"), crs=CP_crs), crs = transform_crs)$geometry) %>%
  group_by(ID) %>% #back to grouping by ID for calculations per individual
  mutate(CPdist = st_distance(geometry_GPS, geometry_CP, by_element = T), #calculate distance between central place and current location
         dLon_CP = as.numeric(Lon)-CPLon, #difference in longitude between current location and central place
         dLat_CP = as.numeric(Lat)-CPLat, #difference in longitude between current location and central place
         CPbearing = atan2(dLon_CP, dLat_CP)*180/pi + (dLon_CP < 0)*360) %>% #bearing (in degrees) from central place to current location using formula theta = atan(y/x), where y = change along y axis from CP & x = change along x axis from CP
  ungroup() %>% select(-c(geometry_GPS, geometry_CP, dLon_CP, dLat_CP)) #ungroup and remove geometries


## Define points at the CP using the distance threshold and CPdist column
df_CP <- df_filtered_CPlocs %>%
  mutate(atCP = ifelse(as.numeric(CPdist) < threshold_dist, "Yes", "No")) # using distance threshold


#-----------------------------------------------------------#
## 3. Option B = define trips using shape file around CP ####
#-----------------------------------------------------------#

## Here, we define trips using shape file defining an area around the CP
## skip this step if using Option A
## For multiple populations, we rely on the shape file having attribute data specifying the population name
## This population name needs to match the name in the data, exactly, for subsequent conditional formatting

#--------------------#
##USER INPUT START##
#--------------------#

# input the file path to the shape file
shapefilepath <- here("Data","RFB_CPshape","RFB_CPshape.shp")

#-----------------#
##USER INPUT END##
#-----------------#

## this requires spatial analysis
## it is good practice to run all spatial analyses in a coordinate system with units in metres

## As an example, we will use Spherical Mercator projection (crs = 3857)
## Consider the location and scale of your data (e.g., equatorial/polar/local scale/global scale) when choosing a projection system
## Other options include (but are not limited to) UTM, Lambert azimuthal equal-area (LAEA)


## read in the shape file and transform to equal area
CP_shape <- st_read(dsn = shapefilepath, crs=4326) %>%
  st_transform(crs = 3857) %>%
  mutate(CP = "CP") # add a common column name for later indexing

## Define points at the CP if they intersect with the shape file
df_CP <- df_filtered %>% 
  st_as_sf(., coords=c("Lon","Lat"), crs = 4326, remove=FALSE) %>% 
  st_transform(crs = 3857) %>%
  st_join(., CP_shape, join = st_intersects) %>%
  mutate(atCP = ifelse(CP %in% ("CP") & .$Population.x == .$Population.y, "Yes", "No")) %>% # atCP = Yes when points intersect with the population-specific polygon
  select(-CP, -Population.y) %>% # remove common column
  rename(Population = Population.x)


#--------------------------------------#
## 4. Visualise trip classification ####
#--------------------------------------#


## Sense check of trip classification = make sure it looks right
## Working on df_CP from option A or B

## Plot colony distance over time
## we should see periods of time at the CP at the bottom of the y axis, 
## interspersed with periods away from the CP as peaks in CPdist
## N.B. check the workflow or run the test data to see what it should look like
## if it looks different with your data, check the coordinates of the central place

ggplot(df_CP, aes(x = DateTime, y = CPdist, col = atCP))+
  geom_point(cex = 0.5)+
  facet_wrap(ID~., scales = "free", ncol = 1)+ # change ncol to increase number of columns
  theme_light()

## ** Option ** ##
## Throughout these visualisations, we'll split the population into individual facets.
## This works fine on the example code, where we only have a few individuals
## If you have more individuals and the facets are too small, you can split the plot onto multiple pages
## Use the below code to use facet_wrap_paginate from the ggforce package:
# install.packages("ggforce")
# library(ggforce)
# ## save plot as object to later extract number of pages
# p <- ggplot(df_CP, aes(x = DateTime, y = CPdist, col = atCP))+
#   geom_point(cex = 0.5)+
#   # split facet by pages
#   # use ncol and nrow to change number of columns and rows per page
#   # use page to specify which page to print
#   facet_wrap_paginate(ID~., scales = "free", ncol = 1, nrow = 2, page = 1)+ 
#   theme_light()
# ## How many pages of plots?
# n_pages(p)
# ## display plot
# p
# ## run through different values of page to show each page in turn


## Plot trip durations

## to do so, number and plot all trips defined so far using spatial filters

df_trips <- df_CP %>%
  group_by(ID) %>%
  mutate(same_lag = ifelse(atCP == lag(atCP), "TRUE", "FALSE"), # is the point still at or away from CP?
         same_lead = ifelse(atCP == lead(atCP), "TRUE", "FALSE"), # is the next point leaving/returning to the CP?
         label = case_when(atCP == "Yes" & same_lead == "FALSE" ~ "first", # label last point at the CP as the first point of the 'trip'
                           atCP == "Yes" & same_lag == "FALSE" ~ "last"), # and the first point back at the CP as the last point of the 'trip'
         trip_spatial = case_when(atCP == "No" | !is.na(label) ~ "trip")) %>% # label last point at CP -> first point back as 'trip'
  filter(!is.na(trip_spatial)) %>% # remove non-trip points, as defined by spatial filters
  mutate(trip_spatial_num = case_when(label == "first" ~ cur_group_rows())) %>% # assign group number based on row number for first points of trip only
  fill(trip_spatial_num) %>% # fill NAs with group number
  group_by(trip_spatial_num) %>%
  ungroup() %>%
  group_split(ID) %>%
  purrr::map_df(~.x %>% group_by(trip_spatial_num) %>% mutate(trip_num = cur_group_id())) %>% # assign sequential trip number
  ungroup() %>% select(-c(same_lag, same_lead, label, trip_spatial, trip_spatial_num)) %>% # remove intermediate columns
  mutate(TripID = paste0(ID, "_", trip_num)) %>% # assign unique trip ID based on ID and trip number
  st_drop_geometry() # remove the geometry column


## calculate trip duration by trip

df_alltripdurations <- df_trips %>%
  group_by(Species, ID, TripID) %>%
  summarise(Trip_duration = difftime(max(DateTime), min(DateTime), units="mins"))

## plot trip durations

ggplot(df_alltripdurations, aes(x = as.numeric(Trip_duration)))+
  geom_histogram(binwidth = 5)+ # each bar = 5 minutes
  scale_x_continuous(breaks = seq(0, as.numeric(max(df_alltripdurations$Trip_duration)), 60))+ # major axis tick = 1 hour
  theme_light()+
  labs(x = "Trip duration, minutes")


#----------------------------------------------------------#
## 5. Use time threshold to define foraging trips, only ####
#----------------------------------------------------------#

## Based on trip classification visualisation, do we need a time filter?
## This can eliminate short, non-foraging trips due to e.g., disturbance at the colony

#--------------------#
##USER INPUT START##
#--------------------#

## add a threshold for time

threshold_time <- 15 #time in minutes

#-----------------#
##USER INPUT END##
#-----------------#

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

ggplot(df_trips, aes(x = DateTime, y = CPdist, col = as.factor(trip_num)))+
  geom_point(cex = 0.5)+
  facet_wrap(ID~., scales = "free", ncol = 1)+
  theme_light() +
  labs(col="Trip number")

#-------------------------------------------#
## 6. Calculate & summarise trip metrics ####
#-------------------------------------------#

## Make two data frames:
## 1. a trip-level file, where each trip has a unique row
## 2. a population-level file, to summarise trip metrics

## function to calculate standard error
se <- function(x) sqrt(sd(x, na.rm = T) / length(x[!is.na(x)]))


#--------------------#
##USER INPUT START##
#--------------------#

sampleRateUnits <- "mins" # units to display sample rate in table

## define levels of grouping factors

## Firstly, down to population level
## here, we are working on data from one population & year, and so use 'Species' and 'Population'
## add any other relevant grouping factors here, e.g., Country / Year / Season / Age

grouping_factors_poplevel <- c("Species", "Population") 

## Secondly, down to individual level
## add e.g., DeployID if relevant

grouping_factors_indlevel <- c("ID")

#-----------------#
##USER INPUT END##
#-----------------#

## trip metrics calculated for each trip = 1 row per trip

df_tripmetrics <- df_trips %>%
  group_by(across(all_of(c(grouping_factors_poplevel, grouping_factors_indlevel, "TripID")))) %>% 
  mutate(cum_dist = cumsum(dist)) %>%
  summarise(Tripstart = min(DateTime),
            Tripend = max(DateTime),
            Completetrip = ifelse(atCP[which.max(DateTime)] == "Yes", "Complete", "Incomplete"), # Complete if  last point is back at the CP
            Trip_duration = difftime(max(DateTime), min(DateTime), units="mins"),
            Total_distance = sum(dist),
            Max_distance = max(CPdist),
            Distal_lon = Lon[which.max(CPdist)],
            Distal_lat = Lat[which.max(CPdist)],
            CPLon = CPLon[1],
            CPLat = CPLat[1],
            Distal_bearing = CPbearing[which.max(CPdist)], #if CPbearing not in df, use Distal_bearing = atan2(Distal_lon-CPLon, Distal_lat-CPLat)*180/pi + (Distal_lon-CPLon < 0)*360
            Speed_mean = mean(speed),
            Speed_se = se(speed),
            Straightness_outbound = max(CPdist)/cum_dist[which.max(CPdist)], # Straightness on outbound leg = ratio of straight line distance to cumulative difference traveled to max distance from CP
            Straightness_inbound = max(CPdist)/(cum_dist[n()]-cum_dist[which.max(CPdist)])) # Straightness on inbound leg = ratio of straight line distance to cumulative difference traveled back from max distance from CP
df_tripmetrics

## intermediate = individual level summary = 1 row per individual
## each individual will therefore have equal weighting in the population summary

df_tripmetrics_ind <- df_tripmetrics %>%
  group_by(across(all_of(c(grouping_factors_poplevel, grouping_factors_indlevel)))) %>% 
  summarise(Trip_duration_ind_mean = mean(as.numeric(Trip_duration, units = sampleRateUnits), na.rm = T),
            Trip_duration_ind_se = se(as.numeric(Trip_duration, units = sampleRateUnits)),
            Total_distance_ind_mean = mean(Total_distance),
            Total_distance_ind_se = se(Total_distance),
            Max_distance_ind_mean = mean(Max_distance),
            Max_distance_ind_se = se(Max_distance),
            N_Trips = length(unique(TripID))) 
df_tripmetrics_ind

## population level summary = 1 row per population

df_tripmetrics_summary_pop <- df_tripmetrics_ind %>%
  group_by(across(all_of(grouping_factors_poplevel))) %>%
  summarise(Trip_duration_mean = mean(Trip_duration_ind_mean),
            Trip_duration_se = se(Trip_duration_ind_mean),
            Total_distance_mean = mean(Total_distance_ind_mean),
            Total_distance_se = se(Total_distance_ind_mean),
            Max_distance_mean = mean(Max_distance_ind_mean),
            Max_distance_se = se(Max_distance_ind_mean),
            N_Trips_total = sum(N_Trips),
            N_Trips_perInd_mean = mean(N_Trips),
            N_Trips_perInd_se = se(N_Trips))
df_tripmetrics_summary_pop



#------------------------#
## 7. Visualise trips ####
#------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

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

species_code <- "RFB"

## We plot maps over a topography base-layer
## topography data include terrestrial (elevation) and marine (bathymetry/water depth) data
## set legend label for topography data, relevant to your data:

topo_label = "Depth (m)"

#------------------#
## USER INPUT END ##
#------------------#

## load additional libraries for spatial visualisation
## optional step - could be removed/improved later!!

message("If you see masking warning these are fine. 
        Watch out for packages that aren't installed yet")
library(rnaturalearth)
library(marmap)
library(plotly)

## create a map of all points
## set the plot limits as the max and min lat/longs as the tracking data

## first set up a basemap to plot over

## use rnatural earth low res countries basemap
## co-ordinates in lat/long
## matches other spatial data
## probably improve this later!!!

countries <- ne_countries(scale = "medium", returnclass = "sf")

## define min and max co-ordinates based on extent of tracking data
minlon <- min(df_trips$Lon)
maxlon <- max(df_trips$Lon)

minlat <- min(df_trips$Lat)
maxlat <- max(df_trips$Lat)

## load in topography basemap
## set limits slightly beyond tracking data to make a buffer
## so no gaps when plotting

base_topography_map <- getNOAA.bathy(lon1 = minlon - 0.1, lon2 = maxlon + 0.1,
                                     lat1 = minlat - 0.1, lat2 = maxlat + 0.1, resolution = 1)

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
  geom_point(data = df_trips, aes(x = CPLon, y = CPLat),
             colour = "#FF3300", fill ="#FF3300", shape = 23, size = 2) +
  # set plot limits
  coord_sf(xlim = c(minlon, maxlon), ylim = c(minlat, maxlat), crs = 4326) +
  # add labels
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text=element_text(colour="black"),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(hjust=0.7),
        axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
        axis.text.y = element_text(hjust=0.7,angle=90,vjust=0.3))+
  theme_light()

## map individual locations
## colour points by trip number
## facet by ID

map_trips <- map_base + 
  # add GPS points and paths between them
  geom_point(data = df_trips, aes(x = Lon, y = Lat, col = as.factor(trip_num)), alpha = 0.8, size = 0.5 ) +
  geom_path(data = df_trips, aes(x = Lon, y = Lat, col = as.factor(trip_num)), alpha = 0.8, size = 0.5 ) +
  ##facet for individual
  facet_wrap(~ ID, ncol = ceiling(sqrt(n_distinct(df_filtered$ID)))) +
  labs(col = "Trip Number")
map_trips

## save map for further use
## use ggsave function

ggsave(plot = map_trips, filename = paste0(species_code, "_map_trips.tiff"),
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)

## Plot trip metrics at population level ##
## Here, we plot max distance, total distance and trip duration 

## create long dataframe for ggplot

df_tripmetrics_long <- df_tripmetrics %>%
  mutate(Trip_duration = Trip_duration/dhours(1), # convert trip duration (mins) to hours
         across(c(Trip_duration, Total_distance, Max_distance), as.numeric),
         across(c(Total_distance, Max_distance), function(x) x/1000)) %>% # convert distances (m) to km
  unite(., col = pop_code, all_of(grouping_factors_poplevel), sep = "_", remove = F) %>% # create pop_code based on levels of grouping_factors_poplevel
  pivot_longer(cols = c(Trip_duration, Total_distance, Max_distance), # convert to long dataframe
               names_to = "Metric", values_to = "Metric_value")

## Full names with units for labelling trip metrics

labs_metrics <- c("Max_distance" = "Max distance (km)", 
                  "Total_distance" = "Total distance (km)", 
                  "Trip_duration" = "Trip duration (hrs)")

## create box plot for each metric & level of pop_code

plot_tripmetrics <- ggplot(df_tripmetrics_long, aes(x = pop_code, y = Metric_value)) +
  facet_wrap(.~Metric, scales = "free_y", labeller = labeller(Metric = labs_metrics)) +
  geom_boxplot()+
  labs(x = "Population", y = "Value") +
  theme_light()


## save figure

ggsave(plot = plot_tripmetrics, filename = paste0(species_code, "_pop_tripmetrics.tiff"),
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)



#------------------#
## 8. Save data ####
#------------------#

## ** Option ** ##
## In this script, we have removed all points from the data frame other than those during foraging trips, specifically.
## This means that we have removed all points from the CP, and all points from non-foraging trips
## This goes against natural instincts of many of us to hoard data - what if we need it after all?
## However, there are many good reasons to work this way, e.g.;
#'  - we still have non-foraging points in our original raw data files, and intermediate df_filtered file
#'  - by keeping well annotated workflows, we can quickly and easily re-run these processing steps and 'restore' non-foraging data
#'  - we can reduce the file size of data
#'  - therefore, reducing back up requirements
#'  - & reducing the carbon cost of file storage
#'  - we have kept our ecologically meaningful foraging data
#'  - this has kept the data processing above more simple, with less conditional formatting
## Depending on your research question, you might want to keep all original points
## If so, run the following code to join trip information to df_filtered, and modify the code below to save this file, instead:

# df_filtered_withTripIDs <- df_filtered %>%
#   left_join(., df_trips)

## Similarly, we will save the trip metrics separately for now to keep the file size small
## If you would like to merge trip metrics with the tracking data (i.e., annotate each row of df_trips with df_tripmetrics), run the following code

# df_trips_withmetrics <- df_trips %>%
#   left_join(., df_tripmetrics)

#--------------------#
##USER INPUT START##
#--------------------#

## Define species code

species_code <- "RFB"

## file path & name for df_trips

filepath_dftrips_out <- here("DataOutputs","WorkingDataFrames") 
filename_dftrips_out <- paste0(species_code, "_trips")

## file path & name for df_tripmetrics and df_tripmetrics_summary_pop

filepath_summary_out <- here("DataOutputs","SummaryDataFrames")
filename_tripmetrics_out <- paste0(species_code, "_tripmetrics")
filename_tripmetrics_summary_pop_out <- paste0(species_code, "_tripmetrics_summary_pop")

#-----------------#
##USER INPUT END##
#-----------------#

## save dataframes as .csv files

write_csv(df_trips, file = here(filepath_dftrips_out, paste0(filename_dftrips_out, ".csv")))
write_csv(df_tripmetrics, file = here(filepath_summary_out, paste0(filename_tripmetrics_out, ".csv")))
write_csv(df_tripmetrics_summary_pop, file = here(filepath_summary_out, paste0(filename_tripmetrics_summary_pop_out, ".csv")))



#---------------------------------#
#### Question for the user ####
#---------------------------------#

## How do thresholds for defining trips effect spatial distributions & trip metrics?



