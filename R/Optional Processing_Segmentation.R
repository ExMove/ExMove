#' ----------------------------------------------------------------------- #
#' PROJECT: Define segments where data are interspersed by gaps
#' CONTENTS: 
#'  - Load libraries
#'  - Read in data
#'  - Define segments using time threshold
#'  - Map segments
#'  - Summarise segments
#'  - Save data
#'  DEPENDENCIES:
#'  - Requires tidyverse to be installed
#'  - Source data: df_diagnostic from data processing scripts
#' AUTHORS: Alice Trevail, Stephen Lang, Luke Ozsanlav-Harris, Liam Langley
#' #----------------------------------------------------------------------- #


#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(tidyverse) #installed using install.packages("tidyverse")
library(lubridate) #installed as part of the tidyverse but needs to be loaded manually
library(sf)
library(here)


#--------------------------#
##0. Pre-flight checks  ####
#--------------------------#

## Data often contain gaps, because of e.g., satellite fix errors or removing equinox periods 
## We might want to define segments to avoid interpolation of data over gaps
## E.g., when running state space models
## This code uses a time threshold to number segments of data either side of a gap

## Data must contain individual ID and a difftime column with time difference between each row 
## Here, we work on data formatted and filtered in the main workflow (df_filtered)
## Refer to the main workflow if your data don't include time difference yet

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

df_diagnostic <- read_csv(filepath)



#-------------------------------------#
## 2. Split tracks into segments  ####
#-------------------------------------#

## Here, we segments where gaps in data exceed a time threshold
## This code will work based on time differences calculated in the main workflow

#--------------------#
##USER INPUT START##
#--------------------#

## set units of time difference column (difftime) in the tracking data
## difftime will be recalculated in the next section of the code as they may have changed after filtering
## Other options include "secs", "hours", "days", "weeks"
units_df_datetime <- "mins"

# define maximum gap in data using time threshold
# If the interval between locaiton exceeds this value then the track will be split into separate segments
threshold_time <- as.period(15, unit = "mins")

#-----------------#
##USER INPUT END##
#-----------------#

## Re-calcualte the time differences between consecutive locations
## These may have changes since the first calculation due to filtering
df_diagnostic <-  df_diagnostic %>%
  ungroup() %>% # make sure data set has no existing grouping
  group_by(ID) %>% # grouping by ID for calculations per individual
  mutate(difftime = difftime(DateTime, lag(DateTime), units= units_df_datetime)) %>%  # time passed since previous fix
  ungroup() 


## Plot histogram of sampling intervals to inform choice of threshold_time user input value
## Dashed line represents the threshold_time value chosen by the user
Samphist <- ggplot(df_diagnostic, aes(x = as.numeric((difftime), na.rm=T))) +
  geom_histogram(bins = 50) + # how many bins the histogram should have
  geom_vline(xintercept = as.numeric(threshold_time, units_df_datetime), linetype = "dashed") + # the dashed line at the mean sampling interval
  scale_x_continuous(n.breaks = 15, limits = c(0, max(as.numeric(df_diagnostic$difftime, na.rm=T))))+ # number of x axis breaks to have
  theme_light() + # change theme
  theme(axis.title = element_text(size = 16), axis.text = element_text(size =12)) + # change axis text formatting
  xlab(paste0("Interval (", units_df_datetime, ")")) + ylab("Frequency") # add axis labels

Samphist


# convert the time threshold to the same units as difftime in df_diagnostic
threshold_units <- as.period(threshold_time, units_df_datetime)

## split the tracking data into segments  
df_segments <- df_diagnostic %>%
  group_by(ID) %>%
  mutate(segment_start = case_when(row_number()==1 ~ TRUE,
                                   difftime > threshold_units ~ TRUE), # split track if difftime exceeds user defined threshold
         segment_row_number = case_when(segment_start == TRUE ~ cur_group_rows())) %>%
  fill(segment_row_number) %>%
  ungroup() %>% group_split(ID) %>%
  purrr::map_df(~.x %>% group_by(segment_row_number) %>% mutate(segment_num = cur_group_id())) %>% # assign sequential segment number
  ungroup() %>% select(-c(segment_row_number)) %>% # remove intermediate column
  mutate(SegmentID = paste0(ID, "_", segment_num)) %>%
  group_by(SegmentID) %>%
  add_count() %>% # calcualte the number of fixes within each segment
  rename(segment_length = n)



#-------------------------------#
## 3. Remove short segments  ####
#-------------------------------#

## Here, we segments where gaps in data exceed a time threshold
## This code will work based on time differences calculated in the main workflow

#--------------------#
##USER INPUT START##
#--------------------#

# define minimum number of points for valid segment 
# If a segments contains few locations then this value then the segment will be discarded
threshold_points <- 25

#-----------------#
##USER INPUT END##
#-----------------#

## plot segments over time for each individual
## we should see the colour change every time the data gap exceeds the threshold
ggplot(df_segments, aes(x = DateTime, y = netdisp, col = as.factor(segment_num)))+
  geom_point(cex = 0.5)+
  facet_wrap(ID~., scales = "free", ncol = 1)+
  theme_light() +
  labs(col="Segment number")


## plot histogram for the number of locations in each proposed segment
## Dashed line at the value of threshold_points that the user has defined
SegLengthHist <- df_segments %>% 
  ggplot(aes(x = segment_length))+
  geom_histogram(bins = 50) + # control the number of bins in the histogram using `bins` argument
  scale_x_continuous(n.breaks = 20, limits = c(0, max(df_segments$segment_length)))+ # control major axis tick 
  geom_vline(xintercept = threshold_points, linetype = "dashed") + # add dashed line at user input value
  theme_light()+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size =12), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_text(size = 12), legend.text = element_text(size = 11))+
  xlab("Segment length (No. locations)") + ylab("Frequency")

SegLengthHist


## remove short segments that do no exceed the user define threshold_point
## If you want to keep all segments in, remove the last line of code to filter the data by segment length
df_segments_valid <- df_segments %>%
  filter(segment_length >= threshold_points) %>%
  ungroup()



#------------------------------------#
## 5. Summarise Retained Segments ####
#------------------------------------#

# Make two data frames:
# 1. a segment-level file, where each segment has a unique row
# 2. a population-level file, to summarise segments

# function to calculate standard error
se <- function(x) sqrt(sd(x, na.rm = T) / length(x[!is.na(x)]))

#--------------------#
##USER INPUT START##
#--------------------#

sampleRateUnits <- "mins" # units to display sample rate in table

# define levels of grouping factors

# Firstly, down to population level
# here, we are working on data from one population & year, and so use 'Species' and 'Population'
# add any other relevant grouping factors here, e.g., Country / Year / Season / Age
grouping_factors_poplevel <- c("Species", "Population") 

# Secondly, down to individual level
# add e.g., DeployID if relevant
grouping_factors_indlevel <- c("ID")

#-----------------#
##USER INPUT END##
#-----------------#

# summary of each segment = 1 row per segment
df_segment_summary <- df_segments_valid %>%
  group_by(across(all_of(c(grouping_factors_poplevel, grouping_factors_indlevel, "SegmentID")))) %>% 
  mutate(cum_dist = cumsum(dist)) %>%
  summarise(Segment_start = min(DateTime),
            Segment_end = max(DateTime),
            Segment_duration = difftime(max(DateTime), min(DateTime), units="mins"),
            Total_distance = sum(dist, na.rm = T)) 
df_segment_summary

# intermediate = individual level summary = 1 row per individual
# each individual will therefore have equal weighting in the population summary
df_segment_summary_ind <- df_segment_summary %>%
  group_by(across(all_of(c(grouping_factors_poplevel, grouping_factors_indlevel)))) %>% 
  summarise(Segment_duration_ind_mean = mean(as.numeric(Segment_duration, units = sampleRateUnits), na.rm = T),
            Segment_duration_ind_se = se(as.numeric(Segment_duration, units = sampleRateUnits)),
            Total_distance_ind_mean = mean(Total_distance),
            Total_distance_ind_se = se(Total_distance),
            N_Segments = length(unique(SegmentID))) 
df_segment_summary_ind

# highest = 1 row per population
df_segment_summary_pop <- df_segment_summary_ind %>%
  group_by(across(all_of(grouping_factors_poplevel))) %>%
  summarise(Segment_duration_mean = mean(Segment_duration_ind_mean),
            Segment_duration_se = se(Segment_duration_ind_mean),
            Total_distance_mean = mean(Total_distance_ind_mean),
            Total_distance_se = se(Total_distance_ind_mean),
            N_Segments_total = sum(N_Segments),
            N_Segments_perInd_mean = mean(N_Segments),
            N_Segments_perInd_se = se(N_Segments))

df_segment_summary_pop



#------------------------------#
## 6. Map Retained Segments ####
#------------------------------#

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

#------------------#
## USER INPUT END ##
#------------------#

## load additional libraries for spatial visualisation
## optional step - could be removed/improved later!!
message("If you see masking warning these are fine. 
        Watch out for packages that aren't installed yet")
library(rnaturalearth)

## create a map of all points
## set the plot limits as the max and min lat/longs as the tracking data

## first set up a basemap to plot over

## use rnatural earth low res countries basemap
## co-ordinates in lat/long
## matches other spatial data
## probably improve this later!!!
countries <- ne_countries(scale = "medium", returnclass = "sf")

## define min and max co-ordinates based on extent of tracking data
minlon <- min(df_segments_valid$Lon)
maxlon <- max(df_segments_valid$Lon)

minlat <- min(df_segments_valid$Lat)
maxlat <- max(df_segments_valid$Lat)


## create base map with correct extent, topography, country outlines, etc.,
map_base <- ggplot() + 
  # add colour scheme for the fill
  scale_fill_viridis_c(option="mako", name = topo_label) + 
  # add map of countries over the top
  geom_sf(data = countries, aes(geometry = geometry), fill = NA) + 
  # set plot limits
  coord_sf(xlim = c(minlon-0.1, maxlon+0.1), ylim = c(minlat-0.1, maxlat+0.1), crs = 4326, expand = F) +
  # add labels
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text=element_text(colour="black"),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(hjust=0.7),
        axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
        axis.text.y = element_text(hjust=0.7,angle=90,vjust=0.3))+
  theme_light()


## map individual locations
## colour points by segment number and plot all individuals together
map_segmentsAll <- map_base + 
  # add GPS points and paths between them
  geom_point(data = df_segments_valid, aes(x = Lon, y = Lat, col = as.factor(segment_num)), alpha = 0.8, size = 0.5 ) +
  geom_path(data = df_segments_valid, aes(x = Lon, y = Lat, col = as.factor(segment_num)), alpha = 0.8, linewidth = 0.5 ) +
  labs(col = "Segment number")
map_segmentsAll

## map individual locations
## colour points by segment number and facet the plot by individual now
map_segments <- map_segs + 
  ##facet for individual
  facet_wrap(~ ID, ncol = round(sqrt(n_distinct(df_segments_valid$ID))))
map_segments


## save map for further use
## use ggsave function
ggsave(plot = map_segments, filename = paste0(species_code, "_map_segments.tiff"),
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)



#------------------#
## 7. Save data ####
#------------------#

## NOTE: Users can return to the start and re-run the segmenting process 
##       if they wanted to make changes after visualising the segments

#--------------------#
##USER INPUT START##
#--------------------#

## Define species code
species_code <- "RFB"

## file path & name for df_segments
filepath_df_segments_out <- here("DataOutputs","WorkingDataFrames") 
filename_df_segments_out <- paste0(species_code, "_segments")


## file path & name for df_segment_summary and df_segment_summary_pop
filepath_summary_out <- here("DataOutputs","SummaryDataFrames")
filename_segment_summary_out <- paste0(species_code, "_segment_summary")
filename_segment_summary_pop_out <- paste0(species_code, "_segment_summary_pop")

#-----------------#
##USER INPUT END##
#-----------------#

## save dataframes as .csv files
write_csv(df_segments, file = here(filepath_df_segments_out, paste0(filename_df_segments_out, ".csv")))
write_csv(df_segment_summary, file = here(filepath_summary_out, paste0(filename_segment_summary_out, ".csv")))
write_csv(df_segment_summary_pop, file = here(filepath_summary_out, paste0(filename_segment_summary_pop_out, ".csv")))



