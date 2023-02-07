#' ----------------------------------------------------------------------- #
#' PROJECT: Welcome to the Workflow for The Great Coding workshop!
#'  - This code can be used for reading and processing tracking data files
#' CONTENTS: 
#'  - Load libraries
#'  - 0. Pre-flight checks
#'  - 1. Read in data files
#'  - 2. Merge with metadata
#'  - 3. Cleaning = remove bad tag values
#'  - 4. Processing = spatial and temporal calculations
#'  - 5. Save df_diagnostic = for use in shiny app
#'  - 6. Filtering = remove erroneous fixes
#'  - 7. Summarise cleaned & filtered tracking data
#'  - 8. Save df_filtered and summary data
#'  - 9. Visualisation
#'  - 10. Post Processing : Optional steps
#'  - 11. Reformat data for upload to public databases
#'  DEPENDENCIES:
#'  - Requires tidyverse, data.table and sf to be installed
#'  - Source data: use example data sets (RFB, SAP) or provide your own
#' AUTHORS: Alice Trevail, Stephen Lang, Luke Ozsanlav-Harris, Liam Langley
#' #----------------------------------------------------------------------- #


#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(data.table)
library(tidyverse) #installed using install.packages("tidyverse")
library(lubridate) #installed as part of the tidyverse but needs to be loaded manually *NB. soon to be core!
library(sf)
library(here) #for reproducible filepaths


#--------------------------#
##0. Pre-flight checks  ####
#--------------------------#

## How to use this workflow:
## - We will inspect the data before reading it in, so there is no need to open it in another program (e.g., excel, which can corrupt dates and times)
## - User-defined parameters are entered between ## USER INPUT START ## and ## USER INPUT END ##
## - These parameters are then called within the subsequent processing steps
## - Where you see: ## ** Option ** ##, there is an alternative version of the code to fit some common alternative data formats
## - Throughout, we will use some key 'functions' to inspect the data (e.g., 'head' for top rows, 'str' for column types, and 'names' for column names)

## Data structure:
## - Data files should all be stored in a specific folder (ideally within Data)
## - Tracking data must contain timestamp and at least one other sensor column
## - Data for each deployment/individual should be in a separate file
## - ID should be in tracking data file name, and should be the same length for all individuals
## - We also provide code to read in data that are already combined into one/multiple file(s) with an ID column
## - Metadata file should be in parent directory of data files
## - Metadata should contain one row per deployment per individual

## The importance of ID:
## - Throughout this workflow, we use ID to refer to the unique code for an individual animal
## - In certain cases, you might have additional ID columns in the metadata (e.g., DeployID),
##   or read in data with a unique TagID instead of ID.
## - This code will work as long as all of the relevant info is included in the metadata
## - For more info and helpful code, see the FAQ document & troubleshooting script

## How to troubleshoot problems if something doesn't work with your data:
## - Refer to the FAQ document in the github page
## - This signposts to helpful resources online (e.g., spatial co-ordinate systems)
## - See the troubleshooting code scripts that we've written to accompany this workflow
## - (e.g., using multiple ID columns for re-deployments of tags/individuals)


#---------------------------#
##1. Read in data files  ####
#---------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

## Set filepath for folder containing raw data files
## NB: this code will try to open all files matching the file pattern within this folder
## Therefore, it is best if this folder only contains the raw data files
filepath <- here("Data", "SAP") 

## Define common file pattern to look for
## An asterisk (*) matches any character except a forward-slash
## e.g., "*.csv" will import all files within filepath folders that end with ".csv"
filepattern <- "*.csv" 

## Let's view the file names, to check that we have the files we want & find ID position
## This will include names of sub-folders
ls_filenames <- list.files(path = filepath, recursive = TRUE, pattern = filepattern)
ls_filenames

## If your data are all in separate files with ID in the file name: (this is optimal for new tracking data)
## Find ID number from file name (excluding names of sub-folders)
## Numbers refer to the position of the characters in the file name string
## e.g., for "GV37501_201606_DG_RFB.csv", the ID number GV37501 is characters 1 to 7
## This will only work if ID numbers are the same length and position in all file names to be imported
IDstart <- 1 #start position of the ID in the filename 
IDend <- 8 #end position of the ID in the filename

## Now, let's inspect the data by reading in the top of the first data file as raw text
## (To inspect the first row of all data files, you can removed the '[1]' and change "n_max" to 1)
test <- fs::dir_ls(path = filepath, recurse = TRUE, type = "file",  glob = filepattern)[1]
read_lines(test, n_max = 5)  # change n_max to change the number of rows to read in

## number of lines at top of file to skip (e.g., if importing a text file with additional info at top)
skiplines <- 0

## define date format(s) used (for passing to lubridate)
## "d"=day as decimal, "m"=month as decimal, "y"=year w/o century (2 digits), "Y"=year w/ century (4 digits)
## Here, we've included common combinations, modify if your data include a different format
date_formats <- c("dmY", "Ymd") #specify date formats (e.g. "dmY" works for 01-12-2022 and 01/12/2022)
datetime_formats <- c("dmY HMS", "Ymd HMS") #specify date & time format 

## define time zone for tracking data 
trackingdatatimezone <- "UTC"

## By default, the below code will find column names from the first row of data: colnames <- TRUE
## if you want to specify your own column names, do so here as a character vector
## set colnames <- FALSE to automatically number columns
colnames <- TRUE

## How are your data delimited?
## Here, we use the function read_delim() and specify the delimiter to make this code more universal
## More guidance here: https://readr.tidyverse.org/reference/read_delim.html
## Some examples:
## ','   = comma delimited       = equivalent to using read_csv (typically saved with file extension .csv)
## '\t'  = tab delimited         = equivalent to using read_tsv 
## ' '   = whitespace delimited  = equivalent to using read_table

## Let's inspect the data again, this time skipping rows if set, to check the file delimiter
read_lines(test, n_max = 5, skip = skiplines)

## Set delimiter to use within read_delim
user_delim <- ","
user_trim_ws <- TRUE # Should leading and trailing whitespace (ASCII spaces and tabs) be trimmed from each field before parsing it?

#------------------#
## USER INPUT END ##
#------------------#


## Read in and merge tracking data files directly from github repository
df_combined <- fs::dir_ls(path = filepath, glob = filepattern, #use our defined filepath and pattern
                          type = "file",  recurse = TRUE) %>% # recurse = T searches all sub-folders
  purrr::set_names(nm = basename(.)) %>% # removing path prefix (makes filename more manageable)
  purrr::map_dfr(read_delim, .id="filename", #read all the files in using filename as ID column
                 col_types = cols(.default = "c"), col_names = colnames, 
                 skip = skiplines, delim = user_delim, trim_ws = user_trim_ws) %>% 
  mutate(ID = str_sub(string = filename, start = IDstart, end = IDend), #substring ID from the filename (start to end of substring)
         .after = filename) #position the new ID column after filename column
df_combined


## ** Option ** ##
## If your data are combined into one or multiple csv files containing an ID column, use the following code:
## This is the same as above, but doesn't create a new ID column from the file name
# df_combined <- fs::dir_ls(path = filepath, recurse = TRUE, type = "file",  glob = filepattern) %>% # recurse = T searches all sub-folders
#   purrr::map_dfr(read_delim, col_types = cols(.default = "c"), col_names = colnames, skip = skiplines, delim = user_delim, trim_ws = user_trim_ws) 
# df_combined



#--------------------#
## USER INPUT START ##
#--------------------#

## Select necessary comments & coerce column names
## Compulsory columns = ID, Date & Time (or single DateTime column?)
## Optional columns depending on sensor type, e.g. Lat, Lon, error

df_slim <- data.frame(ID = as.character(df_combined$ID),
                      Date = df_combined$Date,
                      Time = df_combined$Time,
                      Lat = df_combined$Lat,
                      Lon = df_combined$Lon)


## ** Option ** ##
## Here's an example of how to change the above code for data with different columns and column names
## This code works with immersion data recorded by a GLS logger (no location data)
# df_slim <- data.frame(ID = df_combined$ID,
#                       Date = df_combined$`DD/MM/YYYY`,
#                       Time = df_combined$`HH:MM:SS`,
#                       Immersion = df_combined$`wets0-20`)

#------------------#
## USER INPUT END ##
#------------------#

str(df_slim);head(df_slim)

## Dates and Times
message("If you see any 'failed to parse' warnings below a date or time has not formatted (we will remove these NA's later)")

## Parse date and create datetime column
## If your date and time are in separate columns:
## Create datetime (append time to date object using parse_date_time)

df_slim$Date <- lubridate::parse_date_time(df_slim$Date, orders=date_formats) #use lubridate to parse Date using date_formats
df_slim$DateTime <- lubridate::parse_date_time(paste(df_slim$Date, df_slim$Time), #use lubridate to parse DateTime 
                                               orders=datetime_formats, #using the datetime_formats object we made earlier
                                               tz=trackingdatatimezone) #can add timezone here with "tz="

## Check which DateTimes failed to parse (if any)
Fails <- df_slim %>% filter(is.na(DateTime)==T)
head(Fails)

## ** Option ** ##
## If date and time are already combined in one column (named DateTime), parse to ensure correct format & time zone
# df_slim$DateTime <- lubridate::parse_date_time(df_slim$DateTime,orders=datetime_formats, tz=trackingdatatimezone)

## Sort finished raw dataframe by ID and DateTime
df_raw <- df_slim %>% 
  arrange(ID, DateTime) %>%
  drop_na(DateTime) #remove NA's in datetime column
head(df_raw)

## Remove intermediate files/objects
rm(list=ls()[!ls() %in% c("df_raw","date_formats","datetime_formats","trackingdatatimezone")]) #could just specify objects to keep? (no errors)

#----------------------------#
##2. Merge with metadata  ####
#----------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

## set file path to metadata
filepath_meta <- here("Data","SAP_Metadata.csv")

## define metadata date and time format(s) used (for passing to lubridate)
## "d"=day as decimal, "m"=month as decimal, "y"=year w/o century, "Y"=year w/ century
## Here, we've included common combinations, modify if your data include a different format

metadate_formats <- c("dmY", "Ymd") #specify date format used in metadata
metadatetime_formats <- c("dmY HMS", "Ymd HMS") #specify date & time format
metadatatimezone <- "UTC" #specify timezone used for metadata

#------------------#
## USER INPUT END ##
#------------------#

## Read in metadata file

df_metadata <- readr::read_csv(filepath_meta)
names(df_metadata)

#--------------------#
## USER INPUT START ##
#--------------------#

## Select necessary comments & coerce column names
## Compulsory columns: ID, deployment date & deployment time
## Optional columns depending on sensor type: e.g. populaiton, sex, age, central place (CP) Lat, CP Lon
## Add or delete columns here as appropriate

## If you have multiple ID columns, include them here (e.g., TagID/DeployID)
## For example, if one individual was tracked over multiple deployments/years
## Similarly, if one tag was re-deployed on multiple individuals
## For more information and helpful code, see the FAQ document and troubleshooting script 

df_metadataslim <- data.frame(ID = as.character(df_metadata$Deploy_ID),
                              DeployID = df_metadata$Deploy_ID,
                              DeployDate_local = df_metadata$Deployment_date, # note in col name that these are recorded in local time
                              DeployTime_local = df_metadata$Deployment_time, # note in col name that these are recorded in local time
                              RetrieveDate_local = df_metadata$Retrieval_date, # note in col name that these are recorded in local time
                              RetrieveTime_local = df_metadata$Retrieval_time, # note in col name that these are recorded in local time
                              CPLat = df_metadata$Nest_Lat,
                              CPLon = df_metadata$Nest_Lon,
                              Species = df_metadata$Species,
                              Population = df_metadata$Colony)


## ** Option ** ##
## If central place for each individual is not known,
## add population level central places here using the following code:

# # create a dataframe of population CPs.
# df_PopCPs <- tribble(
#   ~Population,  ~CPLat,    ~CPLon,
#   "DG",         -7.24,     72.43,
#   "NI",         -5.68,     71.24
# )
# 
# # merge df_metadataslim and df_PopCPs
# # The column "Population" must be present in both dataframes
# # This will overwrite "CPLat" and "CPLon" if present in df_metadataslim
# df_metadataslim  %<>%
#   select(matches(setdiff(names(df_metadataslim), c("CPLat", "CPLon")))) %>%
#   left_join(., df_PopCPs, by = "Population")


#------------------#
## USER INPUT END ##
#------------------#

## Format all dates and times, combine them and specifies timezone (tz)

df_metadataslim <- df_metadataslim %>%
  mutate(across(contains('Date'), 
                ~parse_date_time(., orders=metadate_formats))) %>% #format all Date column with lubridate
  mutate(Deploydatetime = parse_date_time(paste(DeployDate_local, DeployTime_local),#create deploy datetime
                                          order=metadatetime_formats, 
                                          tz=metadatatimezone),
         Retrievedatetime = parse_date_time(paste(RetrieveDate_local, RetrieveTime_local), #create retrieve datetime
                                            order=metadatetime_formats, 
                                            tz=metadatatimezone)) %>% 
  select(-c(DeployDate_local,DeployTime_local,RetrieveDate_local,RetrieveTime_local)) %>%
  mutate(across(contains('datetime'), #return datetime as it would appear in a different tz
                ~with_tz(., tzone=trackingdatatimezone))) 


## Merge metadata with raw data using ID column

df_metamerged <- df_raw %>%
  left_join(., df_metadataslim, by="ID") 


### Remove intermediate files/objects
rm(list=ls()[!ls() %in% c("df_metamerged")]) #specify objects to keep


#-----------------#
##3. Cleaning  ####
#-----------------#

## This data cleaning stage is to remove erroneous data values
## E.g., from poor location fixes

#--------------------#
## USER INPUT START ##
#--------------------#

## Define your own no/empty/erroneous data values in Lat and Lon columns
## e.g. bad values specified by the tag manufacturer

No_data_vals <- c(0, -999)

## Define a vector of columns which can't have NAs

na_cols <- c("Lat", "Lon", "DateTime", "ID")


#------------------#
## USER INPUT END ##
#------------------#

## Pipe to clean data:
## Remove NAs
## Remove user-defined no data values in Lat Lon columns
## Remove duplicates
## Remove un deployed locations
## Remove locations within temporal cut off following deployment

df_clean <- df_metamerged %>%
  drop_na(all_of(na_cols)) %>% 
  filter(!Lat %in% No_data_vals & !Lon %in% No_data_vals) %>% # remove bad data values in Lat Lon columns
  distinct(DateTime, ID, .keep_all = TRUE) %>% # this might be a problem for ACC data where we don't have milliseconds
  filter(Deploydatetime < DateTime & DateTime < Retrievedatetime) # keep deployment period, only
head(df_clean)

## Remove intermediate files/objects
rm(list=ls()[!ls() %in% c("df_clean")]) #specify objects to keep


#-------------------#
##4. Processing  ####
#-------------------#

## Some useful temporal and spatial calculations on data

#--------------------#
## USER INPUT START ##
#--------------------#

## Specify co-ordinate projection systems for the tracking data and meta data
## Default here is lon/lat for both tracking data & metadata, for which the EPSG code is 4326. 
## Look online for alternative EPSG codes, e.g., https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/

tracking_crs <- 4326 # Only change if data are in a different coordinate system
meta_crs <- 4326 # Only change if data are in a different coordinate system

#------------------#
## USER INPUT END ##
#------------------#

## transform coordinates of data
## perform spatial calculations

## this requires spatial analysis
## it is good practice to run all spatial analyses in a coordinate system with units in metres

## As an example, we will use Spherical Mercator projection — aka 'WGS' (crs = 3857)
## Consider the location and scale of your data (e.g., equatorial/polar/local scale/global scale) when choosing a projection system
## Other options include (but are not limited to) UTM, Lambert azimuthal equal-area (LAEA)


## ** Option ** ##
## If your data include a Central Place:
## calculate bearings relative to CP
df_diagnostic <-  df_clean %>%
  ungroup() %>% #need to ungroup to extract geometry of the whole dataset
  mutate(geometry_GPS = st_transform( #assign geometry and transform to WGS for dist calcs
    st_as_sf(., coords=c("Lon","Lat"), crs=tracking_crs), crs = 3857)$geometry,
    geometry_first = st_transform(
      st_as_sf(slice(., 1), coords=c("Lon","Lat"), crs=tracking_crs), crs = 3857)$geometry,
    geometry_CP = st_transform( #assign geometry and transform to WGS for dist calcs
      st_as_sf(., coords=c("CPLon","CPLat"), crs=meta_crs), crs = 3857)$geometry) %>% 
  group_by(ID) %>% #back to grouping by ID for calculations per individual
  mutate(dist = st_distance(geometry_GPS, lag(geometry_GPS), by_element = T), #distance travelled from previous fix
         difftime = difftime(DateTime, lag(DateTime), units="secs"),          #time passed since previous fix
         netdisp = st_distance(geometry_GPS, geometry_first, by_element = T),    #calculate distance between first location and current location
         speed = as.numeric(dist)/as.numeric(difftime),                       #calculate speed (distance/time)
         dLon = as.numeric(Lon)-lag(as.numeric(Lon)), #difference in longitude, relative to previous location
         dLat = as.numeric(Lat)-lag(as.numeric(Lat)), #difference in longitude, relative to previous location
         turnangle = atan2(dLon, dLat)*180/pi + (dLon < 0)*360, #angle (in degrees) from previous to current location using formula theta = atan(y/x), where y = change along y axis & x = change along x axis
         CPdist = st_distance(geometry_GPS, geometry_CP, by_element = T), #calculate distance between central place and current location
         dLon_CP = as.numeric(Lon)-CPLon, #difference in longitude between current location and central place
         dLat_CP = as.numeric(Lat)-CPLat, #difference in longitude between current location and central place
         CPbearing = atan2(dLon_CP, dLat_CP)*180/pi + (dLon_CP < 0)*360) %>% #bearing (in degrees) from central place to current location using formula theta = atan(y/x), where y = change along y axis from CP & x = change along x axis from CP
  ungroup() %>% select(-c(geometry_GPS, geometry_CP, dLon, dLat, dLon_CP, dLat_CP)) #ungroup and remove geometries


## ** Option ** ##
## If your data do not include a Central Place:
## use the below code to calculate bearings relative to first location
# df_diagnostic <-  df_clean %>%
#   ungroup() %>% #need to ungroup to extract geometry of the whole dataset
#   mutate(geometry_GPS = st_transform( #assign geometry and transform to WGS for dist calcs
#       st_as_sf(., coords=c("Lon","Lat"), crs=tracking_crs), crs = 3857)$geometry,
#     geometry_first = st_transform(
#       st_as_sf(slice(., 1), coords=c("Lon","Lat"), crs=tracking_crs), crs = 3857)$geometry) %>% 
#   group_by(ID) %>% #back to grouping by ID for calculations per individual
#   mutate(dist = st_distance(geometry_GPS, lag(geometry_GPS), by_element = T), #distance travelled from previous fix
#          difftime = difftime(DateTime, lag(DateTime), units="secs"),          #time passed since previous fix
#          netdisp = st_distance(geometry_GPS, geometry_first, by_element = T),    #calculate distance between first location and current location
#          speed = as.numeric(dist)/as.numeric(difftime),                       #calculate speed (distance/time)
#          dLon = as.numeric(Lon)-lag(as.numeric(Lon)), #difference in longitude, relative to previous location
#          dLat = as.numeric(Lat)-lag(as.numeric(Lat)), #difference in longitude, relative to previous location
#          turnangle = atan2(dLon, dLat)*180/pi + (dLon < 0)*360) %>% #angle (in degrees) from previous to current location using formula theta = atan(y/x), where y = change along y axis & x = change along x axis
#   ungroup() %>% select(-c(geometry_GPS, dLon, dLat)) #ungroup and remove geometries


#---------------------------#
##5. Save df_diagnostic #### 
#---------------------------#

## Save df_diagnostic to use in shiny app
## The app is designed to explore how further filtering and processing steps affect the data

#--------------------#
## USER INPUT START ##
#--------------------#

## It is good practice to include species in the data frame & file name
species_code <- "SAP"

## Define file path for saved file
filepath_dfout <- here("DataOutputs","WorkingDataFrames")

## Define file name for saved file
## here, we use the species code and "_diagnostic"
## Change if you want to use a different naming system
filename_dfout <- paste0(species_code, "_diagnostic")

## ** Option ** ##
## If not added from the metadata, add a species column and any other columns here relevant to your data
# df_diagnostic$Species <- species_code


#------------------#
## USER INPUT END ##
#------------------#

## save dataframe as .csv
write_csv(df_diagnostic, file = here(filepath_dfout, paste0(filename_dfout,".csv")))



## Remove intermediate files/objects
rm(list=ls()[!ls() %in% c("df_diagnostic")]) #specify objects to keep


#-----------------#
##6. Filtering #### 
#-----------------#

## This filtering stage is designed to remove outliers in the data
## You can use outputs from the shiny app to inform these choices

## If you don't need to filter for outliers, skip this step and keep using df_diagnostic


#--------------------#
## USER INPUT START ##
#--------------------#

## Define a period to filter after tag deployment
## All points before the cutoff will be removed
## For example, to remove potentially unnatural behaviour following the tagging event
## This has to be an integer value
## change the units within the function (e.g., "min"/"mins"/"hours"/"year"...)

filter_cutoff <- as.period(5, unit="minutes") 

## Define speed filter in m/s
## Any points with faster speeds will be removed
filter_speed <- 10

## Define net displacement filter and specify units
## Any points further away from the first tracking point will be removed
## If you want to retain points no matter how far, then choose a value larger than max(df_diagnostic$netdisp)
max(df_diagnostic$netdisp)
filter_netdisp_dist <- 300
filter_netdist_units <- "km" # e.g., "m", "km"


#------------------#
## USER INPUT END ##
#------------------#

# create net displacement filter using distance and units
filter_netdisp <- units::as_units(filter_netdisp_dist, filter_netdist_units)

# filter df_diagnostic
df_filtered <- df_diagnostic %>%
  filter(Deploydatetime + filter_cutoff < DateTime, # keep times after cutoff
         speed < filter_speed, # keep speeds slower than speed filter
         netdisp < filter_netdisp) # keep distances less than net displacement filter
head(df_filtered)


## Remove intermediate files/objects
rm(list=ls()[!ls() %in% c("df_filtered")]) #specify objects to keep


#--------------------------------------------------#
##7. Summarise cleaned & filtered tracking data ####
#--------------------------------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

## set the units to display sampling rate in the summary table

sampleRateUnits <- "mins" 


## Define levels of grouping factors to summarise over

## Firstly, down to population level
## here, we are working on data from one population & year, and so use 'Species' and 'Population'
## add any other relevant grouping factors here, e.g., Country / Year / Season / Age
grouping_factors_poplevel <- c("Species", "Population") 

## Secondly, down to individual level
## add e.g., DeployID if relevant
grouping_factors_indlevel <- c("ID")

#------------------#
## USER INPUT END ##
#------------------#

## function to calculate standard error
se <- function(x) sqrt(sd(x, na.rm = T) / length(x[!is.na(x)]))

## create a summary table of individual-level summary statistics
df_summary_ind <- df_filtered %>%
  group_by(across(c(all_of(grouping_factors_poplevel), all_of(grouping_factors_indlevel)))) %>%
  summarise(NoPoints = NROW(ID), # number of fixes
            NoUniqueDates = length(unique(Date)), # number of tracking dates
            FirstDate = as.Date(min(Date)), # first tracking date
            LastDate = as.Date(max(Date)), # last tracking date
            SampleRate = mean(as.numeric(difftime, units = sampleRateUnits), na.rm = T), # sample rate mean
            SampleRate_se = se(as.numeric(difftime, units = sampleRateUnits))) # sample rate standard error
df_summary_ind

## create a table of population-level summary statistics
df_summary_pop <- df_summary_ind %>% # use the individual-level summary data
  group_by(across(grouping_factors_poplevel)) %>%
  summarise(NoInds = length(unique(ID)), # number of unique individuals
            NoPoints_total = sum(NoPoints), # total number of tracking locations
            FirstDate = as.Date(min(FirstDate)), # first tracking date
            LastDate = as.Date(max(LastDate)), # last tracking date
            PointsPerBird = mean(NoPoints), # number of locations per individual: mean
            PointsPerBird_se = se(NoPoints), # number of locations per individual: standard error
            DatesPerBird = mean(NoUniqueDates), # number of tracking days per bird: mean
            DatesPerBird_se = se(NoUniqueDates), # number of tracking days per bird: standard error
            SampleRate_mean = mean(SampleRate), # sample rate mean
            SampleRate_se = se(SampleRate)) # sample rate standard error
df_summary_pop



## Remove intermediate files/objects
rm(list=ls()[!ls() %in% c("df_filtered", "df_summary_ind", "df_summary_pop")]) #specify objects to keep


#-----------------------------------------#
##8. Save df_filtered and summary data #### 
#-----------------------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

## Define species code
species_code <- "SAP"

## Define file path for df_filtered
filepath_filtered_out <- here("DataOutputs","WorkingDataFrames")

## Define file path for summary file
filepath_summary_out <- here("DataOutputs","SummaryDataFrames")

## Define file names for saved files
## here, we use the species code and "_summary_" followed by ind (individual level) or pop (population level)
## Change if you want to use a different naming system
filename_filtered_out <- paste0(species_code, "_filtered")
filename_summary_ind_out <- paste0(species_code, "_summary_ind")
filename_summary_pop_out <- paste0(species_code, "_summary_pop")


#------------------#
## USER INPUT END ##
#------------------#

## save dataframes as .csv files
write_csv(df_filtered, file = here(filepath_filtered_out, paste0(filename_filtered_out,".csv")))
write_csv(df_summary_ind, file = here(filepath_summary_out, paste0(filename_summary_ind_out,".csv")))
write_csv(df_summary_pop, file = here(filepath_summary_out, paste0(filename_summary_pop_out,".csv")))

## Remove intermediate files/objects
rm(list=ls()[!ls() %in% c("df_filtered", "df_summary_ind", "df_summary_pop")]) #specify objects to keep


#----------------------#
##9. Visualisation  ####
#----------------------#

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
species_code <- "SAP"

## We plot maps over a topography base-layer
## topography data include terrestrial (elevation) and marine (bathymetry/water depth) data
## set legend label for topography data, relevant to your data:
topo_label = "Elevation (m)"

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

## version of data for plotting
## transform required columns to numeric
## create time elapsed columns

df_plotting <- df_filtered %>%
  group_by(ID) %>%
  mutate(diffsecs = as.numeric(difftime),
         secs_elapsed = cumsum(replace_na(diffsecs, 0)),
         time_elapsed = as.duration(secs_elapsed),
         days_elapsed = as.numeric(time_elapsed, "days")) %>%
  mutate(across(c(dist,speed,CPdist, Lat, Lon), as.numeric))

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
base_topography_map <- getNOAA.bathy(lon1 = minlon - 0.5, lon2 = maxlon + 0.5,
                                     lat1 = minlat - 0.5, lat2 = maxlat + 0.5, resolution = 1)

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
  geom_point(data = df_plotting, aes(x = CPLon, y = CPLat),
             colour = "#FF3300", fill ="#FF3300", shape = 23, size = 2) +
  # set plot limits
  coord_sf(xlim = c(minlon-0.1, maxlon+0.1), 
           ylim = c(minlat-0.1, maxlat+0.1), crs = 4326, expand = FALSE) +
  # add labels
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text=element_text(colour="black"),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(hjust=0.7),
        axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
        axis.text.y = element_text(hjust=0.7, angle=90, vjust=0.3)) +
  theme_light()

## map all tracking locations
map_alllocs <- map_base + 
  # add GPS points
  geom_point(data = df_plotting, aes(x = Lon, y = Lat), alpha = 0.8, size = 0.5) 
map_alllocs

## map individual locations
## colour points by speed
## facet by ID
map_individuals <- map_base + 
  # add GPS points and paths between them
  geom_point(data = df_plotting, aes(x = Lon, y = Lat, col = speed), 
             alpha = 0.8, size = 0.5 ) +
  geom_path(data = df_plotting, aes(x = Lon, y = Lat, col = speed), 
            alpha = 0.8, size = 0.5 ) +
  # colour birds using scale_colour_gradient2
  scale_colour_gradient2(name = "Speed", low = "blue", mid = "white", high = "red", 
                         midpoint = (max(df_plotting$speed,na.rm=TRUE)/2)) +
  ##facet for individual
  facet_wrap(~ ID, ncol = round(sqrt(n_distinct(df_plotting$ID))))
map_individuals


## ** Option ** ##
## Throughout these visualisations, we'll split the population into individual facets.
## This works fine on the example code, where we only have a few individuals
## If you have more individuals and the facets are too small, you can split the plot onto multiple pages
## Use the below code to use facet_wrap_paginate from the ggforce package:
# install.packages("ggforce")
# library(ggforce)
# ## save plot as object to later extract number of pages
# ## e.g., with 2 per page:
# map_individuals <- map_base + 
#   # add GPS points and paths between them
#   geom_point(data = df_plotting, aes(x = Lon, y = Lat, col = speed), alpha = 0.8, size = 0.5 ) +
#   geom_path(data = df_plotting, aes(x = Lon, y = Lat, col = speed), alpha = 0.8, size = 0.5 ) +
#   # colour birds using scale_colour_gradient2
#   scale_colour_gradient2(name = "Speed", low = "blue", mid = "white", high = "red", midpoint = (max(df_plotting$speed,na.rm=TRUE)/2)) +
#   ##facet for individual
#   facet_wrap_paginate(~ID, ncol = 2, nrow= 1, page = 1)
# ## How many pages of plots?
# n_pages(map_individuals)
# ## display plot
# map_individuals
# ## run through different values of page to show each page in turn

# save maps for further use
# use ggsave function

ggsave(plot = map_alllocs, 
       filename = paste0(species_code, "_map_all_locs.tiff"),
       device = device,
       path = out_path, 
       units = units, width = 200, height = 175, dpi = dpi,   
)

ggsave(plot = map_individuals, 
       filename = paste0(species_code, "_map_individuals.tiff"),
       device = device,
       path = out_path, 
       units = units, width = 200, height = 175, dpi = dpi,   
)


## ** Option ** ##
## generate an interactive version of the maps to explore in plot window
## this can be slow, so we don't run by default
# ggplotly(map_alllocs)

# create a time series plot of speed
# faceted for each individual

speed_time_plot <- df_plotting %>% #speed over time
  ggplot(data=., aes(x=days_elapsed, y=speed, group=ID)) +
  geom_line() + #coord_cartesian(ylim=c(0,25)) +
  xlab("time elapsed (days)") + ylab("speed (m/s)") +
  facet_wrap(~ID, nrow= n_distinct(df_plotting$ID)) +
  theme(axis.text=element_text(colour="black")) +
  theme_light()


message("Warnings about 'non-finite' values for speed/step length plots are expected
         and should refer to the first location for each individual (i.e. number of 
         non-finite values should be equal to number of individuals)")

# save plot for further use

ggsave(plot = speed_time_plot, 
       filename = paste0(species_code, "_speed_timeseries_plot.tiff"),
       device = device,
       path = out_path, 
       units = units, width = 200, height = 175, dpi = dpi  
)

# create a histogram of point to point speeds
# can adjust binwidth and x limits manually
# Q: do your data need a speed filter? Check shiny app, we will add this later
speed_hist <- df_plotting %>% #speed histogram
  ggplot(data=., aes(speed)) +
  geom_histogram(binwidth=0.1, alpha=0.7) +
  geom_density(aes(y =0.1*..count..)) +
  #xlim(-0.1,5) +
  xlab("speed (m/s)") + ylab("count") +
  facet_wrap(~ID, nrow= n_distinct(df_plotting$ID)) +
  theme(axis.text=element_text(colour="black"))+
  theme_light()

# save plot for further use

ggsave(plot = speed_hist, 
       filename = paste0(species_code, "_speed_histogram.tiff"),
       device = device,
       path = out_path,
       units = units, width = 200, height = 175, dpi = dpi,   
)

# create a time series plot of step lengths
# faceted for each individual

step_time_plot <- df_plotting %>% #step length over time
  ggplot(data=., aes(x=days_elapsed, y=as.numeric(netdisp), group=ID)) +
  geom_line() +
  xlab("time elapsed (days)") + ylab("Distance from first fix (m)") +
  facet_wrap(~ID, nrow= n_distinct(df_plotting$ID)) +
  theme(axis.text=element_text(colour="black"))+
  theme_light()

# save plot for further use

ggsave(plot = step_time_plot, 
       filename = paste0(species_code, "_step_time_plot.tiff"),
       device = device,
       path = out_path,
       units = units, width = 200, height = 175, dpi = dpi,   
)

# create a histogram of step lengths
# can adjust binwidth and x limits manually

step_hist <- df_plotting %>% #step histogram
  ggplot(data=., aes(as.numeric(dist))) +
  geom_histogram(binwidth=1, alpha=0.7) +
  geom_density(aes(y =1*..count..)) +
  ##xlim(0,500) +
  xlab("step length (m)") + ylab("count") +
  facet_wrap(~ID, nrow= n_distinct(df_plotting$ID))+
  theme_light()

# save plot for further use

ggsave(plot = step_hist, 
       filename = paste0(species_code, "_step_hist.tiff"),
       device = device,
       path = out_path,
       units = units, width = 200, height = 175, dpi = dpi,   
)


#----------------------------------------#
##10. Post processing: Optional steps ####
#----------------------------------------#

## At this point we may be ready to start analysing our processed movement data
## However in some (many) instances additional processing may be necessary
## Some common additional processing steps that may be required are as follows:
## 1. Reprocessing - Sub-sampling or interpolating movement data to a standardised time interval
## 2. Dividing irregularly sampled movement tracks into defined segments
## 3. Defining foraging trips and calculating trip metrics (for central place foragers)
## We provide example code for performing these optional processing steps in separate R scripts
## We suggest reprocessing is performed before defining segments/trips


#--------------------#
## USER INPUT START ##
#--------------------#

## Here we read the output files of those optional processing scripts back into the workflow
## If no optional processing has been performed, we read back in the saved version of df_filtered
## If optional processing has been performed - change the filepath to your most recent data file

filepath_final <- here("DataOutputs","WorkingDataFrames","RFB_filtered.csv")

## we then read in the relevant data frame using the filepaths defined above

df_final <- read_csv(filepath_final)

#-----------------#
##USER INPUT END##
#-----------------#


#------------------------------------------------------#
##11. Reformat data for upload to public databases  ####
#------------------------------------------------------#

## Many researchers will choose to store their processed biologging data in an online repository
## As the use of bio-logging has proliferated, a number of public databases have been developed
## Such databases usually require our bio-logging data to be saved in a specific format

## Here we first reformat our data for upload to the Movebank database
## Movebank (https://www.movebank.org) is perhaps the largest online bio-logging database
## and is hosted by the Max Planck Institute

## Movebank does not have designated column names but specifies that no columns contain NA values
## Derived columns in this data set have NAs for the first value for each bird
## Such derived columns are superfluous from a data archiving perspective
## they can be recalculated from the data
## and moreover they increase file size 
## Filter data set to remove derived columns - dist, difftime, netdisp, speed, CPdist


df_movebank <- df_final %>%
  select(- c(dist, difftime, netdisp, speed, CPdist))


## Secondly, reformat our data for upload to the Seabird Tracking Database (STDB)
## The STDB (http://seabirdtracking.org) is the largest collection of seabird tracking data
## and is hosted by BirdLife International

## The STDB requests data to be down-sampled to 2 min resolution or lower, to reduce file storage demands
## If relevant, use the optional processing code to do this for your data

## The STDB requires specific columns
## At the time of writing, there isn't a process for uploading multiple grouping factors in one data file (e.g., species/population)
## Each level of the grouping factor therefore needs to be saved in it's own file
## Metadata (species, population location, etc., are added in the web portal)
## Different deployment periods can presumably be uploaded together, as they are distinguishable by date

## The STDB provide this template for data:
# Field name    Type            Data format       Options
# ------------------------------------------------------------- #
# BirdId	      user defined	  String														
# Sex	          limited choice	String	          female	male 	unknown											
# Age	          limited choice	String	          adult	immature	juvenile	fledgling	unknown									
# Breed Stage	  limited choice	String	          pre-egg	incubation	brood-guard	post-guard	chick-rearing	creche	breeding	fail (breeding season)	migration	winter	sabbatical	pre-moult	non-breeding	unknown
# TrackId	      user defined	  String														
# DateGMT     	user defined	  dd/mm/yyyy														
# TimeGMT	      user defined	  hh:mm:ss														
# Latitude	    user defined	  Decimal Degrees														
# Longitude	    user defined	  Decimal Degrees														
# Equinox	      limited choice	Yes/No	          yes	no												
# ArgosQuality	user defined	  String	          3	2	1	0	A	B	Z	


## Below, we provide an example for GPS tracking data of a central place forager
## We have read in df_trips from 'Central place trips.R' as df_final
## NB: this will only work on data where trips have been defined

## remind ourselves of the data structure
df_final
names(df_final)

#--------------------#
## USER INPUT START ##
#--------------------#

## Define parameters:

## time zone of data
tz_data <- "GMT"

## Levels of grouping factors (each will be saved as separate file)
grouping_factors <- c("Species", "Population") 

## File path for saving data
filepath_dfout <- here("DataOutputs","WorkingDataFrames", "RFB_SeabirdTrackingDataBaseFormat") 

#-----------------#
##USER INPUT END##
#-----------------#


## Re-format data for STDB, but retain grouping factor columns
df_STDB <- df_final %>%
  group_by(across(grouping_factors)) %>%
  # format date and time columns
  mutate(DateTimeGMT = with_tz(ymd_hms(DateTime, tz = tz_data), "GMT"), # make column of DateTime in GMT
         DateGMT = as_date(DateTimeGMT), # use lubridate to extract date, only
         TimeGMT = hms::as_hms(DateTimeGMT)) %>% #use hms from within tidyverse to extract time, only
  # rename columns as necessary (e.g. remove "TrackID = TripID," if trips have not been defined)
  rename(BirdID = ID,
         BreedStage = BreedingStage,
         TrackID = TripID,
         Latitude = Lat,
         Longitude = Lon) %>%
  # in this data, sex is unknown and wasn't in metadata. Add it here:
  mutate(Sex= "unknown") %>%
  # select columns specified in template (this will automatically add grouping factors, too)
  select(BirdID, Sex, Age, BreedStage, TrackID, DateGMT, TimeGMT, Latitude, Longitude)

## save each group as unique file
## requires user input to specify each level of grouping factor to keep in column name
## .y$Species returns the species of the current group
df_STDB %>%
  group_by(across(grouping_factors)) %>%
  group_walk(~ write_csv(.x, 
                         here(filepath_dfout, 
                              paste0("df_STDB_", .y$Species, "_", .y$Population, ".csv"))))
