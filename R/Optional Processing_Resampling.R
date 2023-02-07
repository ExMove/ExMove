#' ----------------------------------------------------------------------- #
#' PROJECT: Optional Resampling
#' CONTENTS: 
#'  - This code can be used to sub-sample or interpolate movement data
#'  - Load libraries
#'  - Read in data
#'  - Option A - Sub-sample movement data
#'  - Save resampled data file
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
library(here) #for reproducible filepaths


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

filepath <- "DataOutputs/WorkingDataFrames/RFB_filtered.csv"

#-----------------#
##USER INPUT END##
#-----------------#

df_diagnostic <- read_csv(filepath)


#---------------------#
##2. Sub-sampling  ####
#---------------------#


## might want to sub-sample the tracking data to a regular time interval
## often required to compare movement metrics from tags with different sampling intervals
## NOTE: if sub-sampling to an interval greater then 60 minutes, e.g 2 hours,
##       then change the unit to "hours" and resolution to 2. Do not use 120 "minutes". 

#--------------------#
## USER INPUT START ##
#--------------------#

## set time unit for sub-sampling

subsampling_unit <- "minutes"

## set the resolution to subsample to

subsampling_res <- 20

#------------------#
## USER INPUT END ##
#------------------#

## pipe to subsample tracking data to a defined interval
## need to group by bird to subsample each individual separately

df_subsampled <- df_diagnostic %>%
  group_by(ID) %>%
  mutate(subsample = round_date(DateTime, unit = period(num = subsampling_res, units = subsampling_unit)), #round to nearest value of subsampling unit
         accuracy = as.period(interval(DateTime, subsample)/ seconds(1)),.after=DateTime, #accuracy of round (Date-subsample) in secs
         accuracy = abs(accuracy)) %>% #convert to absolute values (no negatives)
  group_by(ID, subsample) %>% #group by ID and subsample for removing duplicates
  slice_min(accuracy) %>% #slice out subsample duplicates with highest accuracy
  select(-c(accuracy,subsample)) #remove excess columns 


#-----------------------------------#
##3. Save reprocessed data frame ####
#-----------------------------------#

#--------------------#
## USER INPUT START ##
#--------------------#

## It is good practice to include species in the data frame
  
species_code <- "RFB"

filepath_dfout <- here("DataOutputs","WorkingDataFrames") #specify where df's will be saved

## define name for new saved file
## here, we use the species code and "_resampled"

filename_dfout <- paste0(species_code, "_resampled")


#------------------#
## USER INPUT END ##
#------------------#

## Add species column 
## Add any other columns here relevant to your own data

df_subsampled$Species <- species_code

## save dataframe

write_csv(df_subsampled, file = here(filepath_dfout, paste0(filename_dfout,".csv")))

## the subsampled data frame can now be used for further analysis
