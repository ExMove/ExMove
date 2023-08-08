#' ----------------------------------------------------------------------- #
#' PROJECT: Optional Resampling
#' CONTENTS: 
#'  - This code can be used to sub-sample or interpolate movement data
#'  - Load libraries
#'  - Read in data
#'  - Summarise/visualise sampling interval
#'  - Sub-sample movement data
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

## specify file path to data set *_filtered.csv
filepath <- "DataOutputs/WorkingDataFrames/RFB_filtered.csv"

#-----------------#
##USER INPUT END##
#-----------------#

## read in file
df_diagnostic <- read_csv(filepath)



#------------------------------------------------------------#
##2. Summary/Visualization of Current Sampling Intervals  ####
#------------------------------------------------------------#

#--------------------#
##USER INPUT START##
#--------------------#

## select the time unit to summaries and visualise the sampling interval
## Other options include "secs", "hours", "days", "weeks"
Time_unit <- "mins"

#-----------------#
##USER INPUT END##
#-----------------#

## Re-calcualte the time differences between consecutive locations
## These may have changes since the first calculation due to filtering
df_diagnostic <-  df_diagnostic %>%
  ungroup() %>% # make sure data set has no existing grouping
  group_by(ID) %>% # grouping by ID for calculations per individual
  mutate(difftime = difftime(DateTime, lag(DateTime), units= Time_unit)) %>%  # time passed since previous fix
  ungroup() 


## calcualte mean sampling interval for all data
MeanInt <- round(mean((df_diagnostic$difftime), na.rm=T), digits = 1)
MeanInt

## calculate sampling interval standard deviation for all data
SDInt <- round(sd((df_diagnostic$difftime), na.rm=T), digits = 1)
SDInt

## calcualte the max and min sampling interval for all data
MinInt <- round(min((df_diagnostic$difftime), na.rm=T) ,digits = 1)
MinInt
MaxInt <- round(max((df_diagnostic$difftime), na.rm=T) ,digits = 1)
MaxInt


## Summaries sampling intervals per individual
dfSamp_summary <- df_diagnostic %>% 
  ungroup() %>% 
  group_by(ID) %>%  # grouping by ID for calculations per individual
  summarise(NoFixesRAW = n(), # calcualte the number of locations for each level of the grouping variable
            Sampling_IntervalMEAN = round(mean((difftime), na.rm=T), digits = 1),   # mean sampling interval
            Sampling_IntervalSD = round(sd((difftime), na.rm=T), digits = 1),       # standard deviation of the sampling interval
            Sampling_IntervalMIN = round(min((difftime), na.rm=T), digits = 1),     # minimum sampling interval
            Sampling_IntervalMAX = round(max((difftime), na.rm=T), digits = 1)) %>% # maximum sampling interval
  ungroup()

dfSamp_summary # view individual-level summary of sampling intervals


## plot histogram of the sampling sampling intervals
## with a dashed line at the mena smapling interval
Samphist <- ggplot(df_diagnostic, aes(x = as.numeric((difftime), na.rm=T))) +
  geom_histogram(bins = 50) + # how many bins the histogram should have
  geom_vline(xintercept = MeanInt, linetype = "dashed") + # the dashed line at the mean sampling interval
  scale_x_continuous(n.breaks = 15, limits = c(0, max(as.numeric(df_diagnostic$difftime, na.rm=T))))+ # number of x axis breaks to have
  theme_light() + # change theme
  theme(axis.title = element_text(size = 16), axis.text = element_text(size =12)) + # change axis text formatting
  xlab(paste0("Interval (", Time_unit, ")")) + ylab("Frequency") # add axis labels

Samphist



#---------------------#
##3. Sub-sampling  ####
#---------------------#

## might want to sub-sample the tracking data to a regular time interval
## often required to compare movement metrics from tags with different sampling intervals
## NOTE: if sub-sampling to an interval greater then 60 minutes, e.g 2 hours,
##       then change the unit to "hours" and resolution to 2. Do not use 120 "minutes". 

#--------------------#
## USER INPUT START ##
#--------------------#

## set time unit for sub-sampling

subsampling_unit <- "mins"

## set the resolution to subsample to

subsampling_res <- 10

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



#--------------------------------------------------------------#
##4. Summary/Visualization of Resampled Sampling Intervals  ####
#--------------------------------------------------------------#

## Re-calcualte the time differences between consecutive locations after sub sampling
df_subsampled <-  df_subsampled %>%
  ungroup() %>% # make sure data set has no existing grouping
  group_by(ID) %>% # grouping by ID for calculations per individual
  mutate(difftime = difftime(DateTime, lag(DateTime), units= Time_unit)) %>%  # time passed since previous fix
  ungroup() 


## calcualte mean sampling interval for all data
MeanSubInt <- round(mean((df_subsampled$difftime), na.rm=T), digits = 1)
MeanSubInt

## calculate sampling interval standard deviation for all data
SDSubInt <- round(sd((df_subsampled$difftime), na.rm=T), digits = 1)
SDSubInt

## calcualte the max and min sampling interval for all data
MinSubInt <- round(min((df_subsampled$difftime), na.rm=T) ,digits = 1)
MinSubInt
MaxSubInt <- round(max((df_subsampled$difftime), na.rm=T) ,digits = 1)
MaxSubInt


## Summaries sampling intervals per individual
dfSubSamp_summary <- df_subsampled %>% 
  ungroup() %>% 
  group_by(ID) %>%  # grouping by ID for calculations per individual
  summarise(NoFixesRAW = n(), # calcualte the number of locations for each level of the grouping variable
            Sampling_IntervalMEAN = round(mean((difftime), na.rm=T), digits = 1),   # mean sampling interval
            Sampling_IntervalSD = round(sd((difftime), na.rm=T), digits = 1),       # standard deviation of the sampling interval
            Sampling_IntervalMIN = round(min((difftime), na.rm=T), digits = 1),     # minimum sampling interval
            Sampling_IntervalMAX = round(max((difftime), na.rm=T), digits = 1)) %>% # maximum sampling interval
  ungroup()

dfSubSamp_summary # view individual-level summary of sampling intervals


## plot histogram of the sampling sampling intervals for sub sampled data
## with a dashed line at the mean sampling interval
SubSamphist <- ggplot(df_subsampled, aes(x = as.numeric((difftime), na.rm=T))) +
  geom_histogram(bins = 50) + # how many bins the histogram should have
  geom_vline(xintercept = MeanSubInt, linetype = "dashed") + # the dashed line at the mean sampling interval
  scale_x_continuous(n.breaks = 15, limits = c(0, max(as.numeric(df_subsampled$difftime, na.rm=T))))+ # number of x axis breaks to have
  theme_light() + # change theme
  theme(axis.title = element_text(size = 16), axis.text = element_text(size =12)) + # change axis text formatting
  xlab(paste0("Interval (", Time_unit, ")")) + ylab("Frequency") # add axis labels

SubSamphist



#-----------------------------------#
##5. Save reprocessed data frame ####
#-----------------------------------#

## NOTE: If you are not happy with the subsampling then go back and try different intervals before 
##       saving the the data in this step

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
