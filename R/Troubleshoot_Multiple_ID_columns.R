#' ----------------------------------------------------------------------- #
#' PROJECT: Working with multiple ID columns
#' CONTENTS: 
#'  - Load libraries
#'  - A bit more information about this script 
#'  - DeployID: One individual, multiple tag deployments
#'  - Year: One individual, multiple years
#'  - TagID: One tag, multiple individuals
#'  DEPENDENCIES:
#'  - Requires tidyverse to be installed
#' AUTHORS: Alice Trevail, Stephen Lang, Luke Ozsanlav-Harris, Liam Langley
#' #----------------------------------------------------------------------- #


#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(tidyverse)
library(lubridate)


#-----------------------------------------------#
## A bit more information about this script  ####
#-----------------------------------------------#

## This code is a slimmed-down version of the main workflow
## It will all fit into the main workflow, so that you can tailor it to your own data

## The raw data structure is the same = one ID column (TagID/Individual ID), one DateTime column, and sensor data (Lat and Lon)
## The metadata structure is the same = Individual ID column, deploy & retrieve DateTimes, plus additional info = other ID columns (TagID/DeployID)
## We combine raw and meta data and filter the data using the same piped functions as in the main workflow

## Important terminology = 
## ID always refers to the unique animal ID, e.g., bird metal ring/band number
## Other ID columns are named accordingly
## TagID = unique tag ID, e.g., manufacturer's sensor ID number
## DeployID = unique deployment ID, e.g., user defined deployment period

## To demonstrate use of multiple ID columns, we simulate some data in this script
## Dataframe names correspond to the main workflow, but with the addition of "_sim"
## E.g., df_raw in the main workflow corresponds to df_sim_raw here
## Dates and times are based on current system time
## Data are therefore in current time zone, so we won't worry about transformations here
## We add sensor data using arbitrary Lat and Lon values from 1 to 12


#---------------------------------------------#
## DeployID: One individual, multiple tags ####
#---------------------------------------------#

## If two tags have been deployed on one individual, we can use DeployID to sort the data
## For example, two different tag deployments at different stages of the season:

## Let's simulate some raw tracking data for two deployment periods on one individual
## DateTime column comprised of two time periods: 14-9 hours ago, and 6-1 hours ago
## Lat and Lon are numeric
df_sim_raw <- tibble(ID = 1, 
                        DateTime = Sys.time() - period(hour = c(14:9, 6:1)), 
                        Lat = 1:12, 
                        Lon = 1:12)
df_sim_raw


## Now, let's simulate some meta data:
## Comprised of: individual ID, deployment ID, deployment and retrieval times for each deployment
## Deployment 1 was from 13.5 hours - 9.5 hours ago
## Deployment 2 was from 5.5 hours - 1.5 hours ago
df_sim_meta <- tribble(
  ~ID, ~DeployID,  ~Deploydatetime,                            ~Retrievedatetime,
  1,   1,          Sys.time() - period(hour = 13, min = 30),   Sys.time() - period(hour = 9, min = 30), 
  1,   2,          Sys.time() - period(hour = 5, min = 30),    Sys.time() - period(hour = 1, min = 30)
)
df_sim_meta


## Left-join raw and meta data by ID to keep all rows of the raw data
## Each row of the raw data is duplicated for every corresponding row in metadata (i.e., each Deploy ID)
df_sim_metamerged <- df_sim_raw %>%
  left_join(., df_sim_meta, by="ID") 
df_sim_metamerged

## Filter the data by deployment period
## This keeps only the rows corresponding to times of each deployment period
df_sim_clean <- df_sim_metamerged %>%
  filter(Deploydatetime < DateTime & DateTime < Retrievedatetime)
df_sim_clean



#------------------------------------------#
## Year: One individual, multiple years ####
#------------------------------------------#

## If one individual has been tracked during multiple years, we can use Year to sort the data
## This is the same process as with DeployID, but using Year instead as an additional example

## Let's simulate some raw tracking data for one individual tracked over two years
## DateTime column comprised of two time periods: 6-1 hours ago last year, and 6-1 hours ago this year
## Lat and Lon are numeric
df_sim_raw <- tibble(ID = 1, 
                        DateTime = c(Sys.time() - period(year = 1, hour = 6:1),
                                     Sys.time() - period(hour = 6:1)), 
                        Lat = 1:12, 
                        Lon = 1:12)
df_sim_raw


## Now, let's simulate some meta data:
## Comprised of: individual ID, Year, deployment and retrieval times for each deployment
## Deployment 1 was from 5.5 hours - 1.5 hours ago last year
## Deployment 2 was from 5.5 hours - 1.5 hours ago this year
df_sim_meta <- tribble(
  ~ID,  ~Year,                                        ~Deploydatetime,                                    ~Retrievedatetime,
  1,    substr(Sys.Date() - period(year = 1), 1, 4),  Sys.time() - period(year = 1, hour = 5, min = 30),  Sys.time() - period(year = 1, hour = 1, min = 30), 
  1,    substr(Sys.Date(), 1, 4),                     Sys.time() - period(hour = 5, min = 30),            Sys.time() - period(hour = 1, min = 30)
)
df_sim_meta


## Left-join raw and meta data by ID to keep all rows of the raw data
## Each row of the raw data is duplicated for every corresponding row in metadata (i.e., each Year)
df_sim_metamerged <- df_sim_raw %>%
  left_join(., df_sim_meta, by="ID") 
df_sim_metamerged

## Filter the data by deployment period
## This keeps only the rows corresponding to times of each deployment period
df_sim_clean <- df_sim_metamerged %>%
  filter(Deploydatetime < DateTime & DateTime < Retrievedatetime)
df_sim_clean



#------------------------------------------#
## TagID: One tag, multiple individuals ####
#------------------------------------------#

## If one tag has been deployed on multiple individuals, we can use TagID to sort the data
## For example, a satellite tag deployed twice on two individuals, for which one data file is downloaded via an online portal

## Let's simulate some raw tracking data, with a TagID column
## DateTime column comprised of 12 hours up to now
## Lat and Lon are numeric
df_sim_raw <- tibble(TagID = 1, 
                     DateTime = Sys.time() - period(hour = 12:1), 
                     Lat = 1:12, 
                     Lon = 1:12)
df_sim_raw


## Now, let's simulate some meta data:
## Comprised of: TagID, individual ID, deployment and retrieval times for each individual
## Tag 1 was deployed on individual 1 from 11.5 hours - 7.5 hours 
## Tag 1 was then deployed on individual 2 from 5.5 hours - 2.5 hours
df_sim_meta <- tribble(
  ~TagID,  ~ID,  ~Deploydatetime,                            ~Retrievedatetime,
  1,       1,    Sys.time() - period(hour = 11, min = 30),   Sys.time() - period(hour = 7, min = 30), 
  1,       2,    Sys.time() - period(hour = 5, min = 30),    Sys.time() - period(hour = 1, min = 30)
)
df_sim_meta

## Left-join raw and meta data by TagID to keep all rows of the raw data
## Each row of the raw data is duplicated for every corresponding row in metadata (i.e., each ID)
df_sim_metamerged <- df_sim_raw %>%
  left_join(., df_sim_meta, by="TagID")
df_sim_metamerged

## Filter the data by deployment period
## This keeps only the rows corresponding to the deployment period for each individual
df_sim_clean <- df_sim_metamerged %>%
  filter(Deploydatetime < DateTime & DateTime < Retrievedatetime)
df_sim_clean

