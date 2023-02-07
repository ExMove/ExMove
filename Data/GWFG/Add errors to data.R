## load packages
pacman::p_load(tidyverse, data.table, lubridate)

## read in the clean data
Data <- fread("Data/GWFG/Migration_tracks.csv")

## create summary of start and end time for the metadata file
Summary <- Data %>% 
           group_by(device_ID) %>% 
           summarise(StartDate = min(ymd_hms(timestamp)),
                     EndDate = max(ymd_hms(timestamp)))

## Add no data values
Data$longitude[111] <- 0
Data$longitude[1123] <- 0
Data$longitude[2561] <- 0
Data$latitude[5001] <- 0

## Add some NAs
Data$timestamp[1556] <- NA
Data$timestamp[2751] <- NA
Data$timestamp[4991] <- NA
Data$latitude[4511] <- NA


## Add some fixes in the wrong location
Data$longitude[1121] <- Data$longitude[1121] + 1.1
Data$longitude[998] <- Data$longitude[998] + 2
Data$longitude[5201] <- Data$longitude[5201] + 0.5
Data$latitude[3245] <- Data$latitude[3245] + 0.9


## Now read the data back out
write_csv(Data, file = "Data/GWFG/Migration_tracks_errors.csv")