## Script to store Base R Code snippets for reference
## and for use in the Quarto Walkthrough
## Date created - 01/09/2022


#---------------------------#
##1. Read in data files  ####
#---------------------------#

## Read in and merge tracking data files
## Directly from github repository
## Create list of filenames to read in

## Read in list of files while maintaining filename as an ID column

raw_filenames <- list.files(path = filepath, recursive=TRUE, pattern=".csv", full.names = T)
ls_raw_files <-lapply(raw_filenames, function(x){fread(x)})
names(ls_raw_files) <- raw_filenames
head(ls_raw_files)

## rbind with data table to save ID column using rbindlist function from data.table

df_combined <- rbindlist(ls_raw_files, idcol = "filepath", fill = T) 

df_combined$ID <- str_sub(df_combined$filepath, start=IDstart, end=IDend) #extract ID string from filename


#---------------------------------------------------------#
##6. Optional Reprocessessing - trackSubSamp function  ####
#---------------------------------------------------------#

## might want to subsample the tracking data to a regular time interval
## use tracksumsample function - originally from E. Van Loon
## needs a data frame (TD), a time interval (dt) and a time unit (unit)

trackSubSamp <- function(TD, dt=10, unit='mins'){
  
  TD <- TD[order(TD$DateTime),] 
  
  # breakdown to datasets per bird
  unid = unique(TD$ID) 
  nrid = length(unid)
  TDall = list(nrid)  
  TDred = list(nrid)
  timestep = paste(dt,unit)
  # or based on the bird (CT)
  dati_start = min(TD$DateTime,na.rm=T)
  dati_end = max(TD$DateTime,na.rm=T)
  datiseq = seq(from=dati_start, to=dati_end, by=timestep)
  for (i in 1:nrid)
  {
    Dtemp = TD[TD$ID == unid[i],]
    idx = sapply(datiseq, function( x) which.min( abs( difftime( Dtemp$DateTime, x, units='mins') ) ) )
    TDall[[i]] = Dtemp
    TDred[[i]] = unique( Dtemp[idx,] ) # the function unique makes sure that the rows in Dtemp[idx,] are unique - so no duplicate points
  }
  
  TDred
}

# apply subsampling function to list
# reads out a list with a subsampled data frame for each individual

list_reprocessed <- trackSubSamp(df_diagnostic)

# convert list of individuals to a single data frame

df_reprocessed <- do.call("rbind", list_reprocessed)


# TIDYR version of subsampling (but output is different by 4 rows??)
