## Script to create Figure 6 for manuscript
## Maps showing the example tracking data sets from RFB, SAP and GWfG
## Date created - 24/01/2022

rm(list = ls())

#-----------------------------#
## Load required libraries ####
#-----------------------------#

library(tidyverse) #installed using install.packages("tidyverse")
library(lubridate) #installed as part of the tidyverse but needs to be loaded manually
library(here) #for reproducible filepaths
library(rnaturalearth) # for plotting maps
library(marmap) # for plotting amps
library(ggpubr)
library(magick)
library(patchwork)


#-----------------------#
##Create map for RFB ####
#-----------------------#

## Read in the data
## Set file path

filepath <- "DataOutputs/WorkingDataFrames/RFB_filtered.csv"

## Read in data

df_RFB <- read_csv(filepath)

## create a map of all points
## set the plot limits as the max and min lat/longs as the tracking data

## first set up a basemap to plot over
## use rnatural earth low res countries basemap
countries <- ne_countries(scale = "medium", returnclass = "sf")

## define min and max co-ordinates based on extent of tracking data
minlon <- min(df_RFB$Lon)
maxlon <- max(df_RFB$Lon)
minlat <- min(df_RFB$Lat)
maxlat <- max(df_RFB$Lat)

## load in topography basemap
## set limits slightly beyond tracking data to make a buffer
base_topography_map_RFB <- getNOAA.bathy(lon1 = minlon - 0.5, lon2 = maxlon + 0.5,
                                         lat1 = minlat - 0.5, lat2 = maxlat + 0.5, resolution = 1)

## fortify for plotting
base_topography_fort_RFB = fortify(base_topography_map_RFB)


## create base map with correct extent, topography, country outlines, etc.,
map_base_RFB <- ggplot() + 
                 geom_raster(data = base_topography_fort_RFB, aes(x=x, y=y, fill=z), alpha = 0.9) +
                 # add colour scheme for the fill
                 scale_fill_viridis_c(option="mako", name = "Elevation (m)") + 
                 # add map of countries over the top
                 geom_sf(data = countries, aes(geometry = geometry), fill = NA) + 
                 # set plot limits
                 coord_sf(xlim = c(minlon-0.3, maxlon+0.3), ylim = c(minlat-0.3, maxlat+0.3), crs = 4326,
                          expand = FALSE) +
                 # add labels
                 labs(x = "Longitude", y = "Latitude") +
                 # add title
                 ggtitle("A.") +
                 theme_light() +
                 theme(axis.text=element_text(colour="black"),
                       legend.position = "bottom",
                       axis.title.x = element_text(size = 15),
                       axis.text.x = element_text(hjust=0.7, angle = 45),
                       axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
                       axis.text.y = element_text(hjust=0.7,angle=45,vjust=0.3)) 


## map trips over the basemap, same colour for all birds
## add CP points where birds tagged
map_trips_RFB <- map_base_RFB + 
                  # add GPS points and paths between them
                  geom_path(data = df_RFB, aes(x = Lon, y = Lat, group = ID), colour = "#979A9A", alpha = 0.6, size = 1) +
                  geom_point(data = df_RFB, aes(x = Lon, y = Lat, group = ID), col = "#CB4335", alpha = 0.9, size = 0.5) +
                  # add central place points
                  geom_point(data = df_RFB, aes(x = CPLon, y = CPLat),
                  colour = "black", fill ="#979A9A", shape = 23, size = 2)


#-----------------------#
##Create map for GWfG####
#-----------------------#

## Read in the data
## Set file path

filepath <- "DataOutputs/WorkingDataFrames/GWfG_filtered.csv"

## Read in data

df_GW <- read_csv(filepath)

## create a map of all points
## set the plot limits as the max and min lat/longs as the tracking data

## first set up a basemap to plot over
## use rnatural earth low res countries basemap
countries <- ne_countries(scale = "medium", returnclass = "sf")

## define min and max co-ordinates based on extent of tracking data
minlon <- min(df_GW$Lon)
maxlon <- max(df_GW$Lon)
minlat <- min(df_GW$Lat)
maxlat <- max(df_GW$Lat)

## load in topography basemap
## set limits slightly beyond tracking data to make a buffer
base_topography_map_GW <- getNOAA.bathy(lon1 = minlon - 2, lon2 = maxlon + 2,
                                         lat1 = minlat - 2, lat2 = maxlat + 2, resolution = 1)

## fortify for plotting
base_topography_fort_GW = fortify(base_topography_map_GW)


## create base map with correct extent, topography, country outlines, etc.,
map_base_GW <- ggplot() + 
                geom_raster(data = base_topography_fort_GW, aes(x=x, y=y, fill=z), alpha = 0.9) +
                # add colour scheme for the fill
                scale_fill_viridis_c(option="mako", name = "Elevation (m)") + 
                # add map of countries over the top
                geom_sf(data = countries, aes(geometry = geometry), fill = NA) + 
                # set plot limits
                coord_sf(xlim = c(minlon-2, maxlon+2), ylim = c(minlat-2, maxlat+2), crs = 4326,
                         expand = FALSE) +
                # add labels
                labs(x = "Longitude", y = "Latitude") +
                # add title
                ggtitle("B.") +
                theme_light() +
                theme(axis.text=element_text(colour="black"),
                      legend.position = "bottom",
                      axis.text.x = element_text(hjust=0.7, angle = 45),
                      axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
                      axis.text.y = element_text(hjust=0.7,angle=45,vjust=0.3))

  
## map trips over the basemap, same colour for all birds
## add CP points where birds tagged
map_trips_GW <- map_base_GW + 
                 # add GPS points and paths between them
                 geom_path(data = df_GW, aes(x = Lon, y = Lat, group = ID), colour = "#979A9A", alpha = 0.6, size = 1) +
                 geom_point(data = df_GW, aes(x = Lon, y = Lat, group = ID), col = "#CB4335", alpha = 0.9, size = 0.5) +
                 # add central place points
                 geom_point(data = df_RFB, aes(x = CPLon, y = CPLat),
                            colour = "black", fill ="#979A9A", shape = 23, size = 2)


#-----------------------#
##Create map for SAP ####
#-----------------------#

## Read in the data
## Set file path

filepath <- "DataOutputs/WorkingDataFrames/SAP_filtered.csv"

## Read in data

df_SAP <- read_csv(filepath)

## create a map of all points
## set the plot limits as the max and min lat/longs as the tracking data

## first set up a basemap to plot over
## use rnatural earth low res countries basemap
countries <- ne_countries(scale = "medium", returnclass = "sf")

## define min and max co-ordinates based on extent of tracking data
minlon <- min(df_SAP$Lon)
maxlon <- max(df_SAP$Lon)
minlat <- min(df_SAP$Lat)
maxlat <- max(df_SAP$Lat)

## load in topography basemap
## set limits slightly beyond tracking data to make a buffer
base_topography_map_SAP <- getNOAA.bathy(lon1 = minlon - 0.2, lon2 = maxlon + 0.2,
                                        lat1 = minlat - 0.2, lat2 = maxlat + 0.2, resolution = 1)

## fortify for plotting
base_topography_fort_SAP = fortify(base_topography_map_SAP)


## create base map with correct extent, topography, country outlines, etc.,
map_base_SAP <- ggplot() + 
                 geom_raster(data = base_topography_fort_SAP, aes(x=x, y=y, fill=z), alpha = 0.9) +
                 # add colour scheme for the fill
                 scale_fill_viridis_c(option="mako", name = "Elevation (m)") + 
                 # add map of countries over the top
                 geom_sf(data = countries, aes(geometry = geometry), fill = NA) + 
                 # set plot limits
                 coord_sf(xlim = c(minlon-0.1, maxlon+0.1), ylim = c(minlat-0.1, maxlat+0.1), crs = 4326,
                 expand = FALSE) +
                 # add labels
                 labs(x = "Longitude", y = "Latitude") +
                 # add title
                 ggtitle("C.") +
                 theme_light() +
                 theme(axis.text=element_text(colour="black"),
                       legend.position = "bottom",
                       axis.title.x = element_text(size = 15),
                       axis.text.x = element_text(hjust=0.7, angle = 45),
                       axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
                       axis.text.y = element_text(hjust=0.7,angle=45,vjust=0.3))

## map trips over the basemap, same colour for all birds
## add CP points where birds tagged
map_trips_SAP <- map_base_SAP + 
  # add GPS points and paths between them
  geom_path(data = df_SAP, aes(x = Lon, y = Lat, group = ID), colour = "#979A9A", alpha = 0.6, size = 1) +
  geom_point(data = df_SAP, aes(x = Lon, y = Lat, group = ID), col = "#CB4335", alpha = 0.9, size = 0.5)


#-------------------------------#
##Create combined map figure ####
#-------------------------------#

## make composite figure using patchwork
## create figure

fig6 <- map_trips_RFB + map_trips_GW + map_trips_SAP

## set file parameters for saving the figure
## define device to read plots out as e.g. tiff/jpeg
device <- "png"

## define units for plot size - usually mm
units <- "mm"

## define plot resolution in dpi - 300 usually minimum
dpi <- 300

## define filepath to read out plots 

out_path <- here("Manuscript","Figure Images")

## save figure out as a png

ggsave(plot = fig6, 
       filename = "Figure_6.png",
       device = device,
       path = out_path,
       units = units, width = 275, height = 125, dpi = dpi,   
)


#----------------------------------------#
##Annotate figure with species images ####
#----------------------------------------#

## read fig6 back in to annotate with the graphics
fig6_img <- image_read(here("Manuscript","Figure Images", "Figure_6.png"))


## Import SAP graphic
RFB_image <- image_read(here("Manuscript","Figure Images", "RFB_image.png"))
RFB_image_altered <- RFB_image %>% image_scale("240x240") %>%image_flop()

GWF_image <- image_read(here("Manuscript","Figure Images", "GWF_image.png"))
GWF_image_altered <- GWF_image %>% image_scale("250x250") %>%image_flop()

SAP_image <- image_read(here("Manuscript","Figure Images", "SAP_image.png"))
SAP_image_altered <- SAP_image %>% image_scale("240x240") %>%image_flop()


## create annotated map

a <- image_composite(fig6_img, RFB_image_altered, offset = "+650+50")

b <- image_composite(a, GWF_image_altered, offset = "+1900+75")

c <- image_composite(b, SAP_image_altered, offset = "+2850+100")


## save map image

image_write(c, here("Manuscript", "Figure Images", "Figure_6_annotated.png"))


#------------------#
##End of script ####
#------------------#