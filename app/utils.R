## This script contains functions that are used in the server 

## function to calcualte standard error for a vector of avlues 
se <- function(x) sqrt(var(x, na.rm = T) / length(x[!is.na(x)]))


## Function to calcualte leaflet map of tracking data
## Made into a function so that it is easier to work with when we want to change components of the map
## Mapdata = the tracking data used to make the map, should be the same for most uses
## column = the column that we want to color the map by 
## label = the label for the map legend
## NOTE** at a later date may want to put the filtering step in here too, that way we could call different data sets, i.e. the classified foraging trips
MapFunc <- function(Mapdata = Mapdata, column = column, label = label, vartype = vartype, PopSelectMap = PopSelectMap, BirdSelectMap = BirdSelectMap){
  
  if(PopSelectMap == "Individual"){Mapdata <- filter(Mapdata, ID == BirdSelectMap)}
  
  ## get the column index for the variable the map needs to be coloured by
  index <- which(colnames(Mapdata)==column)
  
  ## Filter any NAs in the chosen colour column as this will break the map if the NAs are kept
  MapTracks <- Mapdata[is.na(Mapdata[,index])==FALSE,]
  
  ## create a vector of all the values going to be used to colour the points
  colour_column <- data.frame(MapTracks[,index])
  
  #if(vartype == "numeric" | vartype == "time"){colour_column <- as.numeric(colour_column[,1])}
  if(vartype == "numeric" | vartype == "Circ"){ colour_column <- as.numeric(colour_column[,1]) }
  if(vartype == "factor"){ colour_column <- as.factor(colour_column[,1]) }
  if(vartype == "time"){ colour_column <- as.numeric(hour(hms(colour_column[,1]))) }
  
  ## make locations an sf object
  sf_locs_inc <- st_as_sf(MapTracks, coords = c("Lon","Lat")) %>% 
    st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  ## create the lines between locations (as an sf object)
  sf_lines_inc <- sf_locs_inc %>%
    st_geometry() %>% 
    st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs_inc$ID))) %>% 
    st_cast("MULTILINESTRING") %>% 
    st_sf(tripNo.subsampled = as.factor(unique(sf_locs_inc$ID)))
  
  ## create a color palette for plotting
  ## Needs to be continuous for categorical variables may just have to add an if statment here
  if(vartype == "numeric" | vartype == "time"){pal <- colorNumeric(palette = "viridis", domain = colour_column)}
  if(vartype == "factor"){pal <- colorFactor(palette = "Spectral", domain = colour_column)}
  if(vartype == "Circ"){pal <- colorNumeric(palette = colorRamp(c("#481567FF", "#404788FF", "#33638DFF", "#29AF7FFF", "#B8DE29FF", "#FDE725FF", 
                                                                  "#B8DE29FF", "#29AF7FFF", "#33638DFF", "#404788FF", "#481567FF"), space = "rgb", interpolate = "linear"), domain = colour_column)}
  
  ## Use leaflet to create the plot
  MAP <- leaflet() %>% 
    
    ##adding various tiles and map backdrops - here set up to flick between them interactively
    addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>% 
    addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
    addTiles(group = "OSM(default)") %>% 
    addLayersControl(baseGroup = c("OSM(default)", "ESRI", "GoogleEarth")) %>%
    
    ##add the track as polylines 
    addPolylines(data = sf_lines_inc, weight = 2, color = "black") %>% 
    ## add the locations and colour by the chosen variable
    addCircleMarkers(data = sf_locs_inc, radius = 3, weight = 2, opacity = 0.8, fillOpacity = 0.8, color = ~pal(colour_column)) %>% 
    addLegend(position = "bottomleft", pal = pal, values = colour_column, title = label, opacity = 1) %>% 
    ## add a scale bar
    addScaleBar(position = "bottomright")
  
  ## return the map
  MAP
  
}
