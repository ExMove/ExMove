#' ----------------------------------------------------------------------- #
#' PROJECT: Using {gganimate} for animal movement data
#' CONTENTS: 
#'  - 0. Load libraries
#'  - 1. Prepare data
#'  - 2. Plot data
#'  - 3. Animate and save plot
#'  DEPENDENCIES:
#'  - Requires several packages to be installed (see below)
#'  - Source data: RFB_diagnostic.csv file output from data processing scripts
#' AUTHORS: Alice Trevail, Stephen Lang, Luke Ozsanlav-Harris, Liam Langley
#' #----------------------------------------------------------------------- #

# This code can be used to animate animal movement data using the {gganimate} package
# It works by first creating a static ggplot object with all the data plotted
# We then add some functions from {gganimate} to specify how we want the animation to work
# Finally we can animate the plot and how in RStudio, or save it to file
# There is additional commented code for including map tiles but an API is needed for this

# Some of the below code inspired by Hansen Johnson: https://hansenjohnson.org/post/animate-movement-in-r/
# For more info see documentation for {gganimate}(https://gganimate.com/) and {ggmap}(https://github.com/dkahle/ggmap)

#-------------------------------#
# 0. Load required libraries ####
#-------------------------------#

# Check if pacman is installed and install if not
if (!require("pacman")) install.packages("pacman")

# Load Libraries 
pacman::p_load(here, tidyverse, dplyr, lubridate, #tidyr
               sf, ggspatial, ggmap, #spatial
               rnaturalearth, rnaturalearthdata, #basemap data
               gganimate, scales) #others

#--------------------#
# 0. Prepare data ####
#--------------------#

# Read in downloaded file and filter data to one individual
df_all <- read_csv(here("DataOutputs","WorkingDataFrames","RFB_diagnostic.csv")) %>%
  filter(ID == "GV37501") #filter individual by ID

# Extract shapes of world basemap data as an sf object (low resolution)
basemap <- ne_countries(scale = "medium", returnclass = "sf") 

#----------------------------------#
# 1. Create static plot of data ####
#----------------------------------#

# Expansion factor (i.e. how much space to add around extent of datapoints)
expansion <- 0.2

# Make static plot
plot <- ggplot() +
  geom_sf(data = basemap, fill = "grey40", colour = NA) + #add basemap
  geom_path(data = df_all, aes(x = X, y = Y, group = ID), #add tracks as line
            alpha = 0.8, colour = "grey70", linewidth = 1) +
  geom_point(data = df_all, aes(x = X, y = Y, group = ID, colour = speed), #add data coloured by speed
             alpha = 0.7, shape = 16, size = 2.5) +
  scale_colour_viridis_c(option = "plasma") + #use viridis for points
  coord_sf(xlim = scales::expand_range(range(df_all$X, na.rm = TRUE), mul = expansion), 
           ylim = scales::expand_range(range(df_all$Y, na.rm = TRUE), mul = expansion)) +
  scale_size_continuous(range = c(0.1,10)) +
  labs(x = NULL, y = NULL) +
  theme_bw() + theme(panel.grid = element_blank()) +
  ggspatial::annotation_scale(location = "br", style = "bar", #add scale bar
                              bar_cols = c("grey50","grey50"), 
                              line_col = "grey50", text_col = "grey60", 
                              height = unit(0.15, "cm")) +
  NULL

#-----------------------------#
# 2. Animate plot and save ####
#-----------------------------#

# Add animation functions
anim <- plot + 
  transition_reveal(along = DateTime) + #reveal data over time
  shadow_wake(wake_length = .05, #length of shadow for geom_points
              exclude_layer = 2) + #excluding the geom_path data (otherwise line will also fade out)
  ggtitle("Date: {round(frame_along)}") #have title as the rounded time - could also use subtitle() here

# Test animation to render within RStudio (not smoothed or saved)
#animate(anim, nframes = 300, fps = 10, detail = 20)

# Calculate duration of data from plot (hours) to help choose duration of animation
anim_duration <- as.duration(max(plot$layers[[2]][["data"]]$DateTime)-
                               min(plot$layers[[2]][["data"]]$DateTime)) %>%
  as.integer(.,"hours") #convert to hours

# Save animation to file as gif (can ignore warning - we only have one individual)
anim_save(animation = anim, #name of animation object
          filename = here("DataOutputs", "Figures", "RFB_animation.gif"), #save location/name
          nframes = anim_duration*4, #4 number of frames for every hour of data (as above)
          detail = 20, #how many extra frames to add between each nframe (increase for smoother animation)
          height = 5, width = 5, units = "in", res = 150) #size and resolution of output
            