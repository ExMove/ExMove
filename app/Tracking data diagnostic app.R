## Shiny app for exploring how data filters/cleaning/re sampling influence the data and derived stats
## Created: 28/07/2022


## packages required
pacman::p_load(tidyverse, lubridate, sf, shiny, shinydashboard, ggplot2, rnaturalearth, DT, shinyBS, leaflet)


# read in data to check that code works without running the app
# data = read_csv("TestDataOutputs/WorkingDataFrames/RFB_diagnostic.csv")
# tracks <- data




#------------------------------------#
#### 1. Set up the User Interface ####
#------------------------------------#

#-----------------------------#
#### 1.0 Set Styles for UI ####
#-----------------------------#

## This just allows my to access cutom CSS styles for certain things
## Might be owrth putting then in a styles.css file if there are more 
STYLES <- list(
  help_button = "background-color:#E76F62; width:100%;"
)


#-----------------------------------#
#### 1.1 Set up functions for UI ####
#-----------------------------------#

## help module ui
## This function creates the help buttons, uses it a lot so made it into a function
## If this becoes long then it might be worth putting them in an R file that we then call in using source()
help_button <- function(module_id, style = STYLES$help_button, text = "Help") {
  prefix <- NS(module_id)
  actionButton(prefix("help"),
               text,
               icon = shiny::icon("question", verify_fa = FALSE),
               style = style)
}


#----------------------------#
#### 1.2 Set up UI header ####
#----------------------------#

## Set up the header
header <- dashboardHeader(title = "MoveExplore App")




#-----------------------------#
#### 1.3 Set up UI sidebar ####
#-----------------------------#

## Set up the side bar contents
sidebar <- dashboardSidebar(fileInput(inputId = "filedata", label = "Upload data (.csv)", accept = c(".csv")),
                            sidebarMenu(menuItem("Introduction", tabName = "Intro", icon = icon("book")),
                                        menuItem("Data Filters", tabName = "Filterdata", icon = icon("database")),
                                        menuItem("Foraging Trips", tabName = "ForagingTrips", icon = icon("plane-departure")),
                                        menuItem("Sub-sampling", tabName = "SubSamp", icon = icon("timeline")),
                                        menuItem("Interactive Map", tabName = "IntMap", icon = icon("globe"))))



#--------------------------#
#### 1.4 Set up UI body ####
#--------------------------#

##-- p0.0 Introduction Tab --##
## Can just include a markdown here instead of using the tags$, might be easier so will switch in future
AppOverview <- box(width = 12, title = "App Overview", solidHeader = TRUE, status = "primary",
                   fluidRow(column(width = 10,includeMarkdown("help/p0.0 Introduction Tab.md")))
                   )


##-- p1.0 Filter Data Tab --##

##-- p1.1 Tracking data overview box --##
TrackingDataOverview <- box(infoBoxOutput("IndividualIDs"),infoBoxOutput("NoLocations"),infoBoxOutput("DateRange"),
                            width = 12, title = "Tracking Data Overview", solidHeader = FALSE, status = "primary")

##-- p1.2 User input box --##
FilterUserInput <- box(width = 12, title = "User Input", solidHeader = TRUE, status = "warning",
                       fluidRow(column(width = 3, actionButton("FilterButton", "Apply data filters"),
                                bsTooltip(id = "FilterButton", title = "Move the sliders and then press this button to filter the data. Plots update automatically")),
                                column(2, offset = 7, help_button("FilterHelp"))),
                       fluidRow(column(width = 12, sliderInput("SpeedFilter", "Speed Filter (m/s)", min = 0, max = 50, step =0.1, value = 12))),
                       fluidRow(column(width = 12, sliderInput("NetDistFilter", "Net squared displacement filter (km)", min = 0, max = 500, step =1, value = 100))),
                       fluidRow(column(width = 6, dateRangeInput("DateRange", "Select Date Range", start = "2020-11-29", end = "2021-10-17"))))

##-- p1.3 Summary table boxes --##
RAWTableF <- box(DTOutput("RAWTable"), width = 6, height = "440px", title = "Summary Table (Full dataset)", 
                 solidHeader = TRUE, status = "primary")

FiltTableF <-box(DTOutput("SumTable"), textOutput("FilterSelection"), width = 6, height = "440px", title = "Summary Table (Filtered)", 
                 solidHeader = TRUE, status = "primary")

##-- p1.4 Summary map --##
FilterMap <- box(plotOutput("trackmap"), width = 9, title = "Map of locations", solidHeader = TRUE, status = "primary")

FilterMapSelectors <- box(selectInput("BirdSelect", "Select Animal ID", c("Select file first")),
                          radioButtons("PopSelect", "Pop vs Individual", c("Population", "Individual")),
                          width = 3, title = "Select Plot Options", solidHeader = TRUE, status = "primary")

##-- p1.5 Summary Histograms --##
FiltTabbedHistograms <- tabBox(title = "Diagnostic Plots", id = "tabset1", width = 12, side = "right",
                               tabPanel("Speed Histogram", plotOutput("speedhist")),
                               tabPanel("Net sq. displacement histogram", plotOutput("nethist")))




##-- p2.0 Foraging Trip Tab --##

##-- p2.1 User Input box --##
ForgaingUserInput <- box(width = 12, title = "User Input", solidHeader = TRUE, status = "warning",
                         fluidRow(column(width = 3, actionButton("TripStatButton", "Calculate Trip Statistics")),
                                  column(width = 2, offset = 7, help_button("ForageHelp"))),
                         fluidRow(column(width = 12, sliderInput("ColTripBuffer", "Trip Start Distance (km)", min = 0, max = 20, step = 0.2, value = 1))),
                         fluidRow(column(width = 12, sliderInput("MinTripTime", "Minimum Trip Duration (mins)", min = 0, max = 240, step =1, value = 15))),
                         fluidRow(column(width = 2, radioButtons("UserBox", "Defining colony buffer", choices = c("Colony trip buffer", "Use user define box"), selected = "Colony trip buffer")),
                                  column(width = 2, numericInput("MinLat", "Minimum Latitude:", value = 0, min = -180, max = 180)),
                                  column(width = 2, numericInput("MaxLat", "Maximum Latitude:", value = 0, min = -180, max = 180)),
                                  column(width = 2, numericInput("MinLon", "Minimum Longitude:", value = 0, min = -180, max = 180)),
                                  column(width = 2, numericInput("MaxLon", "Maximum Longitude:", value = 0, min = -180, max = 180))))

##-- p2.2 Summary Table --##
ForagingSummaryTable <- tabBox(title = "Foraging Trip Summary", id = "tabset2", width = 12, side = "right",
                               tabPanel("Current", DTOutput("TripTableFilt")),
                               tabPanel("Previous", DTOutput("TripTablePrev")))

##-- p2.3 Trips over time box --##
ForgaingTripsPlot <- box(plotOutput("TripClassPlot"), width = 9, title = "Foraging Trip Clasification", 
                         solidHeader = TRUE, status = "primary")

ForgaingTripsPlotSelector <- box(selectInput("BirdSelect2", "Select Animal ID", c("Select file first")),
                                 radioButtons("PopSelect2", "Pop vs Individual", c("Population", "Individual")),
                                 width = 3, title = "Select Plot Options", solidHeader = TRUE, status = "primary")

##-- p2.4 Foraging trip map --#
ForageMap <- box(plotOutput("tripmap"), width = 6, title = "Forgaing Trip Map", solidHeader = TRUE, status = "primary")

##-- p2.5 Trip duration histogram --##
ForagingDurationHist <- box(plotOutput("TripHist"), width = 6, title = "Foraging Trip Duration Histogram", 
                            solidHeader = TRUE, status = "primary")
  



##-- p3.0 Sub-sampling Tab --##

##-- p3.1 Sampling Interval Overview --##
SamplingIntervalOverview <- box(infoBoxOutput("MeanSampInt"),infoBoxOutput("SDSampInt"),infoBoxOutput("RangeSampInt"),
                                width = 12, title = "Sampling Interval Overview", solidHeader = FALSE, status = "primary")

##-- p3.2 User Input --##
SubSampUserInput <- box(width = 12, title = "User Input", solidHeader = TRUE, status = "warning",
                        fluidRow(column(width = 3, actionButton("SubSampButton", "Sub-sample data")),
                                 column(width = 2, offset = 7, help_button("SubSampHelp"))),
                        fluidRow(column(width = 12, sliderInput("SubSampInterval", "Required Sampling Interval (mins)", min = 0, max = 300, step =1, value = 60))))

##-- p3.3 Summary Table --##
SubSampSummaryTable <- box(DTOutput("RAWSubSampTable"), textOutput("SubSampSelection"), width = 12, height = "440px", 
                           title = "Summary of Sub-sampled Data", solidHeader = TRUE, status = "primary")

##-- p3.3 Sampling Interval Histogram --##
SubSampHistogram <- box(plotOutput("SubSampHist"), width = 12, title = "Histogram of Sampling Intervals", solidHeader = TRUE, status = "primary")

##-- 3.4 Leaflet map of bounding box --##
Bounding_box_map <- uiOutput("Bounding_box_map")


##-- p4.0 Interactive map Tab --##




##-- Set up the dashboard body contents --##
body <- dashboardBody(
  tabItems(tabItem(tabName = "Intro",
                   fluidRow(AppOverview)
                   ),
           tabItem(tabName = "Filterdata", # Boxes need to be put in a row (or column)
                   fluidRow(TrackingDataOverview),
                   fluidRow(FilterUserInput),
                   fluidRow(RAWTableF,
                            FiltTableF),
                   fluidRow(FilterMap,
                            FilterMapSelectors),
                   fluidRow(FiltTabbedHistograms)
                   ),
           tabItem(tabName = "ForagingTrips",
                   fluidRow(ForgaingUserInput),
                   fluidRow(Bounding_box_map),
                   fluidRow(ForagingSummaryTable),
                   fluidRow(ForgaingTripsPlot, ForgaingTripsPlotSelector),
                   fluidRow(ForageMap, ForagingDurationHist),
                   ),
            tabItem(tabName = "SubSamp",
                    fluidRow(SamplingIntervalOverview),
                    fluidRow(SubSampUserInput),
                    fluidRow(SubSampSummaryTable),
                    fluidRow(SubSampHistogram)
                    ),
            tabItem(tabName = "IntMap")))


#-------------------------#
#### 1.5 Create the UI ####
#-------------------------#

## Create the dashboard page 
ui <- dashboardPage(header, sidebar, body, skin = "red")






#---------------------------#
#### Creating the server ####
#---------------------------#

## Set up any global options for the server here ##

## this increases the max file that can be uploaded to the shiny app, currently 500MB
## If this is not set then i think that the max file that people could upload is only 5MB
options(shiny.maxRequestSize = 500*1024^2)

## messages sent between  R server and  web browser client will be printed on the console. Useful for debugging.
# options(shiny.trace = TRUE)

  


## Create the server function
## Might be worth putting the UI and Server in different scripts if they are gettig a bit big
server <- function(input, output, session) {
  
  #-------------------------#
  #### 1. Server Modules ####
  #-------------------------#
  
  ## get the working directory, this is need to read in the help files
  APP_wd <- getwd()

  # help module server ----
  # need app folder so was placed here, otherwise need to add parameter
  click_help <- function(input, output, session, title, size, file){
    observeEvent(input$help, {
      showModal(modalDialog(
        title = title, size = size,
        fluidPage(includeMarkdown(file.path(APP_wd, file))),
        easyClose = TRUE, fade = FALSE
      ))
    })
  }
  

  #---------------------------#
  #### 2. Data Filters Tab ####
  #---------------------------#
  
  #-----------------------#
  #### 2.0 Help Button ####
  #-----------------------#
  
  # the app option help is registered after help function is ready
  callModule(click_help, "FilterHelp", title = "Help page",
             size = "l", file = "help/p1.0 Filter Data Tab.md")
  
  #-------------------------------#
  #### 2.1 Read in data as csv ####
  #-------------------------------#
  
  ## This reads in my csv file, the UI allows me to initially just find the path for my file
  
  ## create an empty reactive value that we will later put the tracking data into
  tracks <- reactiveValues(data = NULL)
  
  
  ## Now read in the data if a file path is specified
  ## I think I could change this to just read in a object in the R environment
  data <- reactive({
    
    req(input$filedata)
    read_csv(input$filedata$datapath)
    
  })
  
  ## When i read in a file also assign the data to a reactive values
  ## This is important, it allows me to call the RAW data and also filter the data separately
  observeEvent(input$filedata, {
    
    tracks$data <- data()
    
  })
  
  ## create min and max lat/longs for plotting, 
  ## doing this with data() instead of tracks$data prevents map extents changing when we filter the data
  minlon <- eventReactive(input$filedata, { x <- data()
  return(min(x$Lon))})
  maxlon <- eventReactive(input$filedata, { x <- data()
  return(max(x$Lon)) })
  minlat <- eventReactive(input$filedata, { x <- data()
  return(min(x$Lat)) })
  maxlat <- eventReactive(input$filedata, { x <- data()
  return(max(x$Lat)) })
  
  
  ## create basemap that can be used for plotting, this way it only has to be done once
  Basemap <- reactive({
    
    ##get the max lat and longs of the data
    minlon <- minlon()
    maxlon <- maxlon()
    
    minlat <- minlat()
    maxlat <- maxlat()
    
    ## downlodad the country outline of the world
    countries <- ne_countries(scale = "medium", returnclass = "sf")
    
    ## make ggplot
    ## might want to pre-render all of the tracks with just data(), should speed up the app
    basemap <- ggplot() + 
      ##add map of countries over the top
      geom_sf(data = countries, aes(geometry = geometry)) +
      ##set plot limits
      coord_sf(xlim = c(minlon, maxlon), ylim = c(minlat, maxlat), crs = 4326)
    
    basemap
    
  })
  
  
  #-------------------------------#
  #### 2.2 Update input values ####
  #-------------------------------#
  
  ## This updates the select Input drop down options based off of the file that I read in
  observe({
    
    x <- data()
    
    # I need to identify which select input i want to update and then tell it what the new choices should be
    updateSliderInput(session, "SpeedFilter", value= ceiling(max(x$speed, na.rm = T)), max= ceiling(max(x$speed, na.rm = T)))
    updateSliderInput(session, "NetDistFilter", value= ceiling(max(x$netdisp, na.rm = T)/1000), max= ceiling(max(x$netdisp, na.rm = T)/1000))
    updateDateRangeInput(session, "DateRange", min = min(ymd(x$Date), na.rm = T), max= max(ymd(x$Date), na.rm = T),
                         start = min(ymd(x$Date), na.rm = T), end = max(ymd(x$Date), na.rm = T)) 
    updateSelectInput(session, "BirdSelect",choices = unique(x$ID))
    
  })
  
  
  
  #------------------------------#
  #### 2.3 Apply data filters ####
  #------------------------------#
  
  ## Here I am observing if anyone has pressed the filter data button, 
  ## When it is pressed the trackingdata is filtered
  observeEvent(input$FilterButton, {
      
    ## Show notification while the data is sub-sampled
    FilterNotif <- showNotification(shiny::span(shiny:::icon("hourglass-start"), "Filtering data.."),
                                    type = "message", duration = NULL)
    
    ## filter data, this happens whenever the FilterButton is pressed
    tracks$data <- filter(data(), speed <= input$SpeedFilter & netdisp <= (input$NetDistFilter*1000) &
                             Date >= ymd(input$DateRange[1]) & Date <= ymd(input$DateRange[2]))
    
    ## remove the notification
    removeNotification(FilterNotif)
    
  })
  
  
  #-----------------------------------------------------#
  #### 2.4 Create info boxes that summaries GPS data ####
  #-----------------------------------------------------#
  
  ## Create an Info box with the ID of the individual
  output$IndividualIDs <- renderInfoBox({
    
    infoBox("No. of Individuals", length(unique(data()$ID)), color = "purple", icon = shiny::icon("piggy-bank"))
    
  })
  
  ## Create an Info box with the the number of GPS locations
  output$NoLocations <- renderInfoBox({
    
    data <- data()
    data <- as.data.frame(data)
    infoBox("Number of Locations", nrow(data), color = "blue", icon = shiny::icon("globe"))
    
  })
  
  ## Create an Info box with the date rnage of the GP\S data for that individual
  output$DateRange <- renderInfoBox({
    
    data <- data()
    data <- as.data.frame(data)
    data$DateTime <- lubridate::ymd_hms(data$DateTime)
    infoBox("Date Range", paste0(min(data$DateTime), " - ", max(data$DateTime)), color = "teal", icon = shiny::icon("calendar"))
    
  })
  
  
  #-----------------------------------------------------------#
  ####  2.4 make summary table for individuals in the data ####
  #-----------------------------------------------------------#
  
  ## create a reactive value here, means I only have to create the original summary once for the full trakcing data set
  dataRawSum <- reactive({
    
    df_RAWsummarise <- data()
    
    dfRAW_summary_inds <- df_RAWsummarise %>% 
      group_by(ID) %>%
      summarise(NoPoints = NROW(ID),
                NoUniqueDates = length(unique(Date)),
                FirstDate = as.character(as.Date(min(Date))),
                LastDate = as.character(as.Date(max(Date))))
    
    dfRAW_summary_inds
    
  })
  
  
  ## Make the table the summarises the new filtered data
  output$SumTable = renderDT({
    
    if (is.null(tracks$data)) return()
    
    # read in the filtered data
    df_tosummarise <- tracks$data
    
    # read in the summarised raw data
    df_RAWsummarise <- dataRawSum() %>% 
      select(ID, NoPoints) %>% 
      rename(RawPoints = NoPoints)
    
    # create summary of filtered data
    df_summary_inds <- df_tosummarise %>% 
      group_by(ID) %>%
      summarise(NoPoints = NROW(ID),
                NoUniqueDates = length(unique(Date)),
                FirstDate = as.character(as.Date(min(Date))),
                LastDate = as.character(as.Date(max(Date)))) %>% 
      full_join(df_RAWsummarise, by = "ID") %>% 
      mutate(NoPoints = ifelse(is.na(NoPoints)==T, 0, NoPoints),
             NoUniqueDates = ifelse(is.na(NoUniqueDates)==T, 0, NoUniqueDates)) %>% 
      mutate(FixesRetained = paste(round((NoPoints/RawPoints*100),digits=2), "%")) %>% 
      select(-RawPoints)
    
    ## create datatable for output into the UI
    datatable(df_summary_inds, options = list(
      scrollX = TRUE,
      scrollY = "240px",
      paging = FALSE
    ))
    
  })
  
  
  ## Make the table of the raw data
  output$RAWTable = renderDT({
    
    if (is.null(data())) return()
    
    df_RAWsummarise <- dataRawSum()
    
    datatable(df_RAWsummarise, options = list(
      scrollX = TRUE,
      scrollY = "240px",
      paging = FALSE
    )) 
    
  })
  
  
  ## Add text to the filtered table to tell the user the current filter values they have selected
  tabletextfilter <- eventReactive(input$FilterButton, {
    paste0("Speed filter = ", input$SpeedFilter, "m/s | ", 
           "Net squared displacement filter = ", input$NetDistFilter, "km | ", 
           "Date range = ", input$DateRange[1], " to ", input$DateRange[2])
    
  })
  
  ## render the text to the table
  output$FilterSelection = renderText({ tabletextfilter() })
  
  
  #-------------------------------------------#
  #### 2.5 plot the tracking data in a map ####
  #-------------------------------------------#
  
  output$trackmap = renderPlot({
    
    ## if there is not trakcing data then show no plots
    if (is.null(tracks$data)) return()
    
    ## Show notification while the data is sub-sampled
    FilterMapNotif <- showNotification(shiny::span(shiny:::icon("hourglass-start"), "Rendering Map.."),
                                        type = "message", duration = NULL)

    ## add tracking data
    plottracks <- tracks$data
      
    ## add extra columns to data set for plotting
    ## This might be slowing the app down
    df_plotting <- plottracks %>%
                   group_by(ID) %>%
                   mutate(diffsecs = as.numeric(difftime),
                          secs_elapsed = cumsum(replace_na(diffsecs, 0)),
                          time_elapsed = as.duration(secs_elapsed),
                          days_elapsed = as.numeric(time_elapsed, "days")) %>%
                   mutate(across(c(dist,speed,CPdist, Lat, Lon), as.numeric))
                    
      
    ## Read in the basemap that I already made, this map is made just once when a data set is read in
    Basemap <- Basemap()
      
    ## Now filter the data for an individual if it is selected suing the radio buttons in the box
    if(input$PopSelect == "Individual"){df_plotting <- filter(df_plotting, ID == input$BirdSelect)}
      
    ## make ggplot
    ## might want to pre-render all of the tracks with just data(), should speed up the app
    map <- Basemap + 
        ##add GPS points and paths between then
        geom_point(data = df_plotting, aes(x = Lon, y = Lat, col = speed), alpha = 0.8, size = 0.5 ) +
        geom_path(data = df_plotting, aes(x = Lon, y = Lat, col = speed, group = ID), alpha = 0.8, size = 0.5 ) +
        ##colour birds using scale_colour_gradient2
        scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = (max(df_plotting$speed, na.rm = T)/2) ) +
        ##add central place point
        geom_point(data = df_plotting, aes(x = CPLon, y = CPLat),
                   colour = "#FF3300", fill ="#FF3300", shape = 23, size = 2) +
        ##facet for individual, removed for now under the current way the plots are selected
        #facet_wrap(~ ID, ncol = n_distinct(df_plotting$ID)) +
        ##add labels
        labs(x = "Longitude", y = "Latitude", col = "Speed", Fill = "Depth (m)") +
        theme(axis.text=element_text(colour="black"),
              ##Hide panel borders and remove grid lines
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.title.x = element_text(size = 15),
              axis.text.x = element_text(hjust=0.7),
              axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
              axis.text.y = element_text(hjust=0.7,angle=90,vjust=0.3))

    ## remove the notification
    removeNotification(FilterMapNotif)
    
    ## retrun the map
    map
    
    
      
  })
  
  
  
  
  #---------------------------------------#
  #### 2.6 plot histogram of all speed ####
  #---------------------------------------#
  
  ## plot histogram of speeds for all individuals
  ## updates every time th data is filtered
  output$speedhist = renderPlot({
    
    if (is.null(tracks$data)) return()
    
    plottracks <- tracks$data
    
    df_plotting <- plottracks %>%
                   group_by(ID) %>%
                   mutate(across(c(dist,speed,CPdist, Lat, Lon), as.numeric))
    
    speed_hist <- df_plotting %>% #speed histogram
                  ggplot(data=., aes(speed)) +
                  geom_histogram(binwidth=0.1, alpha=0.7) +
                  geom_density(aes(y =0.1*..count..)) +
                  xlab("speed (m/s)") + ylab("count") +
                  theme(axis.text=element_text(colour="black"),
                        # Hide panel borders and remove grid lines
                        panel.border = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank())
    
    speed_hist
    
    
  })
  
  
  
  
  
  #-------------------------------------------------#
  #### 2.7 plot histogram of net sq displacement ####
  #-------------------------------------------------#
  
  ## plot histogram of speeds for all individuals
  ## updates every time the data is filtered
  output$nethist = renderPlot({
    
    if (is.null(tracks$data)) return()
    
    plottracks <- tracks$data
    
    df_plotting <- plottracks %>%
                   group_by(ID) %>%
                   mutate(across(c(dist,speed,netdisp, Lat, Lon), as.numeric))
                
    netdisp_hist <- df_plotting %>% #speed histogram
                    ggplot(data=., aes((netdisp/1000))) +
                    geom_histogram(binwidth=1, alpha=0.7) +
                    geom_density(aes(y =0.1*..count..)) +
                    xlab("Net squared displacement (km)") + ylab("count") +
                    theme(axis.text=element_text(colour="black"),
                          # Hide panel borders and remove grid lines
                          panel.border = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank())
    
    netdisp_hist
    
  })
  
  
  
  
  #----------------------------#
  #### 3. Foraging Trip Tab ####
  #----------------------------#
  
  
  #-----------------------#
  #### 3.0 Help Button ####
  #-----------------------#
  
  ## call the help button module, when the specified help button the code in the module is run
  ## The includeMarkdown() function basically renders the markdown at the filepath I've specified and included it in the app
  callModule(click_help, "ForageHelp", title = "Help page",
             size = "l", file = "help/p2.0 Foraging Trip Tab.md")
  
  
  #-------------------------------------------------------#
  #### 3.1 Set up reactive values & change variable UI ####
  #-------------------------------------------------------#
  
  ## create an empty reactive value that we will later put the tracking data labelled with trips
  trips <- reactiveValues(data = NULL)
  
  ## create a second reactive value, can put the trip stats from the previous calculation into here
  trips2 <- reactiveValues(data = NULL)
  
  ## create a reactive value for the bounding box, this helps with rending the bounding box map when it is needed 
  Box <- reactiveValues(data = NULL)
  
  ## This updates the UI whenever data() changes so whenever a new file is read in
  observe({
    
    x <- data()
    
    # I need to identify which select input i want to update and then tell it what the new choices should be
    updateSelectInput(session, "BirdSelect2", choices = unique(x$ID))
    
  })
  
  
  #--------------------------------#
  #### 3.2 Calculate trip stats ####
  #--------------------------------#
  
  ## I am only going to calcualte trip stats once they press the action button
  ## If they haven't pressed the action button nothing will display in the foraging trip tag
  ## I will set the default values to something sensible so they could just press the button without
  ## Changing the slider and still get something sensible
  
  ## If the action button is pressed again then the last set of trip stats is put into trips2$data
  ## This data set can then be used below to make a table...etc 
  observeEvent(input$TripStatButton,{
    
    ## on the first press of the buttons is.null(trips$data) will equal TRUE
    if(is.null(trips$data)==FALSE){trips2$data <- trips$data}
    
  })
  
  
  ## Calculate trip stats if action button is pressed
  observeEvent(input$TripStatButton, {
    
    ## Show notification while the data is sub-sampled
    ForagCalcNotif <- showNotification(shiny::span(shiny:::icon("hourglass-start"), "Calculating Trips.."),
                                       type = "message", duration = NULL)
      
      ## define inputs for trip stats
      threshold_time <- input$MinTripTime #time in minutes
      
      ## bring in the tracking data that was read in by the user
      CP_data <- data()
      
      ## Define whether birds are within the central placed buffer or within the colony bounding box if this has been input
      ## Run this code if want to use the colony trip buffer
      if(input$UserBox == "Colony trip buffer"){
        
        ## this removes the bounding box map if it is not needed
        Box$data <- NULL
        
        ## define the user selected  colony buffer
        threshold_dist <- (input$ColTripBuffer)*1000 #distance in meters
        
        ## Define if birds are within the central place buffer
        df_CP <- CP_data %>%
          mutate(atCP = ifelse(CPdist < threshold_dist, "Yes", "No")) # using distance threshold
        
      }
      
      ## Run this code if want to use the user defined bounding box
      if(input$UserBox == "Use user define box"){
        
        ## assign the area of the box to a reactive value, this will load the dynamic UI with the leaflet map in
        Box$data <- c(xmin = input$MinLon, xmax = input$MaxLon, ymin = input$MinLat, ymax = input$MaxLat)
        
        ## create the bounding box based off of the user input and transform to an equal area projection
        BB_box <- st_sf(CP = "CP", geometry = st_as_sfc(st_bbox(Box$data, crs = 4326))) %>% 
                  st_transform(crs = 3857)

        ## now make the tracking data locations an sf geometry and join on the bounding box to determine if fixes are inside the bounding box
        df_CP <- CP_data %>% 
          st_as_sf(., coords=c("Lon","Lat"), crs = 4326, remove=FALSE) %>% 
          st_transform(crs = 3857) %>%
          st_join(., BB_box, join = st_intersects) %>%
          mutate(atCP = ifelse(is.na(CP) == FALSE, "Yes", "No")) %>% # atCP = Yes when points intersect with the polygon
          as.data.frame() %>% 
          select(-CP, -geometry) 
          
      }
      
      ## Classify and number the trips now
      df_trip <- df_CP %>%
        group_by(ID) %>%
        mutate(same_lag = ifelse(atCP == lag(atCP), "TRUE", "FALSE"), # is the point still at or away from CP?
               same_lead = ifelse(atCP == lead(atCP), "TRUE", "FALSE"), # is the next point leaving/returning to the CP?
               label = ifelse((atCP == "Yes" & same_lead == "FALSE"), "first", # label last point at the CP as the first point of the 'trip'
                              ifelse((atCP == "Yes" & same_lag == "FALSE"), "last", NA)), # and the first point back at the CP as the last point of the 'trip'
               trip_bydist = ifelse((atCP == "No" | !is.na(label)), "trip", NA)) %>% # label last point at CP -> first point back as 'trip'
        filter(!is.na(trip_bydist)) %>% # remove non-trip points, as defined by distance
        mutate(trip_bydist_num = ifelse(label == "first", cur_group_rows(), NA)) %>% # assign group number based on row number for first points of trip only
        fill(trip_bydist_num) %>% # fill NAs with group number
        group_by(trip_bydist_num) %>%
        mutate(trip_dur = difftime(max(DateTime), min(DateTime), units="mins")) %>% # calculate trip duration
        filter(trip_dur >= threshold_time) %>% # only keep trips longer than threshold duration
        ungroup() %>%
        group_split(ID) %>%
        purrr::map_df(~.x %>% group_by(trip_bydist_num) %>% mutate(trip_num = cur_group_id())) %>% # assign sequential trip number
        ungroup() %>% select(-c(same_lag, same_lead, label, trip_bydist, trip_bydist_num, trip_dur)) %>% # remove intermediate columns
        mutate(TripID = paste0(ID, "_", trip_num)) #%>% # assign unique trip ID based on ID and trip number
        #st_drop_geometry() # remove the geometry column
      
      ## assign the classified trips to a reactive value
      trips$data <- df_trip
      
      ##remove the rendering notification for this section
      removeNotification(ForagCalcNotif)

  })
  
  
  #----------------------------------------#
  #### 3.3 Foraging trip summary tables ####
  #----------------------------------------#
  
  ## Render the table of the previous trip stats first, if the action button has only been pressed once
  ## then trips2$data will be empty and this table will not render
  output$TripTablePrev = renderDT({
    
    if (is.null(trips2$data)) return()
    
    ## create se fucntion 
    se <- function(x) sqrt(var(x, na.rm = T) / length(x[!is.na(x)]))
    
    ## If this is the second press of the button then trip2$data will have the trip metrics from the previous button press
    ## if it is null then the if statement a few lines above will return nothing for this section of code
    df_trips <- trips2$data
    
    ## summarise the each trip to make the tables
    df_tripmetrics <- df_trips %>%
      group_by(Species, Population, ID, TripID) %>% 
      summarise(Tripstart = min(DateTime),
                Tripend = max(DateTime),
                Completetrip = ifelse(atCP[which.max(DateTime)] == "Yes", "Complete", "Incomplete"), # Complete if  last point is back at the CP
                Trip_duration = difftime(max(DateTime), min(DateTime), units="mins"),
                Total_distance = sum(dist, na.rm = T),
                Max_distance = max(CPdist, na.rm = T),
                Distal_lon = Lon[which.max(CPdist)],
                Distal_lat = Lat[which.max(CPdist)],
                CPLon = CPLon[1],
                CPLat = CPLat[1])

    ## summarise trips by each individual
    df_tripmetric_summary <- df_tripmetrics %>%
      group_by(Species, Population, ID) %>%
      summarise(No_Trips = n(),
                Trip_duration_mins = paste0(round(mean(as.numeric(Trip_duration, units = "mins"), na.rm = T), digits = 1), " ± ",
                                            round(se(as.numeric(Trip_duration, units = "mins")), digits = 1)),
                Total_distance_km = paste0(round((mean(Total_distance, na.rm = T)/1000), digits = 1), " ± ",
                                           round((se(Total_distance)/1000), digits = 1)),
                Max_distance_km = paste0(round((mean(Max_distance, na.rm = T)/1000), digits = 1), " ± ",
                                         round((se(Max_distance)/1000), digits = 1))) %>% 
      rename(`Trip Duration (mins)` = Trip_duration_mins, `Total Dist (km)` = Total_distance_km,
             `Max Dist (km)` = Max_distance_km)
  
    ## create the table of the trip summary
    datatable(df_tripmetric_summary, options = list(
      scrollX = TRUE,
      scrollY = "240px",
      paging = FALSE
    ))
    
    
  })
  
  
  ## Make the table the summaries the current trip stats, from the most recent press of the action button
  output$TripTableFilt = renderDT({
    
    if (is.null(trips$data)) return()
    
    ## add the data
    df_trips <- trips$data
    
    ## create se fucntion 
    se <- function(x) sqrt(var(x, na.rm = T) / length(x[!is.na(x)]))
    
    ## summarise the each trip to make the tables
    df_tripmetrics <- df_trips %>%
      group_by(Species, Population, ID, TripID) %>% 
      summarise(Tripstart = min(DateTime),
                Tripend = max(DateTime),
                Completetrip = ifelse(atCP[which.max(DateTime)] == "Yes", "Complete", "Incomplete"), # Complete if  last point is back at the CP
                Trip_duration = difftime(max(DateTime), min(DateTime), units="mins"),
                Total_distance = sum(dist, na.rm = T),
                Max_distance = max(CPdist, na.rm = T),
                Distal_lon = Lon[which.max(CPdist)],
                Distal_lat = Lat[which.max(CPdist)],
                CPLon = CPLon[1],
                CPLat = CPLat[1])
    
    ## summarise trips by each individual
    df_tripmetric_summary <- df_tripmetrics %>%
      group_by(Species, Population, ID) %>%
      summarise(No_Trips = n(),
                Trip_duration_mins = paste0(round(mean(as.numeric(Trip_duration, units = "mins"), na.rm = T), digits = 1), " ± ",
                                            round(se(as.numeric(Trip_duration, units = "mins")), digits = 1)),
                Total_distance_km = paste0(round((mean(Total_distance, na.rm = T)/1000), digits = 1), " ± ",
                                           round((se(Total_distance)/1000), digits = 1)),
                Max_distance_km = paste0(round((mean(Max_distance, na.rm = T)/1000), digits = 1), " ± ",
                                         round((se(Max_distance)/1000), digits = 1))) %>% 
      rename(`Trip Duration (mins)` = Trip_duration_mins, `Total Dist (km)` = Total_distance_km,
             `Max Dist (km)` = Max_distance_km)
    
    ## create the table of the trip summary
    datatable(df_tripmetric_summary, options = list(
      scrollX = TRUE,
      scrollY = "240px",
      paging = FALSE
    ))
    
    
  })
  
  
  
  #------------------------------------#
  #### 3.4 Plot the trips over time ####
  #------------------------------------#
  
  output$TripClassPlot = renderPlot({
    
    if (is.null(trips$data)) return()
    
    ## add the data
    df_trips <- trips$data
    
    ## Now filter the data for an individual id it is selected in the box
    if(input$PopSelect2 == "Individual"){df_trips <- filter(df_trips, ID == input$BirdSelect2)}
    
    ## create individual-level plots
    if(input$PopSelect2 == "Individual"){
      
      ## make the plot
      trip_plot <- ggplot(df_trips, aes(x = DateTime, y = CPdist, col = as.factor(trip_num)))+
        geom_point(size = 1)+
        #facet_wrap(ID~., scales = "free", ncol = 2)+
        theme_light() +
        labs(col="Trip number")
      
    }
    
    ## create population-level plot
    if(input$PopSelect2 == "Population"){
      
      ## create a column with time since deployment
      df_trips <- df_trips %>% 
                  group_by(ID) %>% 
                  mutate(difftime = ifelse(is.na(difftime)==T, 0, difftime),
                         CumulTime = cumsum(difftime))
      
      ## make the plot
      trip_plot <- ggplot(df_trips, aes(x = CumulTime, y = CPdist, group = ID, col = as.factor(trip_num)))+
        geom_point(size = 1)+
        #facet_wrap(ID~., scales = "free", ncol = 2)+
        theme_light() +
        labs(col="Trip number") +
        xlab("Time since deployment (mins)")
      
    }
    
    trip_plot
    
  })
  
  
  #---------------------------------#
  #### 3.5 Map of foraging trips ####
  #---------------------------------#
  
  output$tripmap = renderPlot({
    
    ## if foraging trips have not been classified then do not return any map
    if (is.null(trips$data)) return()
    
    ## add the data
    df_trips <- trips$data
    
    ## Show notification while the data is sub-sampled
    ForageMapNotif <- showNotification(shiny::span(shiny:::icon("hourglass-start"), "Rendering Map.."),
                                       type = "message", duration = NULL)
    
    ## add extra columns to data set for plotting
    ## This might be slowing the app down
    df_plotForage <- df_trips %>%
      group_by(ID) %>%
      mutate(diffsecs = as.numeric(difftime),
             secs_elapsed = cumsum(replace_na(diffsecs, 0)),
             time_elapsed = as.duration(secs_elapsed),
             days_elapsed = as.numeric(time_elapsed, "days")) %>%
      mutate(across(c(dist,speed,CPdist, Lat, Lon), as.numeric))
    
    
    ## Read in the basemap that I already made, this map is made just once when a data set is read in
    Basemap <- Basemap()
    
    ## Now filter the data for an individual if it is selected suing the radio buttons in the box
    if(input$PopSelect2 == "Individual"){df_plotForage <- filter(df_plotForage, ID == input$BirdSelect2)}
    
    ## make ggplot
    ## might want to pre-render all of the tracks with just data(), should speed up the app
    Foragemap <- Basemap + 
      ##add GPS points and paths between then
      geom_point(data = df_plotForage, aes(x = Lon, y = Lat, col = TripID), alpha = 0.8, size = 0.5 ) +
      geom_path(data = df_plotForage, aes(x = Lon, y = Lat, col = TripID, group = ID), alpha = 0.8, size = 0.5 ) +
      ##add central place point
      geom_point(data = df_plotForage, aes(x = CPLon, y = CPLat),
                 colour = "#FF3300", fill ="#FF3300", shape = 23, size = 2) +
      ##add labels
      labs(x = "Longitude", y = "Latitude", col = "Trip ID", Fill = "Depth (m)") +
      theme(axis.text=element_text(colour="black"),
            ##Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(hjust=0.7),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
            axis.text.y = element_text(hjust=0.7,angle=90,vjust=0.3))
    
    ## remove the notification
    removeNotification(ForageMapNotif)
    
    ## retrun the map
    Foragemap
    
    
    
  })
  
  
  #------------------------------------------#
  #### 3.6 Create trip duration histogram ####
  #------------------------------------------#
  
  output$TripHist = renderPlot({
    
    if (is.null(trips$data)) return()
    
    ## add the data
    df_trips <- trips$data
    
    ## calculate trip duration by trip
    df_alltripdurations <- df_trips %>%
      #st_drop_geometry() %>%
      group_by(Species, ID, TripID) %>%
      summarise(Trip_duration = difftime(max(DateTime), min(DateTime), units="mins"))
    
    ## plot trip durations
    triphist <- ggplot(df_alltripdurations, aes(x = as.numeric(Trip_duration)))+
      geom_histogram(binwidth = 5)+ # each bar = 5 minutes
      scale_x_continuous(breaks = seq(0, as.numeric(max(df_alltripdurations$Trip_duration)), 60))+ # major axis tick = 1 hour
      theme_light()+
      labs(x = "Trip duration, minutes")
    
    triphist
    
  })
  
  
  #--------------------------------------#
  #### 3.7 Render map of bounding box ####
  #--------------------------------------#
  
  ## render the UI for the map of the bounding box
  ## because of the lazy loading in shiny i think this is only executed when we have a value for Box$data
  ## Box$data only has a value once we want to calcualte trips with the bounding box and have pressed the action button
  output$Bounding_box_map <- renderUI({
    
    ## need to call this so that it only loads if this reactive value has been assigned some value
    req(Box$data)
    
    box(width = 12, title = "Map of bounding box", solidHeader = TRUE, status = "primary",
        fluidRow(column(width = 10, offset =1, leafletOutput("Bounding_box_leaflet"))))
    
  })
  
  
  ## render the leaflet map with the bounding box
  output$Bounding_box_leaflet <- renderLeaflet({
    
    ## need to call this so that it only loads if this reactive value has been assigned some value
    req(Box$data) 
    
    ## create the bounding box based off of the user input and tranfrom to an equal area projection
    BB_box <- st_sf(CP = "CP", geometry = st_as_sfc(st_bbox(Box$data, crs = 4326))) 
    
    ## create a leaflet map with the bounding box
    leaf <- BB_box %>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addPolygons(color = "red")
    
    ## Now plot which ever of the two plots was created above
    leaf
    
    
  })
  
  
  #-----------------------------------------------#
  #### 3.8 Text with current input values used ####
  #-----------------------------------------------#
  
  
  # ## Add text to the filtered table to tell the user the current filter values they have selected
  # tabletextfilter <- eventReactive(input$FilterButton, {
  #   paste0("Speed filter = ", input$SpeedFilter, "m/s | ", 
  #          "Net squared displacement filter = ", input$NetDistFilter, "km | ", 
  #          "Date range = ", input$DateRange[1], " to ", input$DateRange[2])
  #   
  # })
  # 
  # ## render the text to the table
  # output$FilterSelection = renderText({ tabletextfilter() })
  
  
  
  
  #---------------------------#
  #### 4. Sub-sampling Tab ####
  #---------------------------#
  
  #-----------------------#
  #### 4.0 Help Button ####
  #-----------------------#
  
  ## call the help button module, when the specified help button the code in the module is run
  ## The includeMarkdown() function basically renders the markdown at the filepath I've specified and included it in the app
  callModule(click_help, "SubSampHelp", title = "Help page",
             size = "l", file = "help/p3.0 Sub-sampling Tab.md")
  
  
  #----------------------------------#
  #### 4.1 Set up reactive values ####
  #----------------------------------#
  
  ## create an empty reactive value that we will later put the sub-sampled tracking data in
  SubSamp <- reactiveValues(data = NULL)
  
  
  #---------------------------#
  #### 4.2 Sub-sample data ####
  #---------------------------#
  
  observeEvent(input$SubSampButton, {
    
    ## NOTE: difftime in the data is in seconds and the slider is in minutes
    ## call in the tracking data
    df_diagnostic <- data()
    
    ## Show notification while the data is sub-sampled
    SubSampSin <- showNotification(
                    shiny::span(shiny:::icon("hourglass-start"), "Sub-sampling data.."),
                    type = "message", duration = NULL)

      ## sub sample the data
      df_subsampled <- df_diagnostic %>%
        group_by(ID) %>%
        mutate(subsample = round_date(DateTime, unit = period(num = input$SubSampInterval, units = "minutes")), #round to nearest value of subsampling unit
               accuracy = as.period(interval(DateTime, subsample)/ seconds(1)),.after=DateTime, #accuracy of round (Date-subsample) in secs
               accuracy = abs(accuracy)) %>% #convert to absolute values (no negatives)
        group_by(ID, subsample) %>% #group by ID and subsample for removing duplicates
        slice_min(accuracy) %>%  #slice out subsample duplicates with highest accuracy
        select(-c(accuracy,subsample)) %>%  #remove excess columns 
        ungroup() %>% group_by(ID) %>% 
        mutate(difftime = difftime(DateTime, lag(DateTime), units="secs")) #recalulate difftime columns
      
    ## remove the notification
    removeNotification(SubSampSin)
    
    ## assign the sub-sampled data to a reactive value
    SubSamp$data <- df_subsampled
    
  })
  
  
  #------------------------------------------------------#
  #### 4.3 Create Pop smapling interval summary boxes ####
  #------------------------------------------------------#
  
  ## Create an Info box with the ID of the individual
  output$MeanSampInt <- renderInfoBox({
    
    MeanInt <- round(mean(data()$difftime, na.rm=T), digits = 1)
    
    infoBox("Mean", paste0(MeanInt, " secs"), color = "purple", icon = shiny::icon("piggy-bank"))
    
  })
  
  ## Create an Info box with the the number of GPS locations
  output$SDSampInt <- renderInfoBox({
    
    sdInt <- round(sd(data()$difftime, na.rm=T), digits = 1)
    
    infoBox("Standard Deviation", paste0(sdInt, " secs"), color = "blue", icon = shiny::icon("globe"))
    
  })
  
  ## Create an Info box with the date rnage of the GP\S data for that individual
  output$RangeSampInt <- renderInfoBox({
    
    minInt <- round(min(data()$difftime, na.rm=T) ,digits = 1)
    maxInt <- round(max(data()$difftime, na.rm=T) ,digits = 1)
    
    infoBox("Range", paste0(minInt, " secs - ", maxInt, " secs"), color = "teal", icon = shiny::icon("calendar"))
    
  })
  
  
  
  #----------------------------------------------------------#
  #### 4.4 Create sampling interval tables per individual ####
  #----------------------------------------------------------#
  
  ## create a reactive value here, means I only have to create the original summary once for the full trakcing data set
  RawSumSubSamp <- reactive({
    
    ## insert the tracking data the user has read in
    df_RAW <- data()
    
    
    dfRAW_summary <- df_RAW %>% 
                    group_by(Species, Population, ID) %>%
                    summarise(NoFixesRAW = n(),
                              Sampling_IntervalMEAN = round(mean(difftime, na.rm=T), digits = 1),
                              Sampling_IntervalSD = round(sd(difftime, na.rm=T), digits = 1),
                              Sampling_Interval_RAW = paste0(Sampling_IntervalMEAN, " ± ", Sampling_IntervalSD)) %>% 
                    ungroup() %>% 
                    select(-c(Sampling_IntervalMEAN, Sampling_IntervalSD))
    
    dfRAW_summary
    
  })
  
  
  
  ## create summary table of the sampling intervals for the tracking data the user has read in and the sub-sampled data
  output$RAWSubSampTable = renderDT({
    
    if (is.null(SubSamp$data)) return()
    
    ## insert the sub-sampled data
    df_subsamp <- SubSamp$data
    
    ## Summarise the sub-sampled data
    df_subsamp_summary <- df_subsamp %>% 
                group_by(Species, Population, ID) %>%
                summarise(NoFixesSub = n(),
                          Sampling_IntervalMEAN = round(mean(difftime, na.rm=T), digits = 1),
                          Sampling_IntervalSD = round(sd(difftime, na.rm=T), digits = 1),
                          Sampling_IntervalSub = paste0(Sampling_IntervalMEAN, " ± ", Sampling_IntervalSD)) %>% 
                ungroup() %>% 
                select(-c(Sampling_IntervalMEAN, Sampling_IntervalSD))
    
    ## join together the RAW and sub-sampled data
    SubSampTable <- full_join(df_subsamp_summary, RawSumSubSamp(), by = c("Species", "Population", "ID"))
    
    ## Add column for the difference in the number of fixes and order dataset
    SubSampTable <- SubSampTable %>% 
                    mutate(FixesRetained = paste(round((NoFixesSub/NoFixesRAW*100),digits=2), "%")) %>% 
                    select(Species, Population, ID, NoFixesRAW, NoFixesSub, FixesRetained, Sampling_Interval_RAW, Sampling_IntervalSub)
    
    ## create the table of the data
    ## create the table of the trip summary
    datatable(SubSampTable, options = list(
      scrollX = TRUE,
      scrollY = "240px",
      paging = FALSE
    ))
    
  })
  
  
  ## Add text to the filtered table to tell the user the current filter values they have selected
  tabletextSamp <- eventReactive(input$SubSampButton, {
    paste0("Interval Chosen = ", input$SubSampInterval, " mins")
    
  })
  
  ## render the text to the table
  output$SubSampSelection = renderText({ tabletextSamp() })
  
    
  #--------------------------------------------------#
  #### 4.5 Create histogram of sampling intervals ####
  #--------------------------------------------------#
  
  output$SubSampHist = renderPlot({
    
    if (is.null(SubSamp$data)) return({## add the data
      df_sub <- data()
      
      ## maximum sampling rate
      maxint <- as.numeric(max(as.numeric(df_sub$difftime), na.rm = T))
      
      ## plot sampling intervals
      SubSamphist <- ggplot(df_sub, aes(x = as.numeric(difftime, na.rm=T)))+
        geom_histogram(bins = 50)+ # each bar = 5 minutes
        scale_x_continuous(n.breaks = 20)+ # major axis tick = 1 hour
        theme_light()+
        labs(x = "Trip duration (seconds)")
      
      SubSamphist})
    
    ## add the data
    df_sub <- SubSamp$data
    
    ## maximum sampling rate
    maxint <- as.numeric(max(as.numeric(df_sub$difftime), na.rm = T))
    
    ## plot sampling intervals
    SubSamphist <- ggplot(df_sub, aes(x = as.numeric(difftime, na.rm=T)))+
      geom_histogram(bins = 50)+ # each bar = 5 minutes
      scale_x_continuous(n.breaks = 20)+ # major axis tick = 1 hour
      theme_light()+
      labs(x = "Trip duration (seconds)")
    
    SubSamphist
    
  })
  
  
  
  
} # this bracket closes the whole server function




#-------------------------------#
#### 5. Create the shiny app ####
#-------------------------------#

## All done, make the app...
shinyApp(ui, server)

