## Shiny app for exploring how data filters/cleaning/re sampling influence the data and derived stats
## Created: 28/07/2022


## packages required
## ggnewscale is used quite a lot but would be a good dependency to lose. Will have a look how to remove later on
pacman::p_load(tidyverse, lubridate, sf, shiny, shinydashboard, rnaturalearth, DT, shinyBS, leaflet, ggnewscale)

## Bring in additional functions from utils.R file
source("utils.R")

# read in data to check that code works without running the app
# data = read_csv("DataOutputs/WorkingDataFrames/RFB_diagnostic.csv")
# tracks <- data



#------------------------------------#
#### 0. Set up the User Interface ####
#------------------------------------#


#-------------------------#
#### Set Styles for UI ####
#-------------------------#

## This just allows my to access cutom CSS styles for certain things
## Might be worth putting then in a styles.css file if there are more 
STYLES <- list(
  help_button = "background-color:#E76F62; width:100%;",
  Selection_text = "font-weight: bold; font-size: 15px",
  Render_button = "background-color:#4ED671; width:100%"
)


#-------------------------------#
#### Set up functions for UI ####
#-------------------------------#

## help module ui
## This function creates the help buttons, uses it a lot so made it into a function
## If this becomes long then it might be worth putting them in an R file that we then call in using source()
help_button <- function(module_id, style = STYLES$help_button, text = "Help") {
  prefix <- NS(module_id)
  actionButton(prefix("help"),
               text,
               icon = shiny::icon("question", verify_fa = FALSE),
               style = style)
}


#----------------------------#
#### 0.1 Set up UI header ####
#----------------------------#

## Set up the header
header <- dashboardHeader(title = "MoveExplore App")




#-----------------------------#
#### 0.2 Set up UI sidebar ####
#-----------------------------#

## Set up the side bar contents
sidebar <- dashboardSidebar(fileInput(inputId = "filedata", label = "Upload data (.csv)", accept = c(".csv")),
                            sidebarMenu(menuItem("Introduction", tabName = "Intro", icon = icon("book")),
                                        menuItem("Data Filters", tabName = "Filterdata", icon = icon("database")),
                                        menuItem("Foraging Trips", tabName = "ForagingTrips", icon = icon("plane-departure")),
                                        menuItem("Sub-sampling", tabName = "SubSamp", icon = icon("ellipsis-h")),
                                        menuItem("Segmenting", tabName = "Segment", icon = icon("stream")),
                                        menuItem("Interactive Map", tabName = "IntMap", icon = icon("globe"))))



#--------------------------#
#### 0.3 Set up UI body ####
#--------------------------#

##-- p0.0 Introduction Tab --##
## Can just include a markdown here instead of using the tags$, might be easier so will switch in future
AppOverview <- box(width = 12, title = "App Overview", solidHeader = TRUE, status = "primary",
                   fluidRow(column(width = 10,includeMarkdown("help/p0.0 Introduction Tab.md"))))


##-- p1.0 Filter Data Tab --##

##-- p1.1 Tracking data overview box --##
TrackingDataOverview <- box(infoBoxOutput("IndividualIDs"),infoBoxOutput("NoLocations"),infoBoxOutput("DateRange"),
                            width = 12, title = "Tracking Data Overview", solidHeader = FALSE, status = "primary")

##-- p1.2 User input box --##
FilterUserInput <- box(width = 12, title = "User Input", solidHeader = TRUE, status = "warning",
                       fluidRow(column(width = 2, offset = 10, help_button("FilterHelp"))),
                       fluidRow(column(width = 12, sliderInput("SpeedFilter", "Speed Filter (m/s)", min = 0, max = 50, step =0.1, value = 12))),
                       fluidRow(column(width = 12, sliderInput("NetDistFilter", "Net displacement filter (km)", min = 0, max = 500, step =1, value = 100))),
                       fluidRow(column(width = 12, sliderInput("PostDepFilter", "Post deployment filter (hours)", min = 0, max = 164, step =1, value = 0))),
                       fluidRow(column(width = 6, dateRangeInput("DateRange", "Select Date Range", start = "2020-11-29", end = "2021-10-17"))),
                       fluidRow(column(width = 2, offset = 10, actionButton("FilterButton", "Apply data filters", style = STYLES$Render_button),
                                              bsTooltip(id = "FilterButton", title = "Adjust user inputs then press this button to filter the data. Plots update automatically"))))

##-- p1.3 Summary table boxes --##
RAWTableF <- box(DTOutput("RAWTable"), width = 5, height = "440px", title = "Summary Table (Full dataset)", 
                 solidHeader = TRUE, status = "primary")

FiltTableF <-box(DTOutput("SumTable"), span(textOutput("FilterSelection"), style = STYLES$Selection_text), width = 7, height = "440px", title = "Summary Table (Filtered)", 
                 solidHeader = TRUE, status = "primary")

##-- p1.4 Summary map --##
FilterMap <- box(plotOutput("trackmap"), width = 9, title = "Map of locations", solidHeader = TRUE, status = "primary")

FilterMapSelectors <- box(radioButtons("PopSelect", "Population or Individual", c("Population", "Individual")),
                          conditionalPanel(condition = "input.PopSelect == 'Individual'",
                                           selectInput("BirdSelect", "Select Animal ID (Individual only)", c("Select file first"))),
                          width = 3, title = "Select Plot Options", solidHeader = TRUE, status = "primary")

##-- p1.5 Summary Histograms --##
FiltTabbedHistograms <- tabBox(title = "Diagnostic Plots", id = "tabset1", width = 12, side = "right",
                               tabPanel("Speed Histogram", plotOutput("speedhist")),
                               tabPanel("Net displacement histogram", plotOutput("nethist")))

##-- p1.6 Custom code output --##
Codechunk_filter <- box(title = "User input code for pasting into workflow (Step 6. Filtering 2)", width = 12, verbatimTextOutput("codechunk_filter"))


##-- p2.0 Foraging Trip Tab --##

##-- p2.1 User Input box --##
ForgaingUserInput <- box(width = 12, title = "User Input", solidHeader = TRUE, status = "warning",
                         fluidRow(column(width = 2, offset = 10, help_button("ForageHelp"))),
                         fluidRow(column(width = 12, radioButtons("UserBox", "How to define colony", choices = c("Colony distance buffer (Trip Start Distance)", "Shape file of colony"), selected = "Colony distance buffer (Trip Start Distance)"))),
                         fluidRow(column(width = 12, conditionalPanel(condition = "input.UserBox == 'Colony distance buffer (Trip Start Distance)'",
                                                                      sliderInput("ColDistBuffer", "Trip Start Distance (km)", min = 0, max = 20, step = 0.2, value = 1)))),
                         fluidRow(column(width = 12, conditionalPanel(condition = "input.UserBox == 'Shape file of colony'",
                                                                      selectInput("ColonySelect", "Select Colony", c("Select file first")),
                                                                      title = "Select Colony(s) for shape file", solidHeader = TRUE, status = "primary"))),
                         fluidRow(column(width = 12, conditionalPanel(condition = "input.UserBox == 'Shape file of colony'",
                                                                      fileInput(inputId = "ColShp", label = "Upload shape file (select all files attributed to the .shp file including e.g., .dbf, .prj, .shx)", accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE)))),
                         fluidRow(column(width = 12, sliderInput("MinTripTime", "Minimum Trip Duration (mins)", min = 0, max = 240, step =1, value = 15))),
                         fluidRow(column(width = 2, offset = 10, actionButton("TripStatButton", "Calculate Trip Statistics", style = STYLES$Render_button),
                                                                 bsTooltip(id = "TripStatButton", title = "Adjust user inputs then press this button to calculate foraging trips. Plots update automatically"))))

##-- p2.2 Summary Table --##
ForagingSummaryTable <- tabBox(title = "Foraging Trip Summary", id = "tabset2", width = 12, side = "right",
                               tabPanel("Current", DTOutput("TripTableFilt"), span(textOutput("ForageSelection"), style = STYLES$Selection_text)),
                               tabPanel("Previous", DTOutput("TripTablePrev"), span(textOutput("ForageSelectionPrev"), style = STYLES$Selection_text)))

##-- p2.3 Trips over time box --##
ForgaingTripsPlot <- box(plotOutput("TripClassPlot"), width = 9, title = "Foraging Trip Classification", 
                         solidHeader = TRUE, status = "primary")

ForgaingTripsPlotSelector <- box(radioButtons("PopSelect2", "Population or Individual", c("Population", "Individual")),
                                 conditionalPanel(condition = "input.PopSelect2 == 'Individual'",
                                                  selectInput("BirdSelect2", "Select Animal ID (Individual only)", c("Select file first"))), 
                                 width = 3, title = "Select Plot Options", solidHeader = TRUE, status = "primary")

##-- p2.4 Foraging trip map --#
ForageMap <- box(plotOutput("tripmap"), width = 12, title = "Foraging Trip Map", solidHeader = TRUE, status = "primary")

##-- p2.5 Trip duration histogram --##
ForagingDurationHist <- box(plotOutput("TripHist"), width = 12, title = "Foraging Trip Duration Histogram", 
                            solidHeader = TRUE, status = "primary")

##-- p2.6 Leaflet map of bounding box --##
Bounding_box_map <- uiOutput("Bounding_box_map")

##-- p2.7 Custom code output --##
Codechunk_trips <- box(title = "User input code for pasting into workflow (Step 2 & 5 - Optional Processing Central place trips)", width = 12, verbatimTextOutput("codechunk_trips"))



##-- p3.0 Sub-sampling Tab --##

##-- p3.1 Sampling Interval Overview --##
SamplingIntervalOverview <- box(width = 12, title = "Sampling Interval Summary - Uploaded data", solidHeader = FALSE, status = "primary",
                                valueBoxOutput("MeanSampInt", width =4), valueBoxOutput("SDSampInt", width =4), valueBoxOutput("RangeSampInt", width =4))

##-- p3.2 User Input --##
SubSampUserInput <- box(width = 12, title = "User Input", solidHeader = TRUE, status = "warning",
                        fluidRow(column(width = 2, offset = 10, help_button("SubSampHelp"))),
                        fluidRow(column(width = 12, sliderInput("SubSampInterval", "Required Sampling Interval (mins)", min = 1, max = 300, step =1, value = 60))),
                        fluidRow(column(width = 2, offset = 10, actionButton("SubSampButton", "Sub-sample data", style = STYLES$Render_button),
                                                                bsTooltip(id = "SubSampButton", title = "Adjust user input then press this button to sub-sample data. Plots update automatically"))))

##-- p3.3 Summary Table --##
SubSampSummaryTable <- box(DTOutput("RAWSubSampTable"), span(textOutput("SubSampSelection"), style = STYLES$Selection_text), width = 12, height = "440px", 
                           title = "Summary of Sub-sampled Data", solidHeader = TRUE, status = "primary")

##-- p3.3 Sampling Interval Histogram --##
SubSampHistogram <- box(plotOutput("SubSampHist"), width = 12, title = "Histogram of Sampling Intervals", solidHeader = TRUE, status = "primary")




##-- p4.0 Segmenting Tab --##

##-- p4.1 User Input box --##
SegmentUserInput <- box(width = 12, title = "User Input", solidHeader = TRUE, status = "warning",
                        fluidRow(column(width = 2, offset = 10, help_button("SegmentHelp"))),
                        fluidRow(column(width = 12, sliderInput("IntervalThreshold", "Threshold Interval to Segment Track (mins)", min = 1, max = 240, step =1, value = 200))),
                        fluidRow(column(width = 12, sliderInput("MinSegThreshold", "Minimum Length of Segment (No. Locations)", min = 1, max = 180, step =1, value = 1))),
                        fluidRow(column(width = 2, offset = 10, actionButton("SegmentButton", "Calculate Segments", style = STYLES$Render_button),
                                                   bsTooltip(id = "SegmentButton", title = "Adjust user input then press this button to segment data. Plots update automatically"))))

##-- p4.2 Segment histograms --##
ThresholdIntervalHist <- box(plotOutput("ThresholdIntervalHist"), width = 6, title = "Interval Histogram", 
                            solidHeader = TRUE, status = "primary")

SegmentLengthHist <- box(plotOutput("SegmentLengthHist"), width = 6, title = "Segment Length Histogram", 
                             solidHeader = TRUE, status = "primary")


##-- p4.3 Summary Table --##
SegmentSummaryTable <- tabBox(title = "Segment Summary", id = "tabset2", width = 12, side = "right",
                               tabPanel("Current", DTOutput("SegmentTableFilt"), span(textOutput("SegmentSelection"), style = STYLES$Selection_text)))

##-- p4.4 Segments over time box --##
SegmentPlot <- box(plotOutput("SegmentClassPlot"), width = 9, title = "Segment Classification", 
                         solidHeader = TRUE, status = "primary")

SegmentPlotSelector <- box(radioButtons("PopSelect3", "Population or Individual", c("Population", "Individual")),
                           conditionalPanel(condition = "input.PopSelect3 == 'Individual'",
                                            selectInput("BirdSelect3", "Select Animal ID (Individual only)", c("Select file first"))), 
                                 width = 3, title = "Select Plot Options", solidHeader = TRUE, status = "primary")

##-- p4.5 Segment map --#
SegmentMap <- box(plotOutput("Segmentmap"), width = 12, title = "Segment Map", solidHeader = TRUE, status = "primary")




##-- p5.0 Interactive map Tab --##

##-- p5.1 Box for map controls --##
InteractiveMapControls_box <- box(width = 12, title = "User Input", solidHeader = TRUE, status = "warning",
                                  fluidRow(column(width = 2, offset = 10, help_button("IntMapHelp"))),
                                  fluidRow(column(width = 2, radioButtons("PopSelectMap", "Population or Individual", c("Population", "Individual"))),
                                           column(width = 2, offset = 1, conditionalPanel(condition = "input.PopSelectMap == 'Individual'", selectInput("BirdSelectMap", "Select Animal ID", c("Select file first"))))),
                                  fluidRow(column(width = 4, selectInput("IntMapColour", "Select Track Colour Variable", c("Speed", "Turning Angle", "Age", "Time of Day")))),
                                  fluidRow(column(width = 2, offset = 10, actionButton("IntMapButton", "Render Map", style = STYLES$Render_button))))

##-- p5.2 Box containing the map itself --##
InteractiveMap_Box <- box(leafletOutput("IntLeafletMap", height = 500), width = 12, height = "600px", title ="Interactive Map", solidHeader = TRUE, status = "primary")




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
                   fluidRow(FiltTabbedHistograms),
                   fluidRow(Codechunk_filter)
                   ),
           tabItem(tabName = "ForagingTrips",
                   fluidRow(ForgaingUserInput),
                   fluidRow(Bounding_box_map),
                   fluidRow(ForagingDurationHist),
                   fluidRow(ForagingSummaryTable),
                   fluidRow(ForgaingTripsPlot, ForgaingTripsPlotSelector),
                   fluidRow(ForageMap),
                   fluidRow(Codechunk_trips)
                   ),
            tabItem(tabName = "SubSamp",
                    fluidRow(SamplingIntervalOverview, SubSampHistogram),
                    fluidRow(SubSampUserInput),
                    fluidRow(SubSampSummaryTable)
                    ),
           tabItem(tabName = "Segment",
                   fluidRow(SegmentUserInput),
                   fluidRow(ThresholdIntervalHist, SegmentLengthHist),
                   fluidRow(SegmentSummaryTable),
                   fluidRow(SegmentPlot, SegmentPlotSelector),
                   fluidRow(SegmentMap)
           ),
           tabItem(tabName = "IntMap",
                    fluidRow(InteractiveMapControls_box),
                    fluidRow(InteractiveMap_Box))))


#-------------------------#
#### 0.4 Create the UI ####
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
    print(input$filedata$datapath)
    temp <- read_csv(input$filedata$datapath)
    
    data2 <- temp %>% 
             group_by(ID) %>% 
             mutate(difftime = as.numeric(difftime((DateTime), lag(DateTime), units= "secs")),
                    SinceStart = as.numeric(difftime(DateTime, DateTime[1], units= "hours"))) 
    
    data2
    
  })
  
  ## When i read in a file also assign the data to a reactive values
  ## This is important, it allows me to call the RAW data and also filter the data separately
  observeEvent(input$filedata, {
    
    ## assign the data to a reactive value, can filter this data set later on
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
      coord_sf(xlim = c(minlon, maxlon), ylim = c(minlat, maxlat), crs = 4326)+
      theme_light()+
      theme(panel.grid.minor = element_blank())
    
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
  ## When it is pressed the tracking data is filtered
  observeEvent(input$FilterButton, {
      
    ## Show notification while the data is sub-sampled
    FilterNotif <- showNotification(shiny::span(shiny:::icon("hourglass-start"), "Filtering data.."),
                                    type = "message", duration = NULL)
    
    ## Assign the user input values to a reactive value, this is then used later to render the code chunk that users can copy and paste into the workflow
    ## If the user input values are not reacitvevalues then the code chunk will change each time the user moves a slider
    tracks$SpeedFilt <- input$SpeedFilter
    tracks$NetDistFilt <- input$NetDistFilter
    tracks$PostDepFilt <- input$PostDepFilter
    
    ## filter data, this happens whenever the FilterButton is pressed
    tracks$data <- filter(data(), speed <= input$SpeedFilter & netdisp <= (input$NetDistFilter*1000) &
                                  SinceStart >= as.numeric(input$PostDepFilter) &
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
  ####  2.5 Make summary table for individuals in the data ####
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
      select(-RawPoints) %>% 
      rename(`No Locations` = NoPoints, `No Unique Dates` = NoUniqueDates, `First Date` = FirstDate, 
             `Last Date` = LastDate, `% Fixes retained` = FixesRetained)
    
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
    
    df_RAWsummarise <- df_RAWsummarise %>% 
      rename(`No Locations` = NoPoints, `No Unique Dates` = NoUniqueDates, `First Date` = FirstDate, `Last Date` = LastDate)
    
    datatable(df_RAWsummarise, options = list(
      scrollX = TRUE,
      scrollY = "240px",
      paging = FALSE
    )) 
    
  })
  
  
  ## Add text to the filtered table to tell the user the current filter values they have selected
  tabletextfilter <- eventReactive(input$FilterButton, {
    paste0("Speed filter = ", input$SpeedFilter, "m/s | ", 
           "Net displacement filter = ", input$NetDistFilter, "km | ", 
           "Post deplyment filter = ", input$PostDepFilter, "hours | ",
           "Date range = ", input$DateRange[1], " to ", input$DateRange[2])
    
  })
  
  ## render the text to the table
  output$FilterSelection = renderText({ tabletextfilter() })
  
  
  
  
  #-------------------------------------------#
  #### 2.6 Plot the tracking data in a map ####
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
    if(input$PopSelect == "Individual"){ 
      map <- Basemap + 
        ##add GPS points and paths between then
        geom_point(data = df_plotting, aes(x = Lon, y = Lat, col = speed), size = 0.5 ) +
        geom_path(data = df_plotting, aes(x = Lon, y = Lat, col = speed, group = ID), alpha = 0.8, size = 0.5 ) +
        ##colour birds using scale_colour_gradient2
        scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = (max(df_plotting$speed, na.rm = T)/2) ) +
        ##add central place point
        geom_point(data = df_plotting, aes(x = CPLon, y = CPLat),
                   colour = "#FF3300", fill ="#FF3300", shape = 23, size = 2) +
        coord_sf(xlim = c(min(df_plotting$Lon), max(df_plotting$Lon)), ylim = c(min(df_plotting$Lat), max(df_plotting$Lat)), crs = 4326) +
        ##add labels
        labs(x = "Longitude", y = "Latitude", col = "Speed")

    ## remove the notification
    removeNotification(FilterMapNotif)
    }
    
    if(input$PopSelect == "Population"){ 
      map <- Basemap + 
        ##add GPS points and paths between then
        geom_point(data = df_plotting, aes(x = Lon, y = Lat, col = ID), size = 0.5 ) +
        geom_path(data = df_plotting, aes(x = Lon, y = Lat, col = ID, group = ID), alpha = 0.8, size = 0.5 ) +
        scale_colour_viridis_d()+
        ##add central place point - this will break if no CP
        # geom_point(data = df_plotting, aes(x = CPLon, y = CPLat),
        #            colour = "#FF3300", fill ="#FF3300", shape = 23, size = 2) +
        ##add labels
        labs(x = "Longitude", y = "Latitude", col = "Individual ID")
      
      ## remove the notification
      removeNotification(FilterMapNotif)
    }
    ## retrun the map
    map
      
  })
  
  
  
  
  #---------------------------------------#
  #### 2.7 Plot histogram of all speed ####
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
                  theme_light() +
                  xlab("Speed (m/s)") + ylab("Frequency") +
                  theme(axis.text=element_text(colour="black", size = 12), axis.title = element_text(size = 16),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    speed_hist
    
  })
  
  
  
  
  #----------------------------------------------#
  #### 2.8 Plot histogram of net displacement ####
  #--------------------------------------------- #
  
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
                    xlab("Net displacement (km)") + ylab("Frequency") +
                    theme_light() +
                    theme(axis.text=element_text(colour="black", size = 12), axis.title = element_text(size = 16),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    netdisp_hist
    
  })
  
  
  
  
  #------------------------------#
  #### 2.9 Custom code output ####
  #------------------------------#

  ## Render code that users can paste into the workflow 
  output$codechunk_filter <- renderText({ 
    
    ## Return this if the user hasn't filtered the data yet
    if (is.null(tracks$SpeedFilt)==TRUE) return(paste0('Filter data first'))
    
    ## Return this once the user has selected input values and filtered the data
    paste0(
            '#--------------------#
            ## USER INPUT START ##
            #--------------------#
                  
            ## Define a period to filter after tag deployment
            ## All points before the cutoff will be removed
            ## For example, to remove potentially unnatural behaviour following the tagging event
            ## This has to be an integer value
            ## change the units within the function (e.g., "min"/"mins"/"hours"/"year"...)
                  
            filter_cutoff <- as.period(',tracks$PostDepFilt,', unit="hours") 
                  
            ## Define speed filter in m/s
            ## Any points with faster speeds will be removed
            filter_speed <- ',tracks$SpeedFilt,'
                  
            ## Define net displacement filter and specify units
            ## Any points further away from the first tracking point will be removed
            filter_netdisp_dist <- ',tracks$NetDistFilt,'
            filter_netdist_units <- "km" # e.g., "m", "km"
                  
                  
            #------------------#
            ## USER INPUT END ##
            #------------------#')
    
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
  
  ## create an 'all trips' value to store all trips not filtered by time
  alltrips <- reactiveValues(data = NULL)
    
  ## create a reactive value for the bounding box, this helps with rendering the bounding box map when it is needed 
  Box <- reactiveValues(data = NULL)
  
  ## This updates the UI whenever data() changes so whenever a new file is read in
  observe({
    
    x <- data()
    
    # I need to identify which select input i want to update and then tell it what the new choices should be
    updateSelectInput(session, "BirdSelect2", choices = unique(x$ID))
    updateSelectInput(session, "ColonySelect", choices = c("All", unique(x$Population)))
  })
  
  
  #---------------------------------------#
  #### 3.2 Read in Shapefile of colony ####
  #---------------------------------------#
  
  ## Now read in the the colony shapefile
  ColonyShapefile <- reactive({
    
    ## require this value, this code is only executed when this has a value or the value changes
    req(input$ColShp)
    
    ## get the filepath specificed by the user
    shpdf <- input$ColShp
    
    # The files are uploaded with names
    # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # (path/names are in column datapath)
    # We need to rename the files with the actual names:
    # fe_2007_39_county.dbf, etc.
    # (these are in column name)
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    
    # Now read in the shapefile 
    # We use the function grep() to search the pattern "*.shp$"
    # within each element of the character vector shpdf$name.
    # grep(pattern="*.shp$", shpdf$name)
    # ($ at the end denote files that finish with .shp, not only that contain .shp)
    ColShape <- st_read(dsn = paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))

    ## assign the shapefile to the 
    Box$data <- ColShape
    
    ## return the colony shapefile
    ColShape
                        
    
  })
  
  
  ## When i read the shapefile also assign the data to a reactive values
  ## This is important, it allows me to call the RAW data and also filter the data separately
  ## NOTE: not sure this step is need might delete later
  observeEvent(input$ColShp, {
    
    ## assign the data to a reactive value, can filter this data set later on
    Box$data <- ColonyShapefile()
    
  })
  
  
  
  
  #--------------------------------#
  #### 3.3 Calculate trip stats ####
  #--------------------------------#
  
  ## I am only going to calcualte trip stats once they press the action button
  ## If they haven't pressed the action button nothing will display in the foraging trip tag
  ## I will set the default values to something sensible so they could just press the button without changing the sliders and get something sensible
  
  observeEvent(input$TripStatButton,{
    
    if(is.null(trips$data)==TRUE){trips$ColBuffer <- input$ColDistBuffer; trips$MinDuration <- input$MinTripTime; trips$buffer <- input$UserBox}else{
      
      trips2$ColBuffer <- trips$ColBuffer; trips2$MinDuration <- trips$MinDuration; trips2$buffer <- trips$buffer
      trips$ColBuffer <- input$ColDistBuffer; trips$MinDuration <- input$MinTripTime; trips$buffer <- input$UserBox
      
    }
    
    
  })
  
  ## If the action button is pressed for a second time then the last set of trip stats is put into trips2$data
  ## This data set can then be used to make the table in the previous tab of the foraging tirp sumary tables.  
  observeEvent(input$TripStatButton,{
    
    ## on the first press of the buttons is.null(trips$data) will equal TRUE
    ## On the second press it will equal FLASE and the assigning will happen
    ## NOTE** This bit doesnt work, need to figure out how to save the previous selection of user input values
    if(is.null(trips$data)==FALSE){trips2$data <- trips$data}
    
    ## Here I want to update the select input for the interactive map
    ## So add an option to colour the map by trip number if I have already calculated the trips. 
    ## If I have not calculated the number of trips then it will not appear as an option in the drop down
    ## First save the old select input option
    if(is.null(seg$data)==TRUE){updateSelectInput(session , "IntMapColour", choices =  c("Speed", "Turning Angle", "Age", "Time of Day", "Foraging Trip ID"))}
    if(is.null(seg$data)==FALSE){updateSelectInput(session , "IntMapColour", choices =  c("Speed", "Turning Angle", "Age", "Time of Day", "Foraging Trip ID", "Segment ID"))}
    
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
      if(input$UserBox == "Colony distance buffer (Trip Start Distance)"){
        
        ## this removes the bounding box map if it is not needed
        Box$data <- NULL
        
        ## define the user selected  colony buffer
        threshold_dist <- (input$ColDistBuffer)*1000 #distance in meters
        
        ## Define if birds are within the central place buffer
        df_CP <- CP_data %>%
          mutate(atCP = ifelse(CPdist < threshold_dist, "Yes", "No")) # using distance threshold
        
      }
      
      # Run this code if want to use the user defined bounding box
      if(input$UserBox == "Shape file of colony"){

        if(!input$ColonySelect == "All"){CP_data <- filter(CP_data, Population == input$ColonySelect)}

        ## transform the crs of the shapefile
        CP_shape <- Box$data %>%
                    st_transform(crs = 3857) %>%
                    mutate(CP = "CP")
        
        
        ## Define points at the CP if they intersect with the shape file
        df_CP <- CP_data %>%
          st_as_sf(., coords=c("Lon","Lat"), crs = 4326, remove=FALSE) %>%
          st_transform(crs = 3857) %>%
          st_join(., CP_shape, join = st_intersects) %>%
          mutate(atCP = ifelse((is.na(CP) == FALSE & Population.x == Population.y), "Yes", "No")) %>% # atCP = Yes when points intersect with the population-specific polygon
          #select(-CP, -Population.y) %>% # remove common column
          rename(Population = Population.x)



      }
      
      ## First classify trips and calculate durations without time filter
      df_alltrips <-  df_CP %>%
        group_by(Population, ID) %>%
        mutate(same_lag = ifelse(atCP == lag(atCP), "TRUE", "FALSE"), # is the point still at or away from CP?
               same_lead = ifelse(atCP == lead(atCP), "TRUE", "FALSE"), # is the next point leaving/returning to the CP?
               label = case_when(atCP == "Yes" & same_lead == "FALSE" ~ "first", # label last point at the CP as the first point of the 'trip'
                                 atCP == "Yes" & same_lag == "FALSE" ~ "last"), # and the first point back at the CP as the last point of the 'trip'
               trip_spatial = case_when(atCP == "No" | !is.na(label) ~ "trip")) %>% # label last point at CP -> first point back as 'trip'
        filter(!is.na(trip_spatial)) %>% # remove non-trip points, as defined by spatial filters
        mutate(trip_spatial_num = case_when(label == "first" ~ cur_group_rows())) %>% # assign group number based on row number for first points of trip only
        fill(trip_spatial_num) %>% # fill NAs with group number
        group_by(trip_spatial_num) %>%
        mutate(trip_dur = difftime(max(DateTime), min(DateTime), units="mins")) %>% # calculate trip duration
        ungroup() %>%
        group_split(ID) %>%
        purrr::map_df(~.x %>% group_by(trip_spatial_num) %>% mutate(trip_num = cur_group_id())) %>% # assign sequential trip number
        ungroup() %>% select(-c(same_lag, same_lead, label, trip_spatial, trip_spatial_num)) %>% # remove intermediate columns
        mutate(TripID = paste0(ID, "_", trip_num)) # assign unique trip ID based on ID and trip number
        
      ## assign the classified trips to a reactive value
      alltrips$data <- df_alltrips
      
      
      ## Classify and number the trips with time threshold
      df_trip <- df_CP %>%
        group_by(Population, ID) %>%
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
        ungroup() %>% select(-c(same_lag, same_lead, label, trip_bydist, trip_bydist_num)) %>% # remove intermediate columns
        mutate(TripID = paste0(ID, "_", trip_num)) 
      
        # Need to drop the geometry if the user read in a shapefile
        if(input$UserBox == "Shape file of colony"){df_trip <- df_trip %>% st_drop_geometry()}
        
    
      ## assign the classified trips to a reactive value
      trips$data <- df_trip
      
      ##remove the rendering notification for this section
      removeNotification(ForagCalcNotif)

  })
  
  
  #----------------------------------------#
  #### 3.4 Foraging trip summary tables ####
  #----------------------------------------#
  
  ## Render the table of the previous trip stats first, if the action button has only been pressed once
  ## then trips2$data will be empty and this table will not render
  output$TripTablePrev = renderDT({
    
    if (is.null(trips2$data)) return()
    
    ## If this is the second press of the button then trip2$data will have the trip metrics from the previous button press
    ## if it is null then the if statement a few lines above will return nothing for this section of code
    df_trips <- trips2$data
    
    ## summaries the each trip to make the tables
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
             `Max Dist (km)` = Max_distance_km, `No Locations` = No_Trips)
  
    ## create the table of the trip summary
    datatable(df_tripmetric_summary, options = list(
      scrollX = TRUE,
      scrollY = "240px",
      paging = FALSE
    ))
    
    
  })
  
  
  ## Make the table the summaries the current trip stats, from the most recent press of the action button
  ## There are columns for the comparison of different data sets
  ## NOTE** Feels like there is redundancy here as i Claudette the previous trip summary again but I have already done that above
  output$TripTableFilt = renderDT({
    
    if (is.null(trips$data)) return()
    
    ## add the data
    df_trips <- trips$data
    
    ## summaries each trip for the current selection of user input values
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
    
    ## if the previous trip selection is not available, i.e. the button has only been pressed once then just render a table
    ## without any of the comparison columns
    if(is.null(trips2$data)==TRUE){
      
      ## summaries trips by each individual, format for DT table straight away
      df_tripmetric_summaryFINAL <- df_tripmetrics %>%
        group_by(Species, Population, ID) %>%
        summarise(No_Trips = n(),
                  Trip_duration_mins = paste0(round(mean(as.numeric(Trip_duration, units = "mins"), na.rm = T), digits = 1), " ± ",
                                              round(se(as.numeric(Trip_duration, units = "mins")), digits = 1)),
                  Total_distance_km = paste0(round((mean(Total_distance, na.rm = T)/1000), digits = 1), " ± ",
                                             round((se(Total_distance)/1000), digits = 1)),
                  Max_distance_km = paste0(round((mean(Max_distance, na.rm = T)/1000), digits = 1), " ± ",
                                           round((se(Max_distance)/1000), digits = 1))) %>% 
         rename(`No Trips` = No_Trips, `Trip Duration (mins)` = Trip_duration_mins, `Total Dist (km)` = Total_distance_km,
                `Max Dist (km)` = Max_distance_km)
      
      
    }
    
    ## if there is a previous trip selection then then need to calcualte the comparison columns
    if(is.null(trips2$data)==FALSE){
      
      ## summarise trips by each individual for current selection but do not rename columns
      df_tripmetric_summary <- df_tripmetrics %>%
        group_by(Species, Population, ID) %>%
        summarise(No_Trips = n(),
                  Trip_duration_mins = paste0(round(mean(as.numeric(Trip_duration, units = "mins"), na.rm = T), digits = 1), " ± ",
                                              round(se(as.numeric(Trip_duration, units = "mins")), digits = 1)),
                  Total_distance_km = paste0(round((mean(Total_distance, na.rm = T)/1000), digits = 1), " ± ",
                                             round((se(Total_distance)/1000), digits = 1)),
                  Max_distance_km = paste0(round((mean(Max_distance, na.rm = T)/1000), digits = 1), " ± ",
                                           round((se(Max_distance)/1000), digits = 1)),
                  Trip_duration_mean = round(mean(as.numeric(Trip_duration, units = "mins"), na.rm = T), digits = 1),
                  Total_distance_mean = round((mean(Total_distance, na.rm = T)/1000), digits = 1))
      
      
      ## summaries the each trip for the previous selection of user input values
      df_tripmetricsprev <- trips2$data %>%
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
      
      ## now format the data for the previous section of user inoput values for the table
      df_tripmetric_summary_prev <- df_tripmetricsprev %>%
        group_by(Species, Population, ID) %>%
        summarise(No_Trips = n(),
                  Trip_duration_mins = paste0(round(mean(as.numeric(Trip_duration, units = "mins"), na.rm = T), digits = 1), " ± ",
                                              round(se(as.numeric(Trip_duration, units = "mins")), digits = 1)),
                  Total_distance_km = paste0(round((mean(Total_distance, na.rm = T)/1000), digits = 1), " ± ",
                                             round((se(Total_distance)/1000), digits = 1)),
                  Max_distance_km = paste0(round((mean(Max_distance, na.rm = T)/1000), digits = 1), " ± ",
                                           round((se(Max_distance)/1000), digits = 1)),
                  Trip_duration_mean = round(mean(as.numeric(Trip_duration, units = "mins"), na.rm = T), digits = 1),
                  Total_distance_mean = round((mean(Total_distance, na.rm = T)/1000), digits = 1)) %>%
        ungroup() %>%
        select(ID, No_Trips, Trip_duration_mean, Total_distance_mean) %>%
        rename(No_Trips_prev = No_Trips, Trip_duration_mean_prev = Trip_duration_mean, Total_distance_mean_prev = Total_distance_mean)

      ## Now join the current and previous data stes together
      df_tripmetric_summary2 <- left_join(df_tripmetric_summary, df_tripmetric_summary_prev, by = "ID") 
      
      ## calcualte the differences between the two data sets
      df_tripmetric_summaryFINAL <- df_tripmetric_summary2 %>% 
        mutate(Change_NoTrips = No_Trips_prev-No_Trips,
               Change_trip_duration = round(Trip_duration_mean_prev-Trip_duration_mean, digits = 1),
               Change_trip_dist = round(Total_distance_mean_prev-Total_distance_mean, digits = 1)) %>% 
        select(-c(Trip_duration_mean, Total_distance_mean, Trip_duration_mean_prev, Total_distance_mean_prev, No_Trips_prev)) %>% 
        rename(`No Trips` = No_Trips, `Trip Duration (mins)` = Trip_duration_mins, `Total Dist (km)` = Total_distance_km,
               `Max Dist (km)` = Max_distance_km, `Change in Trip No` = Change_NoTrips, `Change in Trip Duration (mins)` = Change_trip_duration, 
               `Change in Trip Distance (km)` = Change_trip_dist)
    
    }
      
    ## create the table of the trip summary
    datatable(df_tripmetric_summaryFINAL, options = list(
      scrollX = TRUE,
      scrollY = "240px",
      paging = FALSE
    ))
    
  })
  
  
  #------------------------------------#
  #### 3.5 Plot the trips over time ####
  #------------------------------------#
  
  output$TripClassPlot = renderPlot({
    
    if (is.null(trips$data)) return()
    
    ## add the data
    df_trips <- trips$data
    df_alltrips <- alltrips$data %>% 
      mutate(Fill = ifelse(as.numeric(trip_dur) > trips$MinDuration, "Retained" ,"Discarded"))
      
    
    ## Now filter the data for an individual id it is selected in the box
    if(input$PopSelect2 == "Individual"){
      df_trips <- filter(df_trips, ID == input$BirdSelect2)
      df_alltrips <- filter(df_alltrips, ID == input$BirdSelect2)
      }
    
    ## create individual-level plots
    if(input$PopSelect2 == "Individual"){
      
      ## make the plot
      trip_plot <- ggplot()+
        geom_point(data = filter(df_alltrips, Fill == "Discarded"), aes(x = DateTime, y = CPdist, col = Fill), alpha = 0.8, size = 1) +
        scale_colour_manual(values = c("#C5C5C5"), name = "Trip duration\ntoo short")+
        new_scale_color()+
        geom_point(data = df_trips, aes(x = DateTime, y = CPdist, col = as.factor(TripID)), size = 1)+
        scale_colour_viridis_d(option = "plasma", end = 0.9, name = "Trip ID")+
        #facet_wrap(ID~., scales = "free", ncol = 2)+
        theme_light()
      
    }
    
    ## create population-level plot
    if(input$PopSelect2 == "Population"){
      
      
      ## make the plot
      trip_plot <- ggplot()+
        geom_point(data = filter(df_alltrips, Fill == "Discarded"), aes(x = SinceStart, y = CPdist, col = Fill), alpha = 0.8, size = 1) +
        scale_colour_manual(values = c("#C5C5C5"), name = "Trip duration\ntoo short")+
        new_scale_color()+
        geom_point(data = df_trips, aes(x = SinceStart, y = CPdist, group = ID, col = as.factor(ID)), size = 1)+
        scale_colour_viridis_d()+
        #facet_wrap(ID~., scales = "free", ncol = 2)+
        theme_light() +
        labs(col="Individual ID") +
        xlab("Time elapsed since deployment (mins)")
      
    }
    
    trip_plot
    
  })
  
  
  #---------------------------------#
  #### 3.6 Map of foraging trips ####
  #---------------------------------#
  
  output$tripmap = renderPlot({
    
    ## if foraging trips have not been classified then do not return any map
    if (is.null(trips$data)) return()
    
    
    ## add the data
    df_trips <- trips$data
    df_alltrips <- alltrips$data %>% 
      mutate(Fill = ifelse(as.numeric(trip_dur) > trips$MinDuration, "Retained" ,"Discarded"))
    
    ## Now filter the data for an individual if it is selected suing the radio buttons in the box
    if(input$PopSelect2 == "Individual"){
      df_trips <- filter(df_trips, ID == input$BirdSelect2)
      df_alltrips <- filter(df_alltrips, ID == input$BirdSelect2)
    }
    
    
    ## Show notification while the data is sub-sampled
    ForageMapNotif <- showNotification(shiny::span(shiny:::icon("hourglass-start"), "Rendering Map.."),
                                       type = "message", duration = NULL)
    
    ## add extra columns to data set for plotting
    ## This might be slowing the app down
    df_plotForage <- df_trips %>%
      group_by(Population, ID) %>%
      mutate(diffsecs = as.numeric(difftime),
             secs_elapsed = cumsum(replace_na(diffsecs, 0)),
             time_elapsed = as.duration(secs_elapsed),
             days_elapsed = as.numeric(time_elapsed, "days")) %>%
      mutate(across(c(dist,speed,CPdist, Lat, Lon), as.numeric))
    
    
    ## Read in the basemap that I already made, this map is made just once when a data set is read in
    Basemap <- Basemap()
    
    ## make ggplot
    ## might want to pre-render all of the tracks with just data(), should speed up the app
    if(input$PopSelect2 == "Individual"){
      Foragemap <- Basemap + 
        geom_point(data = filter(df_alltrips, Fill == "Discarded"), aes(x = Lon, y = Lat, col = Fill), alpha = 0.8, size = 0.5) +
        scale_colour_manual(values = c("#C5C5C5"), name = "Trip duration\ntoo short")+
        new_scale_color()+
        ##add GPS points and paths between then
        geom_point(data = df_plotForage, aes(x = Lon, y = Lat, col = TripID), size = 0.5 ) +
        geom_path(data = df_plotForage, aes(x = Lon, y = Lat, col = TripID, group = ID), alpha = 0.8, size = 0.5 ) +
        scale_colour_viridis_d(option = "plasma", end = 0.9)+
        coord_sf(xlim = c(min(df_plotForage$Lon), max(df_plotForage$Lon)), ylim = c(min(df_plotForage$Lat), max(df_plotForage$Lat)), crs = 4326) +
        ##add labels
        labs(x = "Longitude", y = "Latitude", col = "Trip ID", Fill = "Depth (m)") 
      
      ## remove the notification
      removeNotification(ForageMapNotif)
    }
    
    if(input$PopSelect2 == "Population"){
      Foragemap <- Basemap + 
        geom_point(data = filter(df_alltrips, Fill == "Discarded"), aes(x = Lon, y = Lat, col = Fill), alpha = 0.8, size = 0.5) +
        scale_colour_manual(values = c("#C5C5C5"), name = "Trip duration\ntoo short")+
        new_scale_color()+
        ##add GPS points and paths between then
        geom_point(data = df_plotForage, aes(x = Lon, y = Lat, col = ID), size = 0.5 ) +
        geom_path(data = df_plotForage, aes(x = Lon, y = Lat, col = ID, group = ID), alpha = 0.8, size = 0.5 ) +
        scale_colour_viridis_d()+
        coord_sf(xlim = c(min(df_plotForage$Lon), max(df_plotForage$Lon)), ylim = c(min(df_plotForage$Lat), max(df_plotForage$Lat)), crs = 4326) +
        ##add labels
        labs(x = "Longitude", y = "Latitude", col = "Individual ID", Fill = "Depth (m)") 
      
      ## remove the notification
      removeNotification(ForageMapNotif)
    }
    
    ## Add colony layer depending on CP definition (location or shape file)
    if(input$UserBox == "Colony distance buffer (Trip Start Distance)"){
      Foragemap <- Foragemap + 
        ##add central place point
        geom_point(data = df_plotForage, aes(x = CPLon, y = CPLat, fill = Population),
                   colour = "black", shape = 23, size = 2)
    }
    if(input$UserBox == "Shape file of colony"){
      req(Box$data)
      
      Col_shp <- st_sf(CP = "CP", geometry = st_as_sfc(Box$data, crs = 4326)) %>% mutate(CP = "CP")
      
      Foragemap <- Foragemap + 
        ggnewscale::new_scale_color()+
        geom_sf(data = Col_shp, inherit.aes = F, aes(fill = CP, colour = CP))+
        coord_sf(xlim = c(min(df_plotForage$Lon), max(df_plotForage$Lon)), ylim = c(min(df_plotForage$Lat), max(df_plotForage$Lat)), crs = 4326)
        
    }
    
    Foragemap
  })
  
  
  #------------------------------------------#
  #### 3.7 Create trip duration histogram ####
  #------------------------------------------#
  
  output$TripHist = renderPlot({
    
    if (is.null(trips$data)) return()
    
    ## add the data
    df_alltrips <- alltrips$data
    
    ## calculate trip duration by trip
    df_alltripdurations <- df_alltrips %>%
      group_by(Species, ID, TripID) %>%
      summarise(Trip_duration = difftime(max(DateTime), min(DateTime), units="mins"))
    
    ## plot trip durations
    triphist <- df_alltripdurations %>% 
      mutate(Fill = ifelse(as.numeric(Trip_duration) > trips$MinDuration, "Retained" ,"Discarded")) %>% 
      ggplot(aes(x = as.numeric(Trip_duration)))+
      geom_histogram(binwidth = 5, aes(fill = as.factor(Fill))) + # each bar = 5 minutes
      geom_vline(xintercept = input$MinTripTime, linetype = "dashed")+
      scale_x_continuous(breaks = seq(0, as.numeric(max(df_alltripdurations$Trip_duration)), 60))+ # major axis tick = 1 hour
      theme_light() +
      scale_fill_manual(values=c("#C5C5C5", "#595959")) +
      labs(x = "Trip duration (minutes)", fill = "Trip Retained?")
    
    triphist
    
  })
  
  
  #--------------------------------------#
  #### 3.8 Render map of shape file ####
  #--------------------------------------#
  
  ## render the UI for the map of the shape file
  ## because of the lazy loading in shiny i think this is only executed when we have a value for Box$data
  ## Box$data only has a value once we want to calcualte trips with the shape file and have pressed the action button
  output$Bounding_box_map <- renderUI({
    
    ## need to call this so that it only loads if this reactive value has been assigned some value
    req(Box$data)
    
    box(width = 12, title = "Map of colony shapefile", solidHeader = TRUE, status = "primary",
        fluidRow(column(width = 10, offset =1, leafletOutput("Bounding_box_leaflet"))))
    
  })
  
  
  ## render the leaflet map with the bounding box
  output$Bounding_box_leaflet <- renderLeaflet({
    
    ## need to call this so that it only loads if this reactive value has been assigned some value
    req(Box$data) 
    
    ## create the bounding box based off of the user input and tranfrom to an equal area projection
    #BB_box <- st_sf(CP = "CP", geometry = st_as_sfc(st_bbox(Box$data, crs = 4326))) 
    
    ## create a leaflet map with the bounding box
    leaf <- Box$data %>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addPolygons(color = "red")
    
    ## Now plot which ever of the two plots was created above
    leaf
    
    
  })
  
  
  #------------------------------------------------------------#
  #### 3.9 Text with current and previous input values used ####
  #------------------------------------------------------------#
  

  ## Render the text for the current selection of User input values
  ## Add text to the filtered table to tell the user the current filter values they have selected
  ## This just uses the slider values when the button is pressed
  tabletextforage <- eventReactive(input$TripStatButton, {
    
    ## text to display if Colony distance buffer used to calcualte trip stats
    if(input$UserBox == "Colony distance buffer (Trip Start Distance)"){
      
      text <- paste0("Trip Start Distance = ", input$ColDistBuffer, "km | ",
             "Minimum Trip Duration = ", input$MinTripTime, "mins ")
      
    }
    
    ## text to display if Colony shapefile used to calcualte trip stats  
    if(input$UserBox == "Shape file of colony"){
      
      text <- paste0("Trip Start Distance = ", "Custom shapefile chosen | ",
             "Minimum Trip Duration = ", input$MinTripTime, "mins ")
    }
    
    text
    
  })

  ## render the text to the table for the foraging trip calculation
  output$ForageSelection = renderText({ tabletextforage() })
  
  
  ## Render the text for the current selection of User input values
  ## Add text to the filtered table to tell the user the current filter values they have selected
  tabletextforagePrev <- reactive({
    
    ## this mean this reactive statement only runs when the trips2$MinDuration reactive value is changed
    req(trips2$MinDuration)
    
    if(trips2$buffer == "Colony distance buffer (Trip Start Distance)"){
      
      text2 <- paste0("Trip Start Distance = ", trips2$ColBuffer, "km | ",
             "Minimum Trip Duration = ", trips2$MinDuration, "mins ")
      
    }
    
    ## text to display if Colony shapefile used to calcualte trip stats  
    if(trips2$buffer == "Shape file of colony"){
      
      text2 <-paste0("Trip Start Distance = ", "Custom shapefile chosen | ",
             "Minimum Trip Duration = ", trips2$MinDuration, "mins ")
    }
    
    text2
    
  }) 
  
  ## render the text to the table for the previous foraging trip calculation
  output$ForageSelectionPrev = renderText({ tabletextforagePrev() })
  
  
  #------------------------------#
  #### 3.10 Custom code output ####
  #------------------------------#
  
  ## Render code chunk with user defined values
  output$codechunk_trips <- renderText({ 
    
    ## If foraging trips have not been calculated then do not render the code chunk
    if (is.null(trips$MinDuration)) return(paste0('Calculate foraging trips first'))
    
    ## If the have calculated the foraging trips then render the code chunk
    paste0(
          '#--------------------#
            ##USER INPUT START##
            #--------------------#
            
            ## add a threshold distance
            threshold_dist <- ',trips$ColBuffer*1000,' #distance in metres
            
            ## add a threshold for time
            threshold_time <- ',trips$MinDuration,' #time in minutes
            
            #-----------------#
            ##USER INPUT END##
            #-----------------#')
    
  })
  
  
  
  
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
  #### 4.3 Create Pop sampling interval summary boxes ####
  #------------------------------------------------------#
  
  ## Create an Info box with the ID of the individual
  output$MeanSampInt <- renderValueBox({
    
    MeanInt <- round(mean((data()$difftime/60), na.rm=T), digits = 1)
    
    infoBox("Mean", paste0(MeanInt, " mins"), color = "purple", icon = shiny::icon("clock"))
    
  })
  
  ## Create an Info box with the the number of GPS locations
  output$SDSampInt <- renderValueBox({
    
    sdInt <- round(sd((data()$difftime/60), na.rm=T), digits = 1)
    
    infoBox("Standard Deviation", paste0(sdInt, " mins"), color = "blue", icon = shiny::icon("globe"))
    
  })
  
  ## Create an Info box with the date rnage of the GP\S data for that individual
  output$RangeSampInt <- renderValueBox({
    
    minInt <- round(min((data()$difftime/60), na.rm=T) ,digits = 1)
    maxInt <- round(max((data()$difftime/60), na.rm=T) ,digits = 1)
    
    infoBox("Range", paste0(minInt, " mins - ", maxInt, " mins"), color = "teal", icon = shiny::icon("calendar"))
    
  })
  
  
  #----------------------------------------------------------#
  #### 4.4 Create sampling interval tables per individual ####
  #----------------------------------------------------------#
  
  ## create a reactive value here, means I only have to create the original summary once for the full trakcing data set
  RawSumSubSamp <- reactive({
    
    ## insert the tracking data the user has read in
    df_RAW <- data()
    
    ## summarise the RAW samplig data
    dfRAW_summary <- df_RAW %>% 
                    group_by(Species, Population, ID) %>%
                    summarise(NoFixesRAW = n(),
                              Sampling_IntervalMEAN = round(mean((difftime/60), na.rm=T), digits = 1),
                              Sampling_IntervalSD = round(sd((difftime/60), na.rm=T), digits = 1),
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
                          Sampling_IntervalMEAN = round(mean((difftime/60), na.rm=T), digits = 1),
                          Sampling_IntervalSD = round(sd((difftime/60), na.rm=T), digits = 1),
                          Sampling_IntervalSub = paste0(Sampling_IntervalMEAN, " ± ", Sampling_IntervalSD)) %>% 
                ungroup() %>% 
                select(-c(Sampling_IntervalMEAN, Sampling_IntervalSD)) 
    
    ## join together the RAW and sub-sampled data
    SubSampTable <- full_join(df_subsamp_summary, RawSumSubSamp(), by = c("Species", "Population", "ID"))
    
    ## Add column for the difference in the number of fixes and order dataset
    SubSampTable <- SubSampTable %>% 
                    mutate(FixesRetained = paste(round((NoFixesSub/NoFixesRAW*100),digits=2), "%")) %>% 
                    select(Species, Population, ID, NoFixesRAW, NoFixesSub, FixesRetained, 
                           Sampling_Interval_RAW, Sampling_IntervalSub) %>% 
                    rename('Raw: Mean Interval (mins)' = Sampling_Interval_RAW, 'Sub-samp: Mean Interval (mins)' = Sampling_IntervalSub,
                          'Raw: No Locations' =  NoFixesRAW, 'Sub-Samp: No Locations' = NoFixesSub, '% Locations Retained' = FixesRetained)
    
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
  output$SubSampSelection = renderText({ paste(tabletextSamp()) })
  
    
  #--------------------------------------------------#
  #### 4.5 Create histogram of sampling intervals ####
  #--------------------------------------------------#
  
  output$SubSampHist = renderPlot({
    
    ## if the data has not been sub-sampled yet then just use ther RAW data to create the histogram
    if (is.null(SubSamp$data)) return({
      
      ## add the data
      df_sub <- data()
      
      ## maximum sampling rate
      maxint <- as.numeric(max(as.numeric(df_sub$difftime), na.rm = T))
      
      ## plot sampling intervals
      SubSamphist <- ggplot(df_sub, aes(x = as.numeric((difftime/60), na.rm=T)))+
        geom_histogram(bins = 50)+ # each bar = 5 minutes
        scale_x_continuous(n.breaks = 20)+ # major axis tick = 1 hour
        theme_light()+
        theme(axis.title = element_text(size = 16), axis.text = element_text(size =12)) +
        xlab("Trip duration (mins)") + ylab("Frequency")
      
      SubSamphist})
    
    
    ## If the data has been sub-sampled then SubSamp$data will not be NULL and the code above inside the
    ## IF statement will not be run
    ## add the data
    df_sub <- SubSamp$data
    
    ## maximum sampling rate
    maxint <- as.numeric(max(as.numeric(df_sub$difftime), na.rm = T))
    
    ## plot sampling intervals
    SubSamphist <- ggplot(df_sub, aes(x = as.numeric((difftime/60), na.rm=T)))+
      geom_histogram(bins = 50)+ # each bar = 5 minutes
      scale_x_continuous(n.breaks = 20)+ # major axis tick = 1 hour
      theme_light() +
      theme(axis.title = element_text(size = 14), axis.text = element_text(size =12)) +
      xlab("Trip duration (mins)") + ylab("Frequency")
    
    SubSamphist
    
  })
  
  
  
  
  #-------------------------#
  #### 5. Segmenting Tab ####
  #-------------------------#
  
  #-----------------------#
  #### 5.0 Help Button ####
  #-----------------------#
  
  ## call the help button module, when the specified help button the code in the module is run
  ## The includeMarkdown() function basically renders the markdown at the filepath I've specified and included it in the app
  callModule(click_help, "SegmentHelp", title = "Help page",
             size = "l", file = "help/p4.0 Segmenting Tab.md")
  
  
  #-------------------------------------------------------#
  #### 5.1 Set up reactive values & change variable UI ####
  #-------------------------------------------------------#
  
  ## create an empty reactive value that we will later put the tracking data labelled with trips
  seg <- reactiveValues(data = NULL)
  
  ## reactive value for thresholds
  thresholds <- reactiveValues(data = NULL)
  
  ## put all segments here (i.e. before removing short ones)
  seg_all <- reactiveValues(data = NULL)
  
  
  ## This updates the UI whenever data() changes so whenever a new file is read in
  observe({
    
    x <- data()
    
    max_time <- max(x$difftime, na.rm = T)/60
    

    # I need to identify which select input i want to update and then tell it what the new choices should be
    updateSelectInput(session, "BirdSelect3", choices = unique(x$ID))
    updateSliderInput(session, "IntervalThreshold", value= (ceiling(max_time)-ceiling(max_time/10)), max= ceiling(max_time))
    
  })
  
  observe({
    
    if (is.null(seg_all$data)) return()
    
    x <- seg_all$data
    
    max_length <- max(x$segment_length)
    
    updateSliderInput(session, "MinSegThreshold",  max= ceiling(max_length))
    
  })
  
  
  
  #------------------------------#
  #### 5.2 Calculate segments ####
  #------------------------------#
  
  ## Calculate segments once they press the action button
  ## If they haven't pressed the action button nothing will display 
  ## I will set the default values to something sensible so they could just press the button without
  ## Changing the slider and still get something sensible
  
  observeEvent(input$SegmentButton,{
    
    ## each time the action button is pressed the input values are assigned to reactive values
    ## These can then later be used to colour the histograms
    thresholds$ThresholdInterval <- input$IntervalThreshold
    thresholds$MinSegLength <- input$MinSegThreshold
    
    ## update select inputs for the interacitve map on the last page
    if(is.null(trips$data)==TRUE){updateSelectInput(session , "IntMapColour", choices =  c("Speed", "Turning Angle", "Age", "Time of Day", "Segment ID"))}
    if(is.null(trips$data)==FALSE){updateSelectInput(session , "IntMapColour", choices =  c("Speed", "Turning Angle", "Age", "Time of Day", "Foraging Trip ID", "Segment ID"))}
    
  })
  
  
  
  ## Calculate trip stats if action button is pressed
  observeEvent(input$SegmentButton, {
    
    ## Show notification while the data is sub-sampled
    SegmentCalcNotif <- showNotification(shiny::span(shiny:::icon("hourglass-start"), "Calculating Segments.."),
                                       type = "message", duration = NULL)
    
    ## define inputs for segments
    threshold_time <- as.period(input$IntervalThreshold, unit = "minutes")
    
    ## difftime in data are in seconds
    threshold_units <- as.numeric(as.period(threshold_time, "seconds"))
    
    # segment length threshold
    threshold_points <- input$MinSegThreshold
    
    ## bring in the tracking data that was read in by the user
    df_diagnostic <- data()
    
    df_segments <- df_diagnostic %>%
      group_by(ID) %>%
      mutate(segment_start = case_when(row_number()==1 ~ TRUE,
                                       difftime > threshold_units ~ TRUE),
             segment_row_number = case_when(segment_start == TRUE ~ cur_group_rows())) %>%
      fill(segment_row_number) %>%
      ungroup() %>% group_split(ID) %>%
      purrr::map_df(~.x %>% group_by(segment_row_number) %>% mutate(segment_num = cur_group_id())) %>% # assign sequential segment number
      ungroup() %>% select(-c(segment_row_number)) %>% # remove intermediate column
      mutate(SegmentID = paste0(ID, "_", segment_num)) %>%
      group_by(SegmentID) %>%
      add_count() %>% # number of fixes within segment
      rename(segment_length = n)
    
    
    ## remove short segments
    ## If you want to keep all segments in, remove the last line of code to filter the data by segment length
    df_segments_valid <- df_segments  %>%
      filter(segment_length >= threshold_points) %>%
      ungroup()
    
    ## assign the classified trips to a reactive value
    seg$data <- df_segments_valid
    seg_all$data <- df_segments
    
    ##remove the rendering notification for this section
    removeNotification(SegmentCalcNotif)
    
  })
  
  
  #----------------------------------#
  #### 5.3 Segment summary tables ####
  #----------------------------------#
  ## Make the table the summaries the current segments, from the most recent press of the action button

  output$SegmentTableFilt = renderDT({
    
    if (is.null(seg$data)) return()
    
    ## add the data
    df_segments <- seg$data
    
    df_segment_summary <- df_segments %>%
      group_by(Species, Population, ID, SegmentID) %>% 
      mutate(cum_dist = cumsum(dist)) %>%
      summarise(Segment_start = min(DateTime),
                Segment_end = max(DateTime),
                Segment_duration = difftime(max(DateTime), min(DateTime), units="mins"),
                Total_distance = sum(dist, na.rm = T)) 
    
    # intermediate = individual level summary = 1 row per individual
    # each individual will therefore have equal weighting in the population summary
    df_segment_summary_ind <- df_segment_summary %>%
      group_by(Species, Population, ID) %>% 
      summarise(Segment_duration_ind_mean = mean(as.numeric(Segment_duration, units = "mins"), na.rm = T),
                Segment_duration_ind_se = se(as.numeric(Segment_duration, units = "mins")),
                Total_distance_ind_mean = mean(Total_distance),
                Total_distance_ind_se = se(Total_distance),
                N_Segments = length(unique(SegmentID))) 
    
    df_segment_summary_pop <- df_segment_summary_ind %>%
      group_by(Species, Population) %>%
      summarise(N_Segments_total = sum(N_Segments),
                N_Segments_perInd = paste0(round(mean(N_Segments, na.rm = T), digits = 1), " ± ",
                                           round(se(N_Segments), digits = 1)),
                Segment_duration = paste0(round(mean(Segment_duration_ind_mean, na.rm = T), digits = 1), " ± ",
                                          round(se(Segment_duration_ind_mean), digits = 1)),
                Segment_distance_km = paste0(round((mean(Total_distance_ind_mean, na.rm = T)/1000), digits = 1), " ± ",
                                             round((se(Total_distance_ind_mean)/1000), digits = 1))) %>% 
      rename(`Segment Duration (mins)` = Segment_duration, `Segment Dist (km)` = Segment_distance_km,
             `No Segments` = N_Segments_total, `Segments per Individual` = N_Segments_perInd)
    
    ## create the table of the trip summary
    datatable(df_segment_summary_pop, options = list(
      scrollX = TRUE,
      scrollY = "240px",
      paging = FALSE
    ))
    
    
  })
  
  
  # Add text to the filtered table to tell the user the current filter values they have selected
  tabletextSeg <- eventReactive(input$SegmentButton, {
    paste0("Threshold Interval to segment Track = ", thresholds$ThresholdInterval, " mins | ",
           "Minimum segment length = ", thresholds$MinSegLength, " locations")
    
  })
  
  ## render the text to the table
  output$SegmentSelection = renderText({ paste(tabletextSeg()) })
  
  
  #-----------------------------------#
  #### 5.4 Plot segments over time ####
  #-----------------------------------#
  
  output$SegmentClassPlot = renderPlot({
    
    if (is.null(seg$data)) return()
    
    ## add the data
    df_segments <- seg$data
    df_all <- seg_all$data %>% 
      mutate(Fill = ifelse(segment_length < thresholds$MinSegLength, "Discarded" , "Retained"))
    
    ## Now filter the data for an individual id it is selected in the box
    if(input$PopSelect3 == "Individual"){
      df_segments <- filter(df_segments, ID == input$BirdSelect3)
      df_all <- filter(df_all, ID == input$BirdSelect3)}
    
    ## create individual-level plots
    if(input$PopSelect3 == "Individual"){
      
      ## make the plot
      segment_plot <- ggplot()+
        geom_point(data = filter(df_all, Fill == "Discarded"), aes(x = DateTime, y = netdisp, col = Fill), alpha = 0.8, size = 1) +
        scale_colour_manual(values = c("#C5C5C5"), name = "Segment length\ntoo short")+
        new_scale_color()+
        geom_point(data = df_segments, aes(x = DateTime, y = netdisp, col = as.factor(segment_num)), size = 1)+
        scale_colour_viridis_d(option = "plasma", end = 0.9)+
        #facet_wrap(ID~., scales = "free", ncol = 2)+
        labs(col="Segment number") + ylab("Net displacement")+
        theme_light()
      
    }
    
    ## create population-level plot
    if(input$PopSelect3 == "Population"){
      
      ## make the plot
      segment_plot <- ggplot()+
        geom_point(data = filter(df_all, Fill == "Discarded"), aes(x = SinceStart, y = netdisp,  group = ID, col = Fill), alpha = 0.8, size = 1) +
        scale_colour_manual(values = c("#C5C5C5"), name = "Segment length\ntoo short")+
        new_scale_color()+
        geom_point(data = df_segments, aes(x = SinceStart, y = netdisp,  group = ID, col = as.factor(ID)), size = 1)+
        #facet_wrap(ID~., scales = "free", ncol = 2)+
        scale_colour_viridis_d()+
        labs(col="Segment number") + ylab("Net displacement")+ xlab("Time elapsed since deployment (mins)")
        theme_light()
    }
    
    segment_plot
    
  })
  
  
  
  #---------------------------#
  #### 5.6 Map of segments ####
  #---------------------------#
  
  output$Segmentmap = renderPlot({
    
    ## if foraging trips have not been classified then do not return any map
    if (is.null(seg$data)) return()
    
    ## add the data
    df_segments <- seg$data
    df_all <- seg_all$data %>% 
      mutate(Fill = ifelse(segment_length < thresholds$MinSegLength, "Discarded" , "Retained"))
      
    ## Show notification while the data is sub-sampled
    SegmentMapNotif <- showNotification(shiny::span(shiny:::icon("hourglass-start"), "Rendering Map.."),
                                       type = "message", duration = NULL)
    
    
    ## Read in the basemap that I already made, this map is made just once when a data set is read in
    Basemap <- Basemap()
    
    ## Now filter the data for an individual if it is selected suing the radio buttons in the box
    if(input$PopSelect3 == "Individual"){
      df_segments <- filter(df_segments, ID == input$BirdSelect3)
      df_all <- filter(df_all, ID == input$BirdSelect3)}
    
    ## make ggplot
    ## might want to pre-render all of the tracks with just data(), should speed up the app
    
    if(input$PopSelect3 == "Individual"){
    Segmemtmap <- Basemap + 
      ##add GPS points and paths between then
      geom_point(data = filter(df_all, Fill == "Discarded"), aes(x = Lon, y = Lat, col = Fill), alpha = 0.8, size = 0.5) +
      scale_colour_manual(values = c("#C5C5C5"), name = "Segment length\ntoo short")+
      new_scale_color()+
      geom_point(data = df_segments, aes(x = Lon, y = Lat, col = SegmentID), alpha = 0.8, size = 0.5 ) +
      geom_path(data = df_segments, aes(x = Lon, y = Lat, col = SegmentID, group = ID), alpha = 0.8, size = 0.5 ) +
      scale_colour_viridis_d(option = "plasma", end = 0.9)+
      coord_sf(xlim = c(min(df_all$Lon), max(df_all$Lon)), ylim = c(min(df_all$Lat), max(df_all$Lat)), crs = 4326) +
      ##add labels
      labs(x = "Longitude", y = "Latitude", col = "Segment ID")
    
    ## remove the notification
    removeNotification(SegmentMapNotif)
    }
    
    if(input$PopSelect3 == "Population"){
      Segmemtmap <- Basemap + 
        ##add GPS points and paths between then
        geom_point(data = filter(df_all, Fill == "Discarded"), aes(x = Lon, y = Lat, col = Fill), alpha = 0.8, size = 0.5) +
        scale_colour_manual(values = c("#C5C5C5"), name = "Segment length\ntoo short")+
        new_scale_color()+
        geom_point(data = df_segments, aes(x = Lon, y = Lat, col = ID), alpha = 0.8, size = 0.5 ) +
        geom_path(data = df_segments, aes(x = Lon, y = Lat, col = ID, group = ID), alpha = 0.8, size = 0.5 ) +
        scale_colour_viridis_d()+
        coord_sf(xlim = c(min(df_all$Lon), max(df_all$Lon)), ylim = c(min(df_all$Lat), max(df_all$Lat)), crs = 4326) +
        ##add labels
        labs(x = "Longitude", y = "Latitude", col = "Segment ID")
      
      ## remove the notification
      removeNotification(SegmentMapNotif)
    }
    
    ## retrun the map
    Segmemtmap
    
  })
  
  
  #----------------------------------------------------------------------#
  #### 5.7 Create histograms of sampling intervals and segment length ####
  #----------------------------------------------------------------------#
  
  output$ThresholdIntervalHist = renderPlot({
    
    ## if the data has not been sub-sampled yet then just use the RAW data to create the histogram
    if (is.null(seg$data)) return({
      
      ## add the data
      df_hist <- data()
      
      ## plot sampling intervals
      ThresholdHist <- ggplot(df_hist, aes(x = as.numeric((difftime/60), na.rm=T)))+
        geom_histogram(binwidth=5)+ # each bar = 5 minutes
        scale_x_continuous(n.breaks = 20)+ # major axis tick = 1 hour
        theme_light()+
        theme(axis.title = element_text(size = 16), axis.text = element_text(size =12)) +
        xlab("Interval") + ylab("Frequency")
      
      ThresholdHist
      })
    
    
    ## If the data has been sub-sampled show the threshold as a dashed line
    df_hist <- data()
   
     ## define inputs for segments
    threshold_time <- as.period(input$IntervalThreshold, unit = "minutes")
    
    ThresholdHist <- df_hist %>% 
      drop_na(difftime) %>% 
      mutate(Fill = ifelse(as.numeric((difftime/60)) > as.numeric(thresholds$ThresholdInterval), "Exceeded" ,"Not Exceeded")) %>% 
      ggplot(aes(x = as.numeric((difftime/60), na.rm=T)))+
      geom_histogram(aes(fill = as.factor(Fill)), binwidth=5)+ # each bar = 5 minutes
      geom_vline(xintercept = input$IntervalThreshold, linetype = "dashed")+
      scale_x_continuous(n.breaks = 30)+ # consider changing from 30
      theme_light()+
      theme(axis.title = element_text(size = 16), axis.text = element_text(size =12), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      scale_fill_manual(values=c("#C5C5C5", "#595959")) +
      labs(fill = "Threshold Exceeded?") +
      xlab("Interval") + ylab("Frequency")
    
    ThresholdHist
    
  })
  
  output$SegmentLengthHist = renderPlot({
    req(data())
    if (is.null(seg_all$data)) return(warning("segments not calculated"))
    
    ## add the data
    df_hist <- seg_all$data
    
   
    ## plot sampling intervals
    SegLengthHist <- df_hist %>% 
      mutate(Fill = ifelse(segment_length < thresholds$MinSegLength, "Discarded" ,"Retained")) %>% 
      ggplot(aes(x = segment_length))+
      geom_histogram(aes(fill = as.factor(Fill)), binwidth=5)+ 
      scale_x_continuous(n.breaks = 20, limits = c(0, max(df_hist$segment_length)))+ # major axis tick 
      scale_fill_manual(values=c("#C5C5C5", "#595959")) +
      geom_vline(xintercept = input$MinSegThreshold, linetype = "dashed")+
      theme_light()+
      theme(axis.title = element_text(size = 16), axis.text = element_text(size =12), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(fill = "Segment retained?") +
      xlab("Segment length (No. locations") + ylab("Frequency")
    
    SegLengthHist
  })
  
  
  
  #------------------------------#
  #### 6. Interactive Map Tab ####
  #------------------------------#
  
  #-----------------------#
  #### 6.0 Help Button ####
  #-----------------------#
  
  ## call the help button module, when the specified help button the code in the module is run
  ## The includeMarkdown() function basically renders the markdown at the filepath I've specified and included it in the app
  callModule(click_help, "IntMapHelp", title = "Help page",
             size = "l", file = "help/p5.0 Interactive Map Tab.md")
  
  #--------------------------------------------#
  #### 6.1 Reactive Value for Map selection ####
  #--------------------------------------------#
  
  ## Want to be able to plot All Individuals vs selected individuals
  ## Change the color between speed, trip ID, distance to CP and turning angle
  ## Can already chnage the background map in leaflet viewer
  ## Also be able to plot difference sexes, ages and colonies
  
  ## Create a reactive value to put all of the map selections in, think you can store multiple values in one reactive values
  IntMap <- reactiveValues(data = NULL)
  
  ## Now use an observe event of the input$IntMapButton, to assign the options for map plotting to the reactive value
  observeEvent(input$IntMapButton, {

    IntMap$colour <- input$IntMapColour


  })
  
  
  #-------------------------------------------------#
  #### 6.2 Update user input based off the data ####
  #-------------------------------------------------#
  
  ## Update the user input options that need changing
  observe({
    
    x <- data()
    
    # I need to identify which select input I want to update and then tell it what the new choices should be
    updateSelectInput(session, "BirdSelectMap", choices = unique(x$ID))
    
  })
  
  
  #-------------------------------------------#
  #### 6.3 Filter data for the leaflet map ####
  #-------------------------------------------#
  
  ## This just assigns the map filter Options to reactive values so that the map is not rendered each time the user presses the radio buttons
  observeEvent(input$IntMapButton, {
 
    ## Assign the tracking data to the reactive value
    IntMap$data <- data()
    
    ## Assign the plot filter selection to a reactive value
    if(input$PopSelectMap == "Individual"){IntMap$PopSelect <- input$PopSelectMap; IntMap$IndChoice <- input$BirdSelectMap}
    if(input$PopSelectMap == "Population"){IntMap$PopSelect <- input$PopSelectMap}
    
    
  })
  
  
  
  #----------------------------------#
  #### 6.4 Render the leaflet map ####
  #----------------------------------#
  
  ## Render leaflet map of all locations
  ## This call a function which produces the leaflet plot, this can be found in utils.R
  ## Doing it this way gives us more control over how the map is rendered depending on what the colour is
  output$IntLeafletMap = renderLeaflet({
    
    ## If not values have been assigned to the reactive value yet then do not render the leaflet map  
    if (is.null(IntMap$data)) return()
    
    # ## Show notification while the map is rendered
    IntMapNotif <- showNotification(shiny::span(shiny:::icon("hourglass-start"), "Rendering Interactive Map.."),
                                    type = "message", duration = NULL)
    
    ## now call the map function dependent on what the variables chosen does
    # Options in the select input = c("Speed", "Turning Angle", "Age", "Time of Day", "Foraging Trip ID", "Segment ID")
    if(IntMap$colour == "Speed"){MAP1 <- MapFunc(Mapdata = IntMap$data, column = "speed", label = "Speed (m/s)", vartype = "numeric", PopSelectMap = IntMap$PopSelect, BirdSelectMap = IntMap$IndChoice)}
    if(IntMap$colour == "Turning Angle"){ MAP1 <- MapFunc(Mapdata = IntMap$data, column = "turnangle", label = "Degrees", vartype = "numeric", PopSelectMap = IntMap$PopSelect, BirdSelectMap = IntMap$IndChoice)}
    if(IntMap$colour == "Age"){ MAP1 <- MapFunc(Mapdata = IntMap$data, column = "Age", label = "Age category", vartype = "factor", PopSelectMap = IntMap$PopSelect, BirdSelectMap = IntMap$IndChoice)}
    ## time does not quite work, the legend is just in seconds and not a readable time. MIght be able to just transform in the addLegend function in leaflet
    if(IntMap$colour == "Time of Day"){ MAP1 <- MapFunc(Mapdata = IntMap$data, column = "Time", label = "Time of day", vartype = "time", PopSelectMap = IntMap$PopSelect, BirdSelectMap = IntMap$IndChoice)}
    ## Colour by derived variables, note here we have to call reactive datasets that we created in previous sections
    if(IntMap$colour == "Foraging Trip ID"){ MAP1 <- MapFunc(Mapdata = trips$data, column = "TripID", label = "Foraging Trip ID", vartype = "factor", PopSelectMap = IntMap$PopSelect, BirdSelectMap = IntMap$IndChoice)}
    if(IntMap$colour == "Segment ID"){ MAP1 <- MapFunc(Mapdata = seg$data, column = "SegmentID", label = "Segment ID", vartype = "factor", PopSelectMap = IntMap$PopSelect, BirdSelectMap = IntMap$IndChoice)}
    
    ## remove the notification
    removeNotification(IntMapNotif)
    
    ## return the map
    MAP1
    
    
  })
  

} # this bracket closes the whole server function




#-------------------------------#
#### 7. Create the shiny app ####
#-------------------------------#

## All done, make the app...
shinyApp(ui, server)

