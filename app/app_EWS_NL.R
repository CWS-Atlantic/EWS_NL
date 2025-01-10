# Shiny app to interactively explore EWS NL data from 1990 to 2024

require(dplyr)
require(ggplot2)
require(leaflet)
require(leaflet.extras)
require(leafpop)
require(mapedit)  ##make sure you have the most recent version ## remotes::install_github("r-spatial/mapedit") 
require(miniUI)
require(RColorBrewer)
require(readr)
require(sf)
require(shiny)
require(shinyWidgets)
require(tidyr)


################################################
##   read in EWS NL data and do some manips   ##
################################################

#setwd("C:/Users/englishm/Documents/EWS/NL/Github/app/")


# #Read in EWS NL data ## old version before we moved to a GDB

# ews.sf <- read_csv(file = "data/EWS_NL_24-06-2024.csv")
# 
# #ews.sf <- read_csv(file = "C:/Users/englishm/Documents/EWS/Shiny App/EWS_NL_16Jan2024.csv", col_types = cols(observation =  col_character()))
# 
# ews.sf <-  ews.sf[order(ews.sf$year),] 
# 
# # convert ews to a sf
# ews.sf <- st_as_sf(ews.sf, 
#                    coords = c("lon", "lat"), 
#                    crs = 4326, 
#                    agr = "constant", 
#                    remove = FALSE)

ews.sf <- st_read("data/EWS_NL_AppData_2024-06-24.gdb")

ews.sf <- st_transform(ews.sf, 4326)

#do some renaming
names(ews.sf)[names(ews.sf) == 'TIP'] <- 'tip'

names(ews.sf)[names(ews.sf) == 'tot'] <- 'total'


##############################
##   create species lists   ##
##############################

## subset out species of interest.  just exploratory for now

## mammals and raptors are not included at the moment!

waterfowl <- c("ABDU", 
               "AGWT",
               "AMWI",
               "BAGO",
               "BLSC",
               "BUFF",
               "BWTE",
               "CAGO", 
               "COGO", 
               #"COLO", 
               "COME",
               "GRSC",
               "HARD",
               "HOME",
               "LESC",
               "LTDU",
               "MALL",
               "NOPI",
               "NSHO",
               "RBME", 
               "RNDU",
               "RTLO",
               "SUSC",
               "USCA",
               "WODU",
               "WWSC")


#waterbirds
waterbirds <- c("COLO",
                "DCCO",
                "GBBG",
                "HERG",
                "RBGU",
                "RTLO",
                "TERN",
                "UNPH")

shorebirds <- c("COSN",
                "SOSA",
                "SPSA",
                "UNYE")

raptors <- c("BAEA",
             "GHOW",
             "NOHA",
             "OSPR",
             "RTHA")

mammals <- c("BLBE",
             "BEAV",
             "CARI",
             "COYT",
             "MINK",
             "MUSK",
             "MOOS",
             "PORC",
             "RFOX",
             "RIOT",
             "WOLF")


#####################################
##   create SF objects for plots   ##
#####################################

# #Read in EWS NL plots shapefile
# plots <- st_read("data/nfplotloc_poly.shp")
# 
# 
# ## #Append the .dbf file for the plots
# # plot.info <- read_csv("data/nfplotloc_poly_.csv")
# # 
# # plots@data$YEAR_ADDED <- plot.info$YEAR_ADDED
# # plots@data$DESC <- plot.info$DESC
# # 
# # st_write(plots.sf, "nfplotloc_poly.shp")
# 
# plots.sf <- st_as_sf(plots)
# 
# plots.sf <- st_transform(plots.sf, 4326)
# 
# names(plots.sf) <- c("plot", "plot_name", "utm", "year_added", "description", "geometry")

##write out plots as a GDB
#st_write(plots.sf, layer = "EWS_NL_Plots", dsn = "EWS_NL_Plots.gdb", driver = "OpenFileGDB", append = F)

plots.sf <- st_read("data/EWS_NL_Plots.gdb")

ews.data.sf <- ews.sf
# 
# 
# # convert ews to a sf
# ews.data.sf <- st_as_sf(ews.data.sf, 
#                         coords = c("lon", "lat"), 
#                         crs = 4326, 
#                         agr = "constant", 
#                         remove = FALSE)



#################################################################
##   identify obs that are in or outside the plot boundaries   ##
#################################################################

#make the plots and ews.sf data have the same CRS
st_crs(plots.sf) <- st_crs(ews.sf)

#identify the obs that are out and put them in a separate DF
ews.out <- ews.sf[!lengths(st_intersects(ews.sf, plots.sf)), ]

#kick out the 'out' obs from the ews.sf DF
ews.sf <- ews.sf[!ews.sf$SHAPE %in% ews.out$SHAPE,]

#assign a new variable "over" and then assign which points are "In" and "Out"
ews.sf$over <- "In"
ews.out$over <- "Out"

#recombine the two DFs in to one
ews.sf <- rbind(ews.sf, ews.out)


#########################
##   create shiny UI   ##
#########################

ui <- miniPage(
  
  #this supresses warning messages in the app. Needed because of how the plot is initially rendered
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  gadgetTitleBar("EWS Data from 1990-2024", left = NULL, right = NULL),
  
  miniTabstripPanel(
    
    miniTabPanel("ReadMe", icon = icon(name = "readme",
                                       lib = "font-awesome"),
                 miniContentPanel(htmlOutput("text"))),
    
    miniTabPanel("Map", icon = icon(name = "map",
                                    lib = "font-awesome"), 
                 miniContentPanel(
                   editModUI("mymap", height = "100%", width = "100%"),
                   
                   absolutePanel(id = "controls", 
                                 class = "panel panel-default", 
                                 fixed = TRUE, 
                                 draggable = TRUE, 
                                 top = 120, 
                                 left = "auto", 
                                 right = 20, 
                                 bottom = "auto", 
                                 width = 420, 
                                 height = "auto",
                                 style = "z-index: 1000; opacity: 0.925",
                                 HTML('<button data-toggle="collapse" data-target="#summarydata">-</button>'), #this is the minimize button
                                 tags$div(id = 'summarydata',  class="collapse-in"),
                                 
                                 h4("Summary Data"),
                                 
                                 radioButtons("dataInput",
                                              label = "Select Data For Plot",
                                              choices = c("TIP", "Total Birds"),
                                              inline = F,
                                              selected = "TIP"),
                                 
                                 plotOutput("plot"),
                                 
                                 uiOutput("downloadDataButton")),
                   
                   
                   absolutePanel(id = "controls", 
                                 class = "panel panel-default", 
                                 fixed = TRUE,
                                 draggable = TRUE, 
                                 top = 370, 
                                 left = 20, 
                                 right = "auto", 
                                 width = 220,
                                 HTML('<button data-toggle="collapse" data-target="#dataselect">-</button>'),
                                 tags$div(id = 'dataselect',  class="collapse-in"),
                                 
                                 sliderInput("range", "Years", min(ews.sf$year), max(ews.sf$year),
                                             value = range(ews.sf$year), step = 1, width = "96%"),
                                 
                                 pickerInput("waterfowlInput", "Waterfowl", waterfowl,
                                             selected = "ABDU", multiple = T,
                                             options = list("size" = 5, "actions-box" = T)),
                                 
                                 pickerInput("waterbirdsInput", "Waterbirds", waterbirds,
                                             multiple = T,
                                             options = list("size" = 5, "actions-box" = T)),
                                 
                                 pickerInput("shorebirdsInput", "Shorebirds", shorebirds,
                                             multiple = T,
                                             options = list("size" = 5, "actions-box" = T)),
                                 
                                 pickerInput("raptorsInput", "Raptors", raptors,
                                             multiple = T,
                                             options = list("size" = 5, "actions-box" = T)),
                                 
                                 pickerInput("mammalsInput", "Mammals", mammals,
                                             multiple = T,
                                             options = list("size" = 5, "actions-box" = T)),
                                 
                                 radioButtons("radio",
                                              label = "Select Palette",
                                              choices = c("Plot", "Year"),
                                              inline = F,
                                              selected = "Year")
                   )
                 )
    )
  )
)


server <- function(input, output, session) {
  
  
  ###########################
  ##    Colour Palettes    ##
  ###########################
  
  
  #create a palette that changes based on the radio button selection - need to find a better way to do this
  
  selectedPal <- reactive({ 
    
    if(input$radio=="Plot"){
      pal <- colorFactor(palette = 'Set3',
                         domain = ews.sf$plot
      )
      return(pal(ews.sf$plot))
    } 
    
    else if(input$radio=="Year"){
      pal <- colorFactor(palette = 'Set3',
                         domain = ews.sf$year
      )
      return(pal(ews.sf$year))
    }
  })
  
  
  ###################
  ##    Mapping    ##
  ###################
  
  #setup a reactive dataset that filters the data based on the year selection on the slider
  filteredData <- reactive({
    ews.sf[ews.sf$year >= input$range[1] & ews.sf$year <= input$range[2],]
  }) 
  
  #do the same but for selected species
  filteredSpecies <- reactive({
    filteredData()[filteredData()$species %in% c(input$waterfowlInput, 
                                                 input$mammalsInput, 
                                                 input$waterbirdsInput,
                                                 input$raptorsInput,
                                                 input$shorebirdsInput),]
  })
  
  #create the leaflet basemap with plots drawing tools
  
  ns <- NS("mymap")
  
  lf <- leaflet() %>% 
    addTiles()  %>% 
    addProviderTiles("Esri.WorldTopoMap", group = "WorldTopoMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>%
    
    addPolygons(data = plots.sf, 
                color = "red", 
                fill = NULL,
                fillOpacity = 0.05,
                weight = 1, 
                popup = popupTable(plots.sf, zcol = c("plot", "plot_name"), row.numbers = F, feature.id = F)) %>%
    
    addDrawToolbar(targetGroup='Selected',
                   polylineOptions=FALSE,
                   markerOptions = FALSE,
                   circleOptions = FALSE,
                   circleMarkerOptions = FALSE,
                   polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.1,
                                                                                     color = 'blue',
                                                                                     weight = 3)),
                   rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.1,
                                                                                         color = 'blue',
                                                                                         weight = 3)),
                   editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  
  
  lf <- lf %>%
    addLayersControl(baseGroups = c("WorldTopoMap", "WorldImagery"),
                     overlayGroups = ews.sf$over,
                     position = "topleft")
  
  edits <- callModule(editMod, "mymap", leafmap = lf)
  
  mapzoom <- reactive({
    input[[ns("map_zoom")]]
  })
  
  mapbounds <- reactive({
    input[[ns("map_bounds")]]
  })
  
  #create bounds for different zoom levels. 
  
  z.low <- c(3:11)     #low zoom level
  z.high <- c(12:15)   #high zoom level
  
  
  observeEvent(c(mapzoom(), input$range, input$radio),{
    proxy.lf <- leafletProxy(ns("map"))
    req(mapzoom())
    req(mapbounds())
    
    
    if(mapzoom() %in% z.low) {
      proxy.lf %>%
        clearMarkers() %>%
        addCircleMarkers(data = ews.sf, 
                         fillColor = ~selectedPal(),
                         color = "black",
                         weight = 1, 
                         fillOpacity = 0.95,
                         radius = ~ 2 + 3*(ews.sf$tip), 
                         group = ews.sf$over,
                         clusterOptions = markerClusterOptions(freezeAtZoom = 9, spiderfyDistanceMultiplier = 1.25),
                         popup = popupTable(ews.sf, zcol = c("year", "plot", "species", "survey", "total", "tip"), row.numbers = F, feature.id = F))
    }
    
    else if(mapzoom() %in% z.high) {
      
      mydata.react <- reactive({
        
        bounds <- input[[ns("map_bounds")]]
        
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(filteredSpecies(),
               lat >= latRng[1]  & lat <= latRng[2]  &
                 lon >= lngRng[1]  & lon <= lngRng[2])
      })
      
      observe({
        
        selectedPal.High <- reactive({
          
          if(input$radio=="Plot"){
            pal <- colorFactor(palette = 'Set3',
                               domain = mydata.react()$plot
            )
            return(pal(mydata.react()$plot))
          }
          
          else if(input$radio=="Year"){
            pal <- colorFactor(palette = 'Set3',
                               domain = mydata.react()$year
            )
            return(pal(mydata.react()$year))
          }
        })
        
        req(nrow(mydata.react())>0 & mapzoom() %in% z.high)
        req(input$radio)
        
        proxy.lf %>%
          
          clearMarkers() %>%
          clearMarkerClusters() %>%
          
          addCircleMarkers(data = mydata.react(),
                           fillColor = ~selectedPal.High(),
                           radius = ~ 2 + 3*(mydata.react()$tip),
                           lng = mydata.react()$lon,
                           lat = mydata.react()$lat,
                           fillOpacity = 0.75,
                           color = "black",
                           weight = 1,
                           group = mydata.react()$over,
                           popup = popupTable(mydata.react(), zcol = c("year", "plot", "species", "survey", "total", "tip"), row.numbers = F, feature.id = F))
      })  
    }
  }) 
  
  
  #################################
  ##   create reactive dataset   ##
  #################################
  
  #setup a reactive dataset that filters the data based on the year selection on the slider
  filteredData.Graph <- reactive({
    ews.data.sf[ews.data.sf$year >= input$range[1] & ews.data.sf$year <= input$range[2],]
  }) 
  
  #do the same but for selected species
  filteredSpecies.Graph <- reactive({
    filteredData.Graph()[filteredData.Graph()$species %in% c(input$waterfowlInput, 
                                                             input$mammalsInput, 
                                                             input$waterbirdsInput,
                                                             input$raptorsInput,
                                                             input$shorebirdsInput),]
  })
  
  ews.sum <- reactive({
    
    df <- filteredSpecies.Graph() %>%
      group_by(year,plot) %>%
      dplyr::summarise(tip_sum = sum(tip, na.rm = T))
    
    return(df)
  })
  
  selectedLocations.Plot <- reactive({
    
    req(edits()$finished)   
    
    df <- as.data.frame(st_intersection(edits()$finished, filteredSpecies.Graph()))
    
    df <- dplyr::select(df, year, plot, species, total, tip)
    
    df <- df %>%
      group_by(year) %>%
      dplyr::summarise(tip_sum = sum(tip, na.rm = T)/(length(unique(plot))),
                       # tip_density = tip_sum/(length(unique(plot))),
                       total_birds = sum(total)/(length(unique(plot))))
    
    df
  })
  
  #create a reactive object that changes what data are plotted (TIP vs total birds)
  
  selectedData <- reactive({ 
    
    if(input$dataInput=="TIP"){ 
      selectedLocations.Plot()$tip_sum
    } 
    
    else if(input$dataInput=="Total Birds"){
      selectedLocations.Plot()$total_birds
    }
  })
  
  selectedFiveYear <- reactive({ 
    
    #create an object that captures the range of the last 5 years of survey in the selected area
    fiveYears <- (max(selectedLocations.Plot()$year) - 4) : max(selectedLocations.Plot()$year)
    
    if(input$dataInput=="TIP"){
      
      df <- selectedLocations.Plot()[selectedLocations.Plot()$year %in% fiveYears,]
      return(df$tip_sum)
      
    } 
    
    else if(input$dataInput=="Total Birds"){
      
      df <- selectedLocations.Plot()[selectedLocations.Plot()$year %in% fiveYears,]
      return(df$total_birds)
      
    }
  })
  
  
  output$plot <- renderPlot({
    
    if(nrow(selectedLocations.Plot())==0) {
      print("No data selected, please draw a shape over points.")
    }
    
    else{
      
      p <- ggplot() + 
        
        geom_point(data = selectedLocations.Plot(), 
                   aes(x = as.numeric(year), 
                       y = selectedData()), 
                   colour = "black",
                   fill = "grey",
                   stroke = 1,
                   shape = 21,
                   size = 3) + 
        
        #raw data line
        geom_smooth(data = selectedLocations.Plot(),
                    aes(x = as.numeric(year),
                        y = selectedData(),
                        linetype = "Loess Line",
                        colour = "Loess Line"),
                    se = F,
                    size = 1.25) +
        
        #add the trend line over the selected area
        geom_line(data = selectedLocations.Plot(),
                  aes(x = as.numeric(year),
                      y = mean(selectedData()),
                      linetype = "Mean of Selection",
                      colour = "Mean of Selection"),
                  alpha = 0.75,
                  size = 1.25) +
        
        #add the trend line for the 5 year average
        geom_line(data = selectedLocations.Plot(),
                  aes(x = as.numeric(year),
                      y = mean(selectedFiveYear()),
                      linetype = "5-Yr Average",
                      colour = "5-Yr Average"),
                  alpha = 0.75,
                  size = 1.25) +
        
        #this is broken for total birds - only works on TIP
        geom_line(data =  ews.sum(),
                  aes(x = as.numeric(year),
                      y = mean(ews.sum()$tip_sum, na.rm = T),
                      linetype = "Entire Survey",
                      colour = "Entire Survey"),
                  alpha = 0.75,
                  size = 1.25) +
        
        #things will be assigned alphabetically based on the labels!
        scale_linetype_manual(name = "Trend Lines",
                              values = c(2,3,1,2),
                              labels = c("5-Yr Average", "Entire Survey", "Loess Line", "Mean of Selection")) +
        
        #things will be assigned alphabetically based on the labels!
        scale_colour_manual(name = "Trend Lines",
                            values = c("blue", "black", "black", "grey"),
                            labels = c("5-Yr Average", "Entire Survey", "Loess Line", "Mean of Selection")) +
        
        theme( 
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
          text = element_text(size = 15)) + 
        
        guides(guide_legend(title = "Trend Lines"),
               colour = guide_legend(keywidth = 3, keyheight = 1, nrow = 2),
               linetype = guide_legend(keywidth = 3, keyheight = 1, nrow = 2)) +
        
        scale_x_continuous() +
        scale_y_continuous(limits = c(0, max(selectedData()))) +
        xlab("Survey Year") +
        ylab(paste(as.character(input$dataInput), "per 25km^2", sep = " "))
      
      p
      
    }
    
  })
  
  # Make the download button only appear when data is selected.
  output$downloadDataButton <- renderUI({
    if(!is.null(selectedLocations.Plot())) {
      downloadButton("downloadData", "Download.csv")
    }
  })
  
  # Downloadable csv of selected reactive dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("EWS_NL_Data_", Sys.Date(),".csv",sep="")
    },
    content = function(file) {
      write.csv(selectedLocations.Plot(), file, row.names = FALSE)
    })
  
  output$text <- renderUI({
    HTML(paste("<h4> This is a Shiny application to interactively explore Eastern Waterfowl Survey data in Newfoundland and Labrador. </h4> <hr>",
               "The raw data are grouped in to clusters for each plot. These clusters can be expanded by clicking on them. To see the raw data plotted on the map, zoom in on a particular plot. <br/>",
               "Species can be selected by using the dropdown menus on the panel on the left. The map and summary data will be automatically updated based on what species are selected / deselected <br/>",
               "The map will still plot the last species you have selected, even if there is nothing currently selected. This is a bug that I'm trying to fix. <br/>",
               "You can draw either a rectagle or a polygon across an area containing points to receive a summary plot showing trends in either total indicated pairs (TIP) or total birds per 25km^2. <br/>", 
               "You may draw multiple shapes across areas and the summary data will update automatically. To remove the shapes, click the <b> Delete </b> button (the garbage can), then click on the shape(s) and click <b> Save </b>. <br/>",
               "You can toggle between the 'WorldTopoMap' and 'WorldImagery' using the tabs on the left panel <br/>",
               "You may download the summarized data you have selected in a .csv by clicking the <b> Download .csv </b> button. <br/>",
               "Click the <b> Map </b> tab at the bottom to get started.",
               "Species list is as follows: <br/>", 
               "<br/>",
               sep = "<br/>"),
         as.list(unique(ews.sf$species)))
  })
  
}

shinyApp(ui, server)
