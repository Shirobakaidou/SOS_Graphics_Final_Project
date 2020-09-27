library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal)
library(dplyr)
library(raster)
library(ggplot2)
library(ggpubr)

setwd("C:/EAGLE/SOS/Graphics/final_project/SOS_Graphics_Final_Project")


# Get Shape Files of marine protected area (MPA) and eez bounding box of South Afica
mpa <- readOGR("./south_africa_MPA/south_africa_MPA.shp")
eez <- readOGR("./EEZ_bounding_box/SA_EEZ_BoundingBox.shp")

# Get File containing paths to raster layers of annaual fishing activity of every year and gear type
raster_paths <- read.csv("./raster_paths.csv")
str(raster_paths)
#raster_paths$year <- as.factor(raster_paths$year)
#raster_paths$fishing_type <- as.factor(raster_paths$fishing_type)

# Get File of Statistics of the fishing activity
fishery_stat <- read.csv("./fishery_stat.csv")
fishery_stat$X <- NULL

# Create unique IDs for each MPA Polygon
mpa$uid <- paste0("p", 1:length(mpa))

# Fix the width and size to the windows size
jscode <- '
$(document).on("shiny:connected", function(e) {
var jsHeight = window.innerHeight;
Shiny.onInputChange("GetScreenHeight",jsHeight);
});
'
tags$script(jscode)

# Function to achieve: 1."roll-down/up" animation with sidebar menu item; 2.open the wanted tab.
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}


# User Interface
ui <- dashboardPage(
  
  dashboardHeader(title = "Fishing Activity in South Africa, 2012-2016",
                  titleWidth = 450
                  #disable = TRUE
  ),
  
  dashboardSidebar(
    sidebarMenu(
      convertMenuItem(menuItem(text = "Map View", tabName = "map_view",
                               selectInput("year_map","Select a year", selected = "2012", choices = c("2012","2013","2014","2015","2016")),
                               selectInput("fishing_type_map", "Select a Fishing Type", choices = c("Drifting Longlines", "Fixed Gear", "Purse Seines", "Squid Jigger", "Trawler", "Others", "All Fishing Types"))),
                      "map_view"),
      
      convertMenuItem(menuItem(text = "Table View", tabName = "table_view",
                               selectInput("year_table","Select a year", selected = "2012", choices = c("-","2012","2013","2014","2015","2016")),
                               selectInput("fishing_type_table", "Select a Fishing Type", choices = c("-","Drifting Longlines", "Fixed Gear", "Purse Seines", "Squid Jigger", "Trawler", "Others", "All Fishing Types")),
                               downloadButton('downloadData', "Download")),
                      "table_view"),
      
      menuItem(text = "Plot View", tabName = "plot_view",
               convertMenuItem(menuItem(text = 'Overall Statistics', tabName = "overall_stat",
                                        selectInput("dropdown_overallstat", 'Select a Fishing Type', choices = c("Drifting Longlines", "Fixed Gear", "Purse Seines", "Squid Jigger", "Trawler", "Others", "All Fishing Types"))),
                               "overall_stat"),
               
               menuItem('Zonal Statistics', tabName = 'zonal_stat'))
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "map_view",
              tags$script(jscode), 
              uiOutput("leafl")),
      
      tabItem(tabName = "table_view",
              tableOutput("fishery_stat_sub")),
      
      tabItem(tabName = "overall_stat",
              fluidRow(
                splitLayout(cellWidths = c("48.5%", "48.5%"), 
                            plotOutput("p1"), plotOutput("p2")),
                br(),br(),br(),
                splitLayout(cellWidths = c("48.5%", "48.5%"), 
                            plotOutput("p3"), plotOutput("p4"))
              )),
      tabItem(tabName = "zonal_stat",
              fluidRow(
                column(width = 6,
                       box(
                         title = "MPA Map", width = NULL,
                         leafletOutput("statmap", height=600)
                       )),
                column(width = 6,
                       box(
                         title = "MPA Zonal Statistical Plots", width = NULL,
                         textOutput("stathint"),
                         plotOutput("statplot", height = 600)
                       ))
              )
      )
    )
  ))


# Server
server <- function(input, output, session){
  
  ##############
  ## Map View ##
  ##############
  
  # Reactive function to select required raster scene based on user selection
  sel_year <- reactive({
    subset_year <- subset(raster_paths, year == input$year_map)
  })
  
  sel_type <- reactive({
    subset_type <- subset(sel_year(), fishing_type == input$fishing_type_map)
  })
  
  
  # Render Map View
  output$fishing_map <- renderLeaflet({
    
    # Load required raster scene
    raster_sub <- sel_type()
    r <- raster(as.character(raster_sub$raster_paths))
    
    # Render Leaflet Map
    pal <- colorNumeric(palette = 'YlOrRd', domain = values(r), na.color = "transparent")
    binpal <- colorBin(palette = 'YlOrRd', domain = values(r), na.color = "transparent", 10, pretty = TRUE)
    leaflet() %>%
      setView(lat = -34.1174, lng = 22.2806, zoom = 5) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      
      addPolygons(data = eez, color = "grey", weight = 1.5, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.3, fillColor = "transparent", popup = eez$geoname) %>%
      addPolygons(data = mpa, color = "orange", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.3, fillColor = "orange",# popup = mpa$NAME)%>%
                  highlightOptions = highlightOptions(color="white", weight=2, bringToFront = TRUE), label = mpa$NAME) %>%
      
      addRasterImage(r, colors = binpal, opacity = 0.85) %>%
      addLegend(pal = binpal, position = "bottomright", values = values(r), title = "Fishing Hours")
  })
  
  
  # Render Leaflet map fitting the screen size
  output$leafl <- renderUI({
    if(!is.null(input$GetScreenHeight)){
      width  <- session$clientData$output_image1_width
      print(session$clientData)
      height <- session$clientData$output_image1_height
      leafletOutput("fishing_map", width = "100%", height = input$GetScreenHeight)
    }
  })
  
  
  
  ################
  ## Table View ##
  ################
  
  # Reactive Function for subsetting the table of statistics based on user selection
  data <- reactive(if (input$fishing_type_table == "-" & input$year_table == "-"){
    fishery_stat
  }
  else if (input$fishing_type_table == "-"){
    subset(fishery_stat, year == input$year_table)
  }
  else if (input$year_table == "-"){
    subset(fishery_stat, fishing_type == input$fishing_type_table)
  }
  else{
    subset(fishery_stat, fishing_type == input$fishing_type_table & year == input$year_table)
  }
  ) 
  
  # Render Table
  output$fishery_stat_sub <- renderTable(data())
  
  # Render Download Function
  output$downloadData <- downloadHandler(
    filename = "Dataframe.csv",
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  ###############
  ## Plot View ##
  ###############
  
  ########################
  ## Overall Statistics ##
  ########################
  
  # Render Bar Plots of fishing statistics based on user selection
  output$p1 <- renderPlot({
    
    # Set Theme
    theme_set(theme_light())
    theme_update(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
    df_1 <- subset(fishery_stat, variables == "totaltime" & fishing_type == input$dropdown_overallstat)
    ggplot(data = df_1, aes(x = year, y = data)) +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.9) + 
      labs(title = paste("Total Fishing Effort of", input$dropdown_overallstat, "in the EEZ")) +
      xlab("Years") + ylab("Fishing hours")
  })
  
  output$p2 <- renderPlot({
    df_2 <- subset(fishery_stat, variables == "mpatime" & fishing_type == input$dropdown_overallstat)
    ggplot(data = df_2, aes(x = year, y = data)) +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.9) + 
      labs(title = paste("Total Fishing Effort of", input$dropdown_overallstat, "in the MPAs")) +
      xlab("Years") + ylab("Fishing hours")
  })
  
  output$p3 <- renderPlot({
    df_3 <- subset(fishery_stat, variables == "percmpatime" & fishing_type == input$dropdown_overallstat)
    ggplot(data = df_3, aes(x = year, y = data, )) + 
      labs(title = paste("Share of MPA Fishing Effort of", input$dropdown_overallstat, "\nin Total Effort in the EEZ")) +
      xlab("Years") + ylab("Percentile Share") +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.9)
  })
  
  output$p4 <- renderPlot({
    df_4 <- subset(fishery_stat, variables == "percmpaarea" & fishing_type == input$dropdown_overallstat)
    ggplot(data = df_4, aes(x = year, y = data, )) + 
      labs(title = paste("Share of MPA Area with Fishing Effort of", input$dropdown_overallstat, "\nin Total MPA Area")) +
      xlab("Years") + ylab("Percentile Share") +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.9)
  })
  
  
  ######################
  ## Zonal Statistics ##
  ######################
  
  # Render Leaflet Map with all MAP displayed
  output$statmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(data = mpa, 
                  color = "orange", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.3, fillColor = "orange", # popup = mpa$NAME)%>%
                  highlightOptions = highlightOptions(color="white", weight=2, bringToFront = TRUE), label = mpa$NAME, #) %>%
                  layerId = ~uid)
  })
  
  
  # Event: the polygon is highlighted red when it's clicked on by users
  observeEvent(input$statmap_shape_click, {
    
    event <- input$statmap_shape_click
    
    # Pull lon and lat from Click Event
    lat <- event$lat
    lon <- event$lng
    
    # Put lat&lon of this point into data frame
    coords <- as.data.frame(cbind(lon,lat))
    
    # Convert data frame into SP object
    point <- SpatialPoints(coords)
    proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    # Subset MPA polygon in which the click point resides
    selected <- mpa[point,]
    proj4string(selected) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    # Create proxy layer on the leaflet map
    proxy <- leafletProxy("statmap")
    
    if(event$id == "Selected"){
      proxy %>% removeShape(layerId = "Selected")
    } else {
      proxy %>% addPolygons(data = selected,
                            fillColor = "red",
                            fillOpacity = 0.8,
                            color = "black",
                            weight = 2,
                            stroke = T,
                            layerId = "Selected")
    }
  })
  
  
  # Event: Plot zonal statistics of a selected polygon
  observe({
    
    event <- input$statmap_shape_click
    
    # Extract the selected MPA
    mpa_sub <- mpa[mpa$uid == event$id, ]
    
    # if no polygon is selected, show a text message
    if (is.null(event$id)){
      
      output$stathint <- renderText(expr = "Select a MPA of your interest")
      
    }else{output$statplot <- renderPlot({
      
      # Subset the file containing paths of raster layers, 
      # only keep the rasters representing "All Fishing Types".  
      raster_all_gear <- raster_paths[raster_paths$fishing_type=="All Fishing Types",]
      
      col_total_time <- c()
      col_perc_fishing_area <- c()
      
      for (i in 1:length(raster_all_gear)){
        
        # Crop raster by selected MPA
        raster_extract <- extract(raster(as.character(raster_all_gear$raster_paths[i])), mpa_sub)
        raster_unlist <- raster_extract %>% unlist()
        
        # Sum of fishing hours in the specified MPA
        total_time <- raster_unlist %>% na.omit() %>% sum()
        col_total_time[i] <- total_time
        
        # Percentage of area under fishing in the specified MPA
        len <- length(raster_unlist)
        len_fish <- sum(!is.na(raster_unlist))
        perc_fish_area <- round((len_fish/len)*100, digits = 2)
        col_perc_fishing_area[i] <- perc_fish_area
      }
      
      # Bind the zonal statistical df into "data"-slot of the selected MPA
      mpa_sub@data <- structure(list(mpa_sub@data,
                                     zonal_stat = structure(list(year = raster_all_gear$year,
                                                                 total_fishing_hour = col_total_time,
                                                                 perc_fish_area = col_perc_fishing_area),
                                                            .Names = c("year", "total_fishing_hour", "perc_fish_area"),
                                                            class = "data.frame",
                                                            row.names = c(NA, 5L))),
                                .Name = c("mpa_df_sub", "zonal_stat"),
                                class = c("data.frame"),
                                row.names = c(NA, 1L))
      
      # Plot
      p1 <- ggplot(data = mpa_sub@data[[2]], aes(x = year, y = total_fishing_hour)) +
        geom_bar(stat = "identity", fill = "blue", alpha = 0.9) + 
        labs(title = paste0("Annual Total Fishing Hours in \n", mpa_sub@data[[1]]$NAME)) +
        xlab("Years") + ylab("Fishing hours")
      
      p2 <- ggplot(data = mpa_sub@data[[2]], aes(x = year, y = perc_fish_area)) +
        geom_bar(stat = "identity", fill = "blue", alpha = 0.9) + 
        labs(title = paste0("How many percent of area in \n", mpa_sub@data[[1]]$NAME, "\n are under fishery?")) +
        xlab("Years") + ylab("Percentage (%)")
      
      plot <- ggarrange(p1, p2, ncol = 1, nrow = 2)
      plot
    })}
  })
}


shinyApp(ui, server)