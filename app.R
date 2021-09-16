#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")

# The Data
load("spp_shapes_cleaned.Rds")
shapes_cleaned <- st_as_sf(shapes_cleaned,
                           crs = 4326)

years <- min(shapes_cleaned$year):max(shapes_cleaned$year)
years[!(years %in% unique(shapes_cleaned$year))] <- NA
init_year <- min(shapes_cleaned$year)

# A Basic Map and some extra info (like colors!)
basemap <- leaflet() %>% 
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(-70.620789,42.983952,-70.608625,42.99337) 

#some color
algae_pal <- colorFactor("viridis", 
                         domain = shapes_cleaned$species_general)


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Maps of Subtidal Algal Canopy Over Time at SML", 
               windowTitle = "Maps of Subtidal Algal Canopy Over Time at SML"),
    fluidRow(column(12, 
        leafletOutput("mymap", height = "1000"),
        
        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 75, left = 65, width = 250, fixed=TRUE,
                      draggable = TRUE, height = "auto",
                      
                      checkboxGroupInput("species_input",
                                         label = h5("Select species shown"),
                                         choices = sort(unique(shapes_cleaned$species_general))
                      ),
                      
                      sliderTextInput("plot_date",
                                      label = h5("Select year"),
                                      choices = sort(unique(shapes_cleaned$year)),
                                      selected = init_year,
                                      grid = FALSE,
                                      animate=animationOptions(interval = 3000, loop = TRUE)))
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #filter the data
    shapes_dat <- reactive({
        if(is.null(input$plot_date)) input$plot_date <- init_year
        
        shapes_cleaned %>%
            filter(species_general %in% input$species_input) %>%
            filter(year == input$plot_date)
    })

    #make the map
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    #the update
    observeEvent({input$plot_date 
        input$species_input}, {
        leafletProxy("mymap") %>%
            clearShapes() %>%
            clearControls() %>%
            addPolygons(data = shapes_dat(),
                        fillOpacity = 0.2,
                        color = ~algae_pal(shapes_dat()$species_general)) %>%
                addLegend("topright", 
                          pal = algae_pal, 
                          values = shapes_dat()$species_general,
                          title = "Canopy Species",
                          opacity = 1
                )
    },
    ignoreInit = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
