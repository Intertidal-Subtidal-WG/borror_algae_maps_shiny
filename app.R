#'---------------------------------------------
#' Shiny App for Notes from the Past Show How Local 
#' Variability can Stymie Urchins and the Rise of the Reds in the Gulf of Maine
#' 
#' @author Jarrett Byrnes
#' See about section of app for more info
#'---------------------------------------------

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(glue)
setwd(here::here())

if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(ricv)) remotes::install_github("xvrdm/ricv")

# The Data
#load("spp_shapes_cleaned.Rds")
load("island_perimeter_segments.rds")
shapes_cleaned <- st_as_sf(#shapes_cleaned,
  df %>% ungroup(),
  crs = 4326) %>%
  mutate(species_general = gsub("Saccharina", 
                                "Saccharina latissima", 
                                species_general),
         species_general = gsub("Kelps", 
                                "kelps", 
                                species_general),
  )


years <- min(shapes_cleaned$year):max(shapes_cleaned$year)
years[!(years %in% unique(shapes_cleaned$year))] <- NA
init_year <- min(shapes_cleaned$year)


# add dominant species layer
kelp_sp <- c( "Laminaria digitata",    
              "Saccharina latissima",   
              "Rope kelps",
              "Alaria esculenta",
              "Saccorhiza dermatodea")


kelps_reds_barrens <- shapes_cleaned %>%
  filter(!(species_general %in% c("Codium fragile" ))) %>%
  as_tibble() %>%
  mutate(geometry = st_as_text(geometry)) %>%
  group_by(year, geometry) %>%
  summarize(has_urchin = ("Urchin barrens" %in% species_general),
            has_reds = ("Mixed reds" %in% species_general),
            has_kelp = (sum(kelp_sp %in% species_general))>0) %>%
  ungroup() %>%
  mutate(dominant_cover = case_when(
    (has_urchin) ~ "Urchin Barren",
    (has_reds & has_kelp) ~ "Mixed Kelp and Reds",
    (has_reds & !has_kelp) ~ "Mixed Reds",
    (!has_reds & has_kelp) ~ "Kelp"),
    geometry = st_as_sfc(geometry, crs=4326)
  ) %>%
  st_as_sf(crs=4326)


# A Basic Map and some extra info (like colors!)
basemap <- leaflet() %>% 
  # addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(-70.620789,42.983952,-70.608625,42.99337) 

#some color
source("color_pal.R")

shapes_cleaned <- left_join(shapes_cleaned, colors) 

algae_pal <- colorFactor(palette = colors$color, 
                         ordered = TRUE,
                         levels = colors$species_general)

group_pal <- colorFactor(c("darkgreen", "darkmagenta", "red", "grey"),
                         levels = sort(unique(kelps_reds_barrens$dominant_cover)))

# Define UI for application 
ui <- navbarPage(
  title  = "Maps of Subtidal Algal Canopy Over Time at SML",
  theme = shinytheme("journal"),
  tabPanel("All Species",
           fluidRow(column(12, 
                           leafletOutput("mymap", height = "1000"),
                           
                           absolutePanel(id = "controls", class = "panel panel-default",
                                         top = 90, left = 65, width = 250, fixed=TRUE,
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
           ))),
  
  tabPanel("Taxonomically Grouped",
           fluidRow(column(12, 
                           leafletOutput("group_mymap", height = "1000"),
                           
                           absolutePanel(id = "controls", class = "panel panel-default",
                                         top = 90, left = 65, width = 250, fixed=TRUE,
                                         draggable = TRUE, height = "auto",
                                         
                                         checkboxGroupInput("group_input",
                                                            label = h5("Select species shown"),
                                                            choices = sort(unique(kelps_reds_barrens$dominant_cover))
                                         ),
                                         
                                         sliderTextInput("group_plot_date",
                                                         label = h5("Select year"),
                                                         choices = sort(unique(kelps_reds_barrens$year)),
                                                         selected = init_year,
                                                         grid = FALSE,
                                                         animate=animationOptions(interval = 3000, loop = TRUE)))
           ))),
  
  # #ricv did not work....
  #   # let's do some year-to year image comparison
  #         tabPanel("Compare Years",
  #              sidebarLayout(
  #                sidebarPanel(width=2,
  #                             radioButtons(inputId = "start_year",
  #                                          "Start Year",
  #                                          choices = years[!is.na(years)],
  #                                          selected = 1982),
  # 
  #                             radioButtons(inputId = "end_year",
  #                                          "End Year",
  #                                          choices = years[!is.na(years)],
  #                                          selected = 2014)
  #                             ),
  #                mainPanel(
  #                  ricvOutput("compare_years")
  #                )
  #              )
  #         )
  tabPanel("About",
           HTML("These are digital renderings of maps of the dominant subtidal
                  habitat structure in the shallow subtidal (1-5m depth) around Appledore
                  Islands in the Isles of Shoals, Maine, USA. These renderings were derived
                  from surveys by Dr. Art Borror (1982-1990) and later Drs. Jim Coyer and Jarrett 
                  Byrnes (2014) while at the Shoals Marine Lab by taking a boat around the island 
                  at low tide and visually
                  examining the habitats through a bathyscope. <br><br> The maps were
                  georeferenced and digitized by Kate Sheridan, Andrea Brown, and Tianna 
                  Peller. Jake Lawlor and Jarrett Byrnes then took the resulting shapefiles and
                  cropped and aggregated the information into the maps shown here. For any
                  additional questions contact <a href=mailto:jarrett.byrnes@umb.edu>Jarrett Byrnes</a><br><br>
                  <!--
                  <center><img src='logos/ciee.jpg'>
                  <img src='logos/Isle_of_Shoals_Marine_Logo.jpg'>
                  <img src='logos/neracoos.png'>
                  </center> -->"))
)

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
        addPolylines(data = shapes_dat(),
                     fillOpacity = 0.2,
                     color = ~algae_pal(species_general))
    },
    ignoreInit = TRUE)
  
  
  ## for grouped ==================
  #make the map
  output$group_mymap <- renderLeaflet({ 
    basemap
  })
  
  
  #filter the data
  kelps_reds_barrens_dat <- reactive({
    if(is.null(input$group_plot_date)) 
      input$group_plot_date <- init_year
    
    kelps_reds_barrens %>%
      filter(dominant_cover %in% input$group_input) %>%
      filter(year == input$group_plot_date)
  })
  
  #the update
  observeEvent({input$group_plot_date 
    input$group_input}, {
      leafletProxy("group_mymap") %>%
        clearShapes() %>%
        addPolylines(data = kelps_reds_barrens_dat(),
                     fillOpacity = 0.2,
                     color = ~group_pal(dominant_cover))
    },
    ignoreInit = TRUE)
  
  
  # comparing years
  output$compare_years <- renderRicv(
    ricv(
      img1 = glue("figures/{input$start_year}.jpg"),
      img2 = glue("figures/{input$end_year}.jpg"),
      options = list(showLabels = FALSE),
      css = list(both = "padding: 40px;", 
                 before = "font-size: 2rem;", 
                 after = "font-size: 4rem; font-family: serif;")
    )
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
