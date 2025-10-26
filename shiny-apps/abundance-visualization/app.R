# add packages
library(sf)
library(shiny)
library(shiny.fluent)
library(bslib)
library(leaflet)
library(dplyr)
library(ggplot2)
library(leafpop)
library(tidyr)
library(DT)
library(maps)

# Load appendix data and eco-regions
s2 <- read.csv("data/appendixS2.csv")
s3 <- read.csv("data/appendixS3.csv")
s4 <- read.csv("data/appendixS4.csv")

eco <- read_sf("data/ecoshape/us_eco_l3_state_boundaries.shp") %>%
  st_transform(crs = 4326) %>%
  st_make_valid() %>%
  unite(NA_L3KEY, NA_L3CODE, NA_L3NAME, sep = "  ")


# pull out data from eastern temperate forests and northern forests
s2_forests <- s2 %>%
  filter(NA_L1KEY == "8  EASTERN TEMPERATE FORESTS" | NA_L1KEY == "5  NORTHERN FORESTS")

# pull out data that contains "abundant" in its commonness column
s2_abun <- s2_forests %>%
  filter(grepl("Abundant", commonness))

# pull out species names from filtered data
final_unique <- data.frame(unique(s2_abun$Orig.Genus.species))

# join s3 and s4 for to plot ecoregions for map1
s7 <- merge(s3, s4, by = "USDA.Genus.species")

# create df of just unique ecoregion/ species combinations for list attribiute
L3_list <- data.frame(s7$USDA.Genus.species, s7$NA_L3KEY)
L3_list <- data.frame(
  Species = s7$USDA.Genus.species,
  NA_L3KEY = s7$NA_L3KEY
)

# Define UI for application
ui <- page_fillable(

  # Application title
  titlePanel("Abundance Mockup"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # coordinate input for map1 ecoregions
      card(
        card_header("Set Coordinates"),
        numericInput("lon", "Longitude", value = -72, step = 0.1),
        numericInput("lat", "Latitude", value = 42, step = 0.1),
        actionButton("go", "Find Ecoregion")
      ),

      # speices dropdown for map2
      card(
        card_header("Select a Species Below"),
        height = 275,
        selectizeInput("select",
          "Select Species Below",
          choices = NULL,
          multiple = FALSE
        )
      )
    ),
    mainPanel(
      # Show a map of the US with ecoregion associated with input coords
      card(
        leafletOutput("map1")
      ),
      # display download buttons
      card(
        h3("Abundant Invasive Plants in Ecoregion:"),
        div(
          style = "height: 300px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
          tableOutput(outputId = "list")
        ),
        fluidRow(
          column(6, downloadButton("download_csv", "Download CSV")),
          column(6, downloadButton("download_pdf", "Download PDF"))
        ),
        verbatimTextOutput("species_list")
      ),

      # Show a map of the US with selected species points
      card(
        leafletOutput("map2")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  selected_region <- reactiveVal(NULL)

  point <- reactiveVal(NULL)
  region <- reactiveVal(NULL)

  observeEvent(input$go, {
    req(input$lat, input$lon)

    # Create a point from input coordinates
    pt <- st_sfc(st_point(c(input$lon, input$lat)), crs = 4326)
    point(pt)

    # Find which ecoregion the point falls into
    region_found <- eco[st_intersects(eco, pt, sparse = FALSE), ]

    if (nrow(region_found) > 0) {
      region(region_found)
    } else {
      region(NULL)
    }
  })

  # filter selected species for map2
  observe({
    updateSelectizeInput(
      session,
      "select",
      choices = as.character(final_unique[[1]]),
      selected = ' ',
      server = TRUE
    )
  })
  
  filtered_data <- reactive({
    req(input$select)
    s2 %>%
      filter(Orig.Genus.species == input$select)  # Changed from USDA.Genus.species to Orig.Genus.species
  })

  # render map1
  output$map1 <- renderLeaflet({
    if (is.null(point())) {
      # Default map like map2 when app loads
      leaflet() %>%
        addTiles() %>%
        setView(lng = -96.6638, lat = 39.7177, zoom = 4)
    } else {
      leaflet() %>%
        addTiles() %>%
        addMarkers(lng = input$lon, lat = input$lat, popup = "Your Point") %>%
        {
          if (!is.null(region())) {
            addPolygons(.,
                        data = region(), color = "blue", weight = 2,
                        fillOpacity = 0.3,
                        popup = ~ paste("Ecoregion:", NA_L3KEY)
            )
          } else {
            .
          }
        }
    }
  })

  output$region_info <- renderPrint({
    if (is.null(region())) {
      "No ecoregion found at this location."
    } else {
      as.data.frame(region())[c("US_L3NAME")]
    }
  })

  # render species list
  output$list <- renderTable({
    req(region())
    region_name <- as.character(region()$NA_L3KEY[1]) # Take first region name as character
    L3_list %>%
      filter(NA_L3KEY == region_name)
  })

  # render map2
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -96.6638, lat = 39.7177, zoom = 4)
  })

  # update map1 with associated ecoregion
  observe({
    leafletProxy("map1", data = region)
  })

  # update map2 with selected species points
  observe({
    data <- filtered_data()
    req(nrow(data) > 0)  # Only proceed if there's data
    
    leafletProxy("map2") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = data,
        lng = ~x, lat = ~y,
        popup = ~ paste(
          "<strong> Dataset: </strong>", Dataset, "<br>",
          "<strong> Year: </strong>", Year, "<br>",
          "<strong> Qualitative: </strong>", Qualitative, "<br>",
          "<strong> Percent Cover: </strong>", PctCov, "<br>",
          "<strong> Cover Class: </strong>", AvgCovClass, "<br>"
        ),
        radius = 2
      )
  })
  
  # download button functionality
  # Reactive data for download
  species_data <- reactive({
    req(region())
    region_name <- as.character(region()$NA_L3KEY[1])
    L3_list %>%
      filter(NA_L3KEY == region_name)
  })
  
  # CSV download
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("species_list_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(species_data(), file, row.names = FALSE)
    }
  )
  
  # PDF download
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("species_list_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Use a simple rmarkdown approach for PDF
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(data = species_data())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
