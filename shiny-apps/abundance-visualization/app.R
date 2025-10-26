# add packages
library(sf)
library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(tidyr)
library(DT)
library(maps)
# Ensure rmarkdown is available for PDF download
library(rmarkdown)

# Load appendix data and eco-regions
s2 <- read.csv("data/appendixS2.csv")
s3 <- read.csv("data/appendixS3.csv")
s4 <- read.csv("data/appendixS4.csv")

# Load ecoregions and prepare them for spatial operations
eco <- read_sf("data/ecoshape/us_eco_l3_state_boundaries.shp") %>%
  st_transform(crs = 4326) %>%
  st_make_valid() %>%
  unite(NA_L3KEY, NA_L3CODE, NA_L3NAME, sep = "  ", remove = FALSE)

# pull out data from eastern temperate forests and northern forests
s2_forests <- s2 %>%
  filter(NA_L1KEY == "8  EASTERN TEMPERATE FORESTS" | NA_L1KEY == "5  NORTHERN FORESTS")

# pull out data that contains "abundant" in its commonness column
s2_abun <- s2_forests %>%
  filter(grepl("Abundant", commonness))

# pull out species names from filtered data
final_unique <- data.frame(unique(s2_abun$Orig.Genus.species))

# join s3 and s4 for to plot ecoregions
s7 <- merge(s3, s4, by = "USDA.Genus.species")

# create df of just unique ecoregion/ species combinations for list attribiute
L3_list <- data.frame(
  Species = s7$USDA.Genus.species,
  NA_L3KEY = s7$NA_L3KEY
)

# -----------------------------------------------------------------
# NEW UI (User Interface)
# -----------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    # Link to the external CSS file (must be in 'www' folder)
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    # Link to Font Awesome for icons
    tags$link(
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css",
      rel = "stylesheet"
    )
  ),
  
  useShinyjs(),
  
  # 1. App Header
  div(class = "app-header",
      div(class = "header-container",
          h1(tags$i(class = "fas fa-leaf"), " Plant Abundance & Ecoregion Viewer")
      )
  ),
  
  # 2. Main Content Area
  div(class = "main-container",
      
      # Main layout with Sidebar and Main Panel
      div(class = "main-layout",
          
          # 2a. Sidebar for filters
          div(class = "sidebar",
              div(class = "filter-card",
                  div(class = "filter-header",
                      h5(tags$i(class = "fas fa-filter"), " Filter Options")
                  ),
                  div(class = "filter-body",
                      
                      # Ecoregion via Lat/Lon Input (RESTORED)
                      div(class = "form-group",
                          tags$label(
                              tags$i(class = "fas fa-map-marker-alt"), " Find Ecoregion by Coordinates:"
                          ),
                          numericInput("lon", "Longitude", value = -72, step = 0.1, width = "100%"),
                          numericInput("lat", "Latitude", value = 42, step = 0.1, width = "100%"),
                          actionButton(
                                                    "go", 
                                                    "Find Ecoregion", # label
                                                    icon = icon("search"), # icon argument with icon() wrapper
                                                    class = "btn-custom-small", 
                                                    style = "margin-top: 5px; margin-bottom: 10px;"
                                                )                      ),
                      
                      # Species Dropdown (MAINTAINED)
                      div(class = "form-group",
                          tags$label(`for` = "selected_species",
                                     tags$i(class = "fas fa-seedling"), " Choose a Species:"
                          ),
                          selectizeInput("selected_species", label = NULL,
                                      choices = NULL, # Populated by server
                                      options = list(placeholder = 'Type to search...'),
                                      width = "100%")
                      ),
                      
                      actionButton("reset_btn", "Reset Map & Filters",
                                   icon = icon("refresh"), class = "btn-custom")
                  )
              )
          ),
          
          # 2b. Main Panel for Map and Table
          div(class = "main-panel",
              # Map Card
              div(class = "map-card",
                  div(class = "map-header",
                      h5(tags$i(class = "fas fa-map-location-dot"), " Ecoregion & Species Map")
                  ),
                  div(class = "map-body",
                      leafletOutput("map", height = 600)
                  )
              ),
              
              # Results Table Card
              div(class = "results-card",
                  div(class = "results-header",
                      # Dynamic title
                      uiOutput("results_title", inline = TRUE)
                  ),
                  div(class = "results-body",
                      div(class = "table-container",
                          tableOutput("list")
                      ),
                      hr(),
                      fluidRow(
                        column(6, downloadButton("download_csv", "Download CSV", class = "btn-custom-small")),
                        column(6, downloadButton("download_pdf", "Download PDF", class = "btn-custom-small"))
                      )
                  )
              )
          )
      )
  ),
  
  # 3. App Footer
  tags$footer(class = "app-footer",
              div(class = "footer-container",
                  p("Abundance Mockup")
              )
  )
)


# -----------------------------------------------------------------
# SERVER LOGIC (Restored Lat/Lon functionality)
# -----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive values to hold the point and region (RESTORED)
  point <- reactiveVal(NULL)
  region <- reactiveVal(NULL)
  
  # Populate species dropdown
  observe({
    updateSelectizeInput(
      session,
      "selected_species",
      choices = c("None" = "", as.character(final_unique[[1]])),
      selected = '',
      server = TRUE
    )
  })
  
  # Action observer for coordinates (RESTORED)
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
  
  # Reset button (UPDATED)
  observeEvent(input$reset_btn, {
    # Reset coordinates and triggers
    updateNumericInput(session, "lon", value = -98.5)
    updateNumericInput(session, "lat", value = 39.8)
    point(NULL)
    region(NULL)
    
    # Reset species filter
    updateSelectizeInput(session, "selected_species", selected = "")
    
    # Reset map view
    leafletProxy("map") %>%
      setView(lng = -96.6638, lat = 39.7177, zoom = 4)
  })
  
  # -- REACTIVES --
  
  # Reactive for selected ecoregion polygon (Uses the reactive value region())
  selected_ecoregion_data <- reactive({
    region()
  })
  
  # Reactive for selected species points
  filtered_species_data <- reactive({
    # Allow filtering by species without an ecoregion, but default to NULL if "None"
    req(input$selected_species != "") 
    s2 %>%
      filter(Orig.Genus.species == input$selected_species)
  })
  
  # Reactive for species list table (Uses the reactive value region())
  species_list_data <- reactive({
    req(region()) # Require a region to proceed
    region_name <- as.character(region()$NA_L3KEY[1])
    L3_list %>%
      filter(NA_L3KEY == region_name)
  })
  
  # -- OUTPUTS --
  
  # Base Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -96.6638, lat = 39.7177, zoom = 4)
  })
  
  # Dynamic Results Card Title (UPDATED)
  output$results_title <- renderUI({
    if (!is.null(region())) {
      region_name <- as.character(region()$NA_L3NAME[1])
      h5(tags$i(class = "fas fa-list"), "Abundant Species in:", tags$br(), region_name)
    } else {
      h5(tags$i(class = "fas fa-list"), "Abundant Species (Enter coordinates above)")
    }
  })
  
  # Species List Table (UPDATED to use region())
  output$list <- renderTable({
    if (is.null(region())) {
      return(data.frame(Message = "Enter coordinates and click 'Find Ecoregion' to see the species list."))
    }
    data <- species_list_data()
    if (nrow(data) == 0) {
      return(data.frame(Message = "No abundant species found for this ecoregion in the dataset."))
    }
    data
  }, colnames = FALSE) # Use colnames=FALSE as the dataframe already has a clean header
  
  # Map Updater Observer (UPDATED for point/region logic)
  observe({
    proxy <- leafletProxy("map") %>%
      clearGroup("ecoregion") %>%
      clearGroup("species_points") %>%
      clearControls() 
      
    
    show_legend <- FALSE
    legend_colors <- c()
    legend_labels <- c()
    
    region_data <- selected_ecoregion_data()
    species_data <- filtered_species_data()
    
    region_is_active <- !is.null(region_data)
    species_is_active <- !is.null(species_data) && nrow(species_data) > 0

    # 1. Add Ecoregion
    if (region_is_active) {
      
      proxy %>%
        # Add the point marker
        addMarkers(lng = point()[[1]][1], lat = point()[[1]][2], popup = "Your Point", group = "ecoregion") %>%
        # Add the ecoregion polygon
        addPolygons(
          data = region_data,
          color = "#007BFF", # Blue
          weight = 2,
          fillOpacity = 0.3,
          popup = ~ paste("Ecoregion:", NA_L3KEY),
          group = "ecoregion"
        )
      
      # Add to legend
      show_legend <- TRUE
      legend_colors <- c(legend_colors, "#007BFF")
      legend_labels <- c(legend_labels, "Selected Ecoregion")
    }
    
    # 2. Add Species
    if (species_is_active) {
      proxy %>%
        addCircleMarkers(
          data = species_data,
          lng = ~x, lat = ~y,
          popup = ~ paste(
            "<strong> Dataset: </strong>", Dataset, "<br>",
            "<strong> Year: </strong>", Year, "<br>",
            "<strong> Qualitative: </strong>", Qualitative, "<br>",
            "<strong> Percent Cover: </strong>", PctCov, "<br>",
            "<strong> Cover Class: </strong>", AvgCovClass, "<br>"
          ),
          radius = 3,
          color = "#FF0000", # Red
          stroke = FALSE,
          fillOpacity = 0.6,
          group = "species_points"
        )
      
      # Add to legend
      show_legend <- TRUE
      legend_colors <- c(legend_colors, "#FF0000")
      legend_labels <- c(legend_labels, "Species Occurrences")
    }
    
    # 3. Handle Zoom
    if (region_is_active && species_is_active) {
      # Get bbox of both layers and combine them
      region_bbox <- st_bbox(region_data)
      species_bbox <- st_bbox(st_as_sf(species_data, coords = c("x", "y"), crs = 4326))
      
      combined_bbox <- st_bbox(c(
        xmin = min(region_bbox[[1]], species_bbox[[1]]),
        ymin = min(region_bbox[[2]], species_bbox[[2]]),
        xmax = max(region_bbox[[3]], species_bbox[[3]]),
        ymax = max(region_bbox[[4]], species_bbox[[4]])
      ))
      
      proxy %>% fitBounds(
        combined_bbox[[1]], combined_bbox[[2]],
        combined_bbox[[3]], combined_bbox[[4]]
      )
    } else if (region_is_active) {
      # Zoom to ecoregion
      bbox <- st_bbox(region_data)
      proxy %>% fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    } else if (species_is_active) {
      # Zoom to species
       proxy %>% fitBounds(
         lng1 = min(species_data$x), lat1 = min(species_data$y),
         lng2 = max(species_data$x), lat2 = max(species_data$y)
       )
    }
    
    # 4. Add Legend if needed
    if (show_legend) {
      proxy %>%
        addLegend("bottomright",
                  colors = legend_colors,
                  labels = legend_labels,
                  opacity = 0.7)
    }
    
  })
  
  # -- DOWNLOADS --
  
  # CSV download
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("species_list_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(is.null(region())) {
        write.csv(data.frame(Message = "No ecoregion selected."), file, row.names = FALSE)
      } else {
        write.csv(species_list_data(), file, row.names = FALSE)
      }
    }
  )
  
  # PDF download (basic)
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("species_list_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      if(is.null(region())) {
        writeLines(c("PDF generation skipped. No ecoregion selected."), file)
      } else {
         tryCatch({
           tempReport <- file.path(tempdir(), "report.Rmd")
           
           # Create a minimal Rmd file on the fly
           writeLines(c(
             "---",
             "title: 'Abundant Species List'",
             "output: pdf_document",
             "params:",
             "  data: NA",
             "---",
             "",
             "```{r, echo=FALSE}",
             "if(is.data.frame(params$data)) {",
             "  knitr::kable(params$data)",
             "} else {",
             "  'No data to display.'",
             "}",
             "```"
           ), tempReport)
           
           params <- list(data = species_list_data())
           
           rmarkdown::render(tempReport, output_file = file,
                             params = params,
                             envir = new.env(parent = globalenv()))
         }, error = function(e) {
           writeLines(c("PDF generation failed. Please ensure 'rmarkdown' and 'tinytex' are installed.",
                        "Error:", e$message), file)
         })
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)