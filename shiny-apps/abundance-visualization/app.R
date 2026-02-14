# add packages
library(sf)
library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(tidyr)
library(DT)
library(maps)
library(httr)
library(readr)
# rmarkdown loaded on-demand for PDF download (avoids slow startup)

# Load appendix data (RDS preferred for speed; fread faster than read_csv when CSV used)
if (file.exists("data/appendixS2.rds")) {
  s2 <- readRDS("data/appendixS2.rds")
} else if (requireNamespace("data.table", quietly = TRUE)) {
  s2 <- as.data.frame(data.table::fread("data/appendixS2.csv"))
} else {
  s2 <- read_csv("data/appendixS2.csv", show_col_types = FALSE)
}

# Load ecoregions and prepare them for spatial operations
eco <- readRDS("data/eco_simplified.rds")

# Filter out GEOMETRYCOLLECTION types that leaflet can't handle
eco <- eco %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))

# Dissolve ecoregion pieces across state boundaries (cached for faster loading)
eco_dissolved_path <- "data/eco_dissolved.rds"
if (file.exists(eco_dissolved_path)) {
  eco_dissolved <- readRDS(eco_dissolved_path)
} else {
  eco <- tryCatch(sf::st_make_valid(eco), error = function(e) eco)
  eco_dissolved <- eco %>%
    group_by(NA_L3KEY, NA_L3NAME) %>%
    summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    filter(sf::st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
  tryCatch(saveRDS(eco_dissolved, eco_dissolved_path), error = function(e) NULL)
}

# Load pre-computed species list and indexes (or compute at startup)
if (file.exists("data/species_list.rds")) {
  final_unique <- readRDS("data/species_list.rds")
} else {
  s2_forests <- s2 %>%
    filter(NA_L1KEY == "8  EASTERN TEMPERATE FORESTS" | NA_L1KEY == "5  NORTHERN FORESTS")
  s2_abun <- s2_forests %>% filter(grepl("Abundant", commonness))
  final_unique <- sort(unique(s2_abun$Orig.Genus.species))
}

# Species index: use precomputed if available; otherwise NULL (filter on-demand in reactive)
species_index <- if (file.exists("data/species_index.rds")) {
  readRDS("data/species_index.rds")
} else {
  NULL  # Avoid expensive split(s2, ...) at startup
}

# Load s7 and L3 data
s7 <- readRDS("data/s7_merged.rds")
L3_list <- data.frame(Species = s7$USDA.Genus.species, NA_L3KEY = s7$NA_L3KEY)
L3_index <- if (file.exists("data/L3_index.rds")) {
  readRDS("data/L3_index.rds")
} else {
  NULL  # Avoid split at startup; use filter in reactive
}


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
          h1(tags$i(class = "fas fa-leaf"), " Plant Abundance & Ecoregion Viewer"),
          a(href = "/", class = "home-btn",
            tags$i(class = "fas fa-home"), " Back to Home"
          )
      )
  ),
  
  # 2. Main Content Area
  div(class = "main-container",
          div(
      class = "info-card",
      h5(tags$i(class = "fas fa-info-circle"), "Tool information"),
      div(
        class = "abundant-definition",
        p(strong("What is an abundant species? "), "Species abundant indicates that the species has been reported as present with a qualitative abundance value, a reported percent cover above threshold, or an average cover class above threshold.")
      ),
      p("This tool visualizes introduced plant occurrence and abundance (source Bradley et al. 2025 in Scholarworks) across the continental U.S.  Occurrence data are sourced from ",
        tags$a(href = "https://www.eddmaps.org/", "EDDMapS", target = "_blank"),
        ", ",
        tags$a(href = "https://www.imapinvasives.org/", "iMap Invasives", target = "_blank"),
        ", and twelve other state and regional databases. All data points include information about qualitative abundance (e.g., high), percent cover (from 0-100%), and cover class (the average of a range of percent cover e.g., 3 is the mean of 1-5%). NAs indicate no information about abundance (we only know that the species is present). ‘Species present’ indicates that the species has been reported as present at either unknown or low abundance. ‘Species abundant’ indicates that the species has been reported as present with a qualitative abundance value of X, a reported percent cover >Y, or an average cover class >Z.
      "),
      p("To use the tool, select an ecoregion by clicking on the map, entering a zip code, or providing coordinates. Then choose a species from the dropdown to visualize its occurrence and abundance within the selected ecoregion. The map will update to show relevant data points, and the table below will list abundant species in that ecoregion. You can download the species list as a CSV or PDF for further analysis."
    ),),
      # Main layout with Sidebar and Main Panel
      div(class = "main-layout",
          
          # 2a. Sidebar for filters
          div(class = "sidebar",
              div(class = "filter-card",
                  div(class = "filter-header",
                      h5(tags$i(class = "fas fa-filter"), " Filter Options")
                  ),
                  div(class = "filter-body",
                      
                      # Radio buttons for input method
                      div(class = "form-group",
                          tags$label(
                            tags$i(class = "fas fa-location-arrow"), " Select Ecoregion By:"
                          ),
                          div(class = "radio-group",
                              radioButtons("input_method", label = NULL,
                                         choices = c("Click on Map" = "click",
                                                   "Zip Code" = "zip",
                                                   "Coordinates" = "coords"),
                                         selected = "click")
                          )
                      ),
                      
                      # Conditional panels based on input method
                      conditionalPanel(
                        condition = "input.input_method == 'coords'",
                        div(class = "form-group",
                            tags$label(tags$i(class = "fas fa-map-marker-alt"), " Enter Coordinates:"),
                            numericInput("lon", "Longitude", value = -72, step = 0.1, width = "100%"),
                            numericInput("lat", "Latitude", value = 42, step = 0.1, width = "100%"),
                            actionButton("go", "Find Ecoregion", 
                                       icon = icon("search"), 
                                       class = "btn-custom-small", 
                                       style = "margin-top: 5px; margin-bottom: 10px;")
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.input_method == 'zip'",
                        div(class = "form-group",
                            tags$label(tags$i(class = "fas fa-mail-bulk"), " Enter Zip Code:"),
                            textInput("zipcode", label = NULL, value = "", 
                                    placeholder = "e.g., 10001", width = "100%"),
                            actionButton("go_zip", "Find Ecoregion", 
                                       icon = icon("search"), 
                                       class = "btn-custom-small", 
                                       style = "margin-top: 5px; margin-bottom: 10px;")
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.input_method == 'click'",
                        div(class = "form-group instruction-box",
                            tags$p(tags$i(class = "fas fa-hand-pointer"), 
                                 " Click on any ecoregion on the map to select it.")
                        )
                      ),
                      
                      hr(),
                      
                      # Species Dropdown
                      div(class = "form-group",
                          tags$label(`for` = "selected_species",
                                     tags$i(class = "fas fa-seedling"), " Choose a Species:"
                          ),
                          selectizeInput("selected_species", label = NULL,
                                      choices = NULL,
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
                      leafletOutput("map", height = 450)
                  )
              ),
              
              # Results Table Card
              div(class = "results-card",
                  div(class = "results-header",
                      uiOutput("results_title", inline = TRUE)
                  ),
                  div(class = "results-body",
                      div(class = "table-container",
                          tableOutput("list")
                      ),
                      hr(),
                      fluidRow(
                        column(6, downloadButton("download_csv", "Download CSV", class = "btn-custom")),
                        column(6, downloadButton("download_pdf", "Download PDF", class = "btn-custom"))
                      )
                  )
              )
          )
      )
  ),
  
  # 3. App Footer
  tags$footer(class = "app-footer",
              div(class = "footer-container",
                  p("Plant Abundance & Ecoregion Viewer"),
                  div(
                    class = "citation-section",
                    p(
                      strong("Recommended Citation: "),
                      "Singh, K., J. Salva, M. Fertakos, and B.A. Bradley. RISCC Tools: Plant abundance and ecoregion viewer. URL: https://www.riscctools.org/abundance-visualization/, access date."
                    )
                  )
              )
  )
)



server <- function(input, output, session) {
  
  point <- reactiveVal(NULL)
  region <- reactiveVal(NULL)

  # Persistent zip code cache (survives restarts)
  zip_cache_path <- "data/zip_cache.rds"
  if (file.exists(zip_cache_path)) {
    zip_coord_cache <- readRDS(zip_cache_path)
  } else {
    zip_coord_cache <- list()
  }
  onSessionEnded(function() {
    tryCatch(saveRDS(zip_coord_cache, zip_cache_path), error = function(e) NULL)
  })
  
  # Populate species dropdown (now alphabetically sorted)
  observe({
    updateSelectizeInput(
      session,
      "selected_species",
      choices = c("None" = "", as.character(final_unique)),
      selected = '',
      server = TRUE
    )
  })
  
  # Action observer for coordinates
  observeEvent(input$go, {
    req(input$lat, input$lon)

    # Create a point from input coordinates
    pt <- st_sfc(st_point(c(input$lon, input$lat)), crs = 4326)
    point(pt)

    # Find which ecoregion the point falls into
    region_found <- eco_dissolved[sf::st_intersects(eco_dissolved, pt, sparse = FALSE), ]

    if (nrow(region_found) > 0) {
      region(region_found)
    } else {
      region(NULL)
      showNotification("No ecoregion found at these coordinates.", type = "warning")
    }
  })
  
# Action observer for zip code
observeEvent(input$go_zip, {
  req(input$zipcode)
  
  # Validate zip code format (basic check)
  if (!grepl("^\\d{5}$", input$zipcode)) {
    showNotification("Please enter a valid 5-digit zip code.", type = "error")
    return()
  }
  
  zip_key <- input$zipcode
  # Check cache first to avoid repeated API calls
  if (zip_key %in% names(zip_coord_cache)) {
    cached <- zip_coord_cache[[zip_key]]
    lon <- cached[1]
    lat <- cached[2]
    pt <- st_sfc(st_point(c(lon, lat)), crs = 4326)
    point(pt)
    region_found <- eco_dissolved[sf::st_intersects(eco_dissolved, pt, sparse = FALSE), ]
    if (nrow(region_found) > 0) {
      region(region_found)
      showNotification(paste("Found coordinates (cached):", round(lat, 4), ",", round(lon, 4)),
                       type = "message")
    } else {
      region(NULL)
      showNotification("No ecoregion found for this zip code.", type = "warning")
    }
  } else {
    url <- paste0("https://nominatim.openstreetmap.org/search?",
                  "postalcode=", input$zipcode,
                  "&country=US&format=json&limit=1")
    tryCatch({
      response <- GET(url, user_agent("ShinyApp"))
      if (status_code(response) == 200) {
        result <- content(response, "parsed")
        if (length(result) > 0) {
          lon <- as.numeric(result[[1]]$lon)
          lat <- as.numeric(result[[1]]$lat)
          zip_coord_cache[[zip_key]] <- c(lon, lat)
          pt <- st_sfc(st_point(c(lon, lat)), crs = 4326)
          point(pt)
          region_found <- eco_dissolved[sf::st_intersects(eco_dissolved, pt, sparse = FALSE), ]
          if (nrow(region_found) > 0) {
            region(region_found)
            showNotification(paste("Found coordinates:", round(lat, 4), ",", round(lon, 4)),
                             type = "message")
          } else {
            region(NULL)
            showNotification("No ecoregion found for this zip code.", type = "warning")
          }
        } else {
          showNotification("Zip code not found. Please try another.", type = "error")
        }
      } else {
        showNotification("Geocoding service unavailable. Please try again later.", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error geocoding zip code:", e$message), type = "error")
    })
  }
})
  
  # Observer for map clicks (ecoregion selection)
  observeEvent(input$map_shape_click, {
    req(input$input_method == "click")
    
    click <- input$map_shape_click
    
    if (!is.null(click$id)) {
      # Find the clicked ecoregion by ID
      region_found <- eco_dissolved[eco_dissolved$NA_L3KEY == click$id, ]
      
      if (nrow(region_found) > 0) {
        region(region_found)
        
        # Set point to centroid of the region
        centroid <- st_centroid(st_geometry(region_found))
        point(centroid)
        
        showNotification(paste("Selected:", region_found$NA_L3NAME[1]), type = "message")
      }
    }
  })
  
  # Reset button
  observeEvent(input$reset_btn, {
    # Reset coordinates and triggers
    updateNumericInput(session, "lon", value = -72)
    updateNumericInput(session, "lat", value = 42)
    updateTextInput(session, "zipcode", value = "")
    point(NULL)
    region(NULL)
    
    # Reset species filter
    updateSelectizeInput(session, "selected_species", selected = "")
    
    # Reset map view
    leafletProxy("map") %>%
      clearGroup("selected_region") %>%
      clearGroup("species_points") %>%
      clearMarkers() %>%
      clearControls() %>%
      setView(lng = -96.6638, lat = 39.7177, zoom = 4)
  })
  
  # -- REACTIVES --
  
  # Reactive for selected ecoregion polygon
  selected_ecoregion_data <- reactive({
    region()
  })
  
  # Reactive for selected species points (index lookup when precomputed, else filter)
  filtered_species_data <- reactive({
    if(is.null(input$selected_species) || input$selected_species == "") {
      return(NULL)
    }
    if (!is.null(species_index)) {
      res <- species_index[[input$selected_species]]
      if (is.null(res)) data.frame() else res
    } else {
      s2 %>% filter(Orig.Genus.species == input$selected_species)
    }
  })
  
  # Reactive for species list table (index lookup when precomputed, else filter)
  species_list_data <- reactive({
    req(region())
    region_name <- as.character(region()$NA_L3KEY[1])
    if (!is.null(L3_index)) {
      res <- L3_index[[region_name]]
      if (is.null(res)) data.frame(Species = character(), NA_L3KEY = character()) else res
    } else {
      L3_list %>% filter(NA_L3KEY == region_name)
    }
  })
  
  # -- OUTPUTS --
  
  # Base Map with all ecoregions displayed
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4,maxZoom = 20)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      # Panes ensure points stay clickable above polygons
      addMapPane("ecoregionsPane", zIndex = 410) %>%
      addMapPane("selectedEcoregionPane", zIndex = 415) %>%
      addMapPane("speciesPointsPane", zIndex = 420) %>%
      addPolygons(
        data = eco_dissolved,
        layerId = ~NA_L3KEY,
        color = "#666666",
        weight = 1,
        fillColor = "#e0e0e0",
        fillOpacity = 0.2,
        options = pathOptions(pane = "ecoregionsPane"),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.4,
          bringToFront = TRUE
        ),
        label = ~NA_L3NAME,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        ),
        group = "all_ecoregions"
      ) %>%
      setView(lng = -96.6638, lat = 39.7177, zoom = 4)
  })
  
  # Dynamic Results Card Title
  output$results_title <- renderUI({
    if (!is.null(region())) {
      region_name <- as.character(region()$NA_L3NAME[1])
      h5(tags$i(class = "fas fa-list"), "Abundant Species in:", tags$br(), region_name)
    } else {
      h5(tags$i(class = "fas fa-list"), "Abundant Species (Select an ecoregion)")
    }
  })
  
  # Species List Table
  output$list <- renderTable({
    if (is.null(region())) {
      return(data.frame(Message = "Select an ecoregion to see the species list."))
    }
    data <- species_list_data()
    if (nrow(data) == 0) {
      return(data.frame(Message = "No abundant species found for this ecoregion in the dataset."))
    }
    data
  }, colnames = FALSE)
  
  # Map Updater Observer (incremental: only clear overlay groups, not base ecoregions)
  observe({
    region_data <- selected_ecoregion_data()
    species_data <- filtered_species_data()
    region_is_active <- !is.null(region_data)
    species_is_active <- !is.null(species_data) && nrow(species_data) > 0

    proxy <- leafletProxy("map") %>%
      clearGroup("selected_region") %>%
      clearGroup("species_points") %>%
      clearMarkers() %>%
      clearControls()

    show_legend <- FALSE
    legend_colors <- c()
    legend_labels <- c()

    # 1. Add Selected Ecoregion
    if (region_is_active) {
      
      # Add marker if point exists
      if (!is.null(point())) {
        proxy <- proxy %>%
          addMarkers(lng = st_coordinates(point())[1], 
                    lat = st_coordinates(point())[2], 
                    popup = "Selected Location", 
                    group = "selected_region")
      }
      
      # Highlight selected ecoregion
      proxy <- proxy %>%
        addPolygons(
          data = region_data,
          color = "#007BFF",
          weight = 3,
          fillColor = "#007BFF",
          fillOpacity = 0.4,
          options = pathOptions(pane = "selectedEcoregionPane"),
          popup = ~ paste("Ecoregion:", NA_L3NAME),
          group = "selected_region"
        )
      
      # Add to legend
      show_legend <- TRUE
      legend_colors <- c(legend_colors, "#007BFF")
      legend_labels <- c(legend_labels, "Selected Ecoregion")
    }
    
    # 2. Add Species
    if (species_is_active) {
      # Segment data into presence-only and abundance data
      species_presence <- species_data %>%
        filter(is.na(Qualitative) & is.na(PctCov) & is.na(AvgCovClass))
      
      species_abundance <- species_data %>%
        filter(!is.na(Qualitative) | !is.na(PctCov) | !is.na(AvgCovClass))
      
      # Add presence-only points (red)
      if (nrow(species_presence) > 0) {
        proxy <- proxy %>%
          addCircleMarkers(
            data = species_presence,
            lng = ~x, lat = ~y,
            popup = ~ paste(
              "<strong> Dataset: </strong>", Dataset, "<br>",
              "<strong> Year: </strong>", Year, "<br>",
              "<strong> Type: </strong> Presence Only", "<br>"
            ),
            radius = 8,
            color = "#FF0000",
            options = pathOptions(pane = "speciesPointsPane"),
            stroke = FALSE,
            fillOpacity = 0.6,
            group = "species_points"
          )
        
        # Add to legend
        show_legend <- TRUE
        legend_colors <- c(legend_colors, "#FF0000")
        legend_labels <- c(legend_labels, "Species Presence")
      }
      
      # Add abundance points (green)
      if (nrow(species_abundance) > 0) {
        proxy <- proxy %>%
          addCircleMarkers(
            data = species_abundance,
            lng = ~x, lat = ~y,
            popup = ~ paste(
              "<strong> Dataset: </strong>", Dataset, "<br>",
              "<strong> Year: </strong>", Year, "<br>",
              "<strong> Qualitative: </strong>", ifelse(is.na(Qualitative), "NA", Qualitative), "<br>",
              "<strong> Percent Cover: </strong>", ifelse(is.na(PctCov), "NA", PctCov), "<br>",
              "<strong> Cover Class: </strong>", ifelse(is.na(AvgCovClass), "NA", AvgCovClass), "<br>"
            ),
            radius = 8,
            color = "#00CC00",
            options = pathOptions(pane = "speciesPointsPane"),
            stroke = FALSE,
            fillOpacity = 0.6,
            group = "species_points"
          )
        
        # Add to legend
        show_legend <- TRUE
        legend_colors <- c(legend_colors, "#00CC00")
        legend_labels <- c(legend_labels, "Species Abundance")
      }
    }
    
    # 3. Handle Zoom
    if (region_is_active && species_is_active) {
      region_bbox <- st_bbox(region_data)
      species_bbox <- st_bbox(st_as_sf(species_data, coords = c("x", "y"), crs = 4326))
      
      combined_bbox <- st_bbox(c(
        xmin = min(region_bbox[[1]], species_bbox[[1]]),
        ymin = min(region_bbox[[2]], species_bbox[[2]]),
        xmax = max(region_bbox[[3]], species_bbox[[3]]),
        ymax = max(region_bbox[[4]], species_bbox[[4]])
      ))
      
      proxy <- proxy %>% fitBounds(
        combined_bbox[[1]], combined_bbox[[2]],
        combined_bbox[[3]], combined_bbox[[4]]
      )
    } else if (region_is_active) {
      bbox <- st_bbox(region_data)
      proxy <- proxy %>% fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    } else if (species_is_active) {
      proxy <- proxy %>% fitBounds(
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
  
  # Helper to sanitize ecoregion name for filename
  sanitize_filename <- function(name) {
    x <- gsub("[^A-Za-z0-9_-]", "_", name)
    gsub("_+", "_", x)
  }

  # CSV download
  output$download_csv <- downloadHandler(
    filename = function() {
      if (is.null(region())) {
        paste0("species_list_", Sys.Date(), ".csv")
      } else {
        region_name <- sanitize_filename(as.character(region()$NA_L3NAME[1]))
        paste0(region_name, "_abundant_species_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      if(is.null(region())) {
        write.csv(data.frame(Message = "No ecoregion selected."), file, row.names = FALSE)
      } else {
        write.csv(species_list_data(), file, row.names = FALSE)
      }
    }
  )
  
  # PDF download
  output$download_pdf <- downloadHandler(
    filename = function() {
      if (is.null(region())) {
        paste0("no_ecoregion_selected_", Sys.Date(), ".txt")
      } else {
        region_name <- sanitize_filename(as.character(region()$NA_L3NAME[1]))
        paste0(region_name, "_abundant_species_", Sys.Date(), ".pdf")
      }
    },
    content = function(file) {
      if(is.null(region())) {
        writeLines("PDF generation skipped. No ecoregion selected.", file)
      } else {
        tryCatch({
          if (!requireNamespace("rmarkdown", quietly = TRUE)) {
            stop("rmarkdown package is required for PDF export")
          }
          library(rmarkdown)
          tempReport <- file.path(tempdir(), "report.Rmd")
          
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