library(tidyverse)
library(leaflet)
library(readxl)
library(shiny)
library(DT)
library(maps)

# Load plant data
plants_data <- readxl::read_xls('Regulatory visualization/data/Merged_Plants - MergedPlantsByState.xls')

# State name and abbreviation mapping
state_map <- data.frame(StateName = state.name, StateAbbr = state.abb, stringsAsFactors = FALSE)

# Map formatting helper
state_to_maps_name <- function(state_name) {
  x <- tolower(state_name)
  x <- gsub("[[:punct:]]", "", x)
  x
}

# Rename columns for readability
clean_colnames <- function(df) {
  df %>%
    rename(
      `Raw Scientific Name` = Raw.Scientific.Name,
      `Cleaned Scientific Name` = Clean.Scientific.Name,
      `Regulatory Level` = Reg.Level,
      `Accepted Symbol (USDA Plants)` = Accepted.Symbol,
      `Native Status` = Native.Status
    )
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4fdf7;
        font-family: 'Verdana', sans-serif;
      }
      .well {
        background-color: #2e5939 !important;
        color: white !important;
        border-radius: 8px;
      }
      .well p {
        font-size: 16px;
        line-height: 1.5;
      }
      h2 {
        color: darkolivegreen;
        font-weight: bold;
      }
      .btn {
        background-color: #4e944f;
        color: white;
        border: none;
        border-radius: 5px;
        margin-top: 10px;
        width: 100%;
      }
      .btn:hover {
        background-color: #3b7d3b;
      }
    "))
  ),
  
  titlePanel(HTML("<h2><i class='fas fa-seedling'></i> State-Based Regulated Plant Data Viewer</h2>")),
  
  fluidRow(
    column(3,
           wellPanel(
             h4("How to use this app!"),
             p("This app allows you to filter between a particular state or species name to find the data you're looking for!")
           ),
           br(),
           wellPanel(
             h4("Filter Options"),
             selectInput("selected_state", "Choose a State:", choices = c("All", state_map$StateName)),
             selectInput("selected_species", "Choose a Species:", choices = NULL),
             actionButton("reset_btn", "Reset Filters", class = "btn"),
             downloadButton("download_csv", "Download CSV"),
             downloadButton("download_txt", "Download TXT")
           )
    ),
    
    column(9,
           leafletOutput("state_map", height = 600),
           br(),
           DTOutput("filtered_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Populate species dropdown
  observe({
    species_choices <- sort(unique(plants_data$Clean.Scientific.Name))
    updateSelectInput(session, "selected_species", choices = c("All", species_choices))
  })
  
  # Reset button clears filters
  observeEvent(input$reset_btn, {
    updateSelectInput(session, "selected_state", selected = "All")
    updateSelectInput(session, "selected_species", selected = "All")
  })
  
  # Filtering
  filtered_data <- reactive({
    df <- plants_data
    
    if (input$selected_state != "All") {
      selected_abbr <- state_map$StateAbbr[state_map$StateName == input$selected_state]
      df <- df %>% filter(State == selected_abbr)
    }
    
    if (!is.null(input$selected_species) && input$selected_species != "All") {
      df <- df %>% filter(Clean.Scientific.Name == input$selected_species)
    }
    
    clean_colnames(df)
  })
  
  # Table
  output$filtered_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, dom = 'frtip'),
      rownames = FALSE
    )
  })
  
  # Base map
  output$state_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)
  })
  
  # Highlighting
  observe({
    leafletProxy("state_map") %>% clearShapes() %>% clearControls()
    
    species_active <- (!is.null(input$selected_species) && input$selected_species != "All")
    state_active <- (input$selected_state != "All")
    
    # Reset to full view when no filters
    if (!species_active && !state_active) {
      leafletProxy("state_map") %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      return()
    }
    
    # Highlight species states
    if (species_active) {
      states_for_species <- plants_data %>%
        filter(Clean.Scientific.Name == input$selected_species) %>%
        pull(State) %>%
        unique()
      
      for (abbr in states_for_species) {
        state_name <- state_map$StateName[state_map$StateAbbr == abbr]
        if (length(state_name) == 0) next
        
        region_name <- state_to_maps_name(state_name)
        poly <- tryCatch({
          maps::map("state", regions = region_name, plot = FALSE, fill = TRUE)
        }, error = function(e) NULL)
        
        if (!is.null(poly) && length(poly$x) > 0) {
          leafletProxy("state_map") %>%
            addPolygons(
              lng = poly$x,
              lat = poly$y,
              fillColor = "forestgreen",
              fillOpacity = 0.5,
              color = "darkgreen",
              weight = 1,
              label = state_name,
              group = "species"
            )
        }
      }
    }
    
    # Highlight selected state
    if (state_active) {
      region_name <- state_to_maps_name(input$selected_state)
      
      selected_state_poly <- tryCatch({
        maps::map("state", regions = region_name, plot = FALSE, fill = TRUE)
      }, error = function(e) {
        list(x = numeric(0), y = numeric(0))
      })
      
      # Adjust Alaska/Hawaii
      if (region_name == "alaska" && length(selected_state_poly$x) > 0) {
        selected_state_poly$x <- (selected_state_poly$x - 160) * 0.35 - 130
        selected_state_poly$y <- (selected_state_poly$y * 0.35) + 25
      }
      if (region_name == "hawaii" && length(selected_state_poly$x) > 0) {
        selected_state_poly$x <- selected_state_poly$x - 50
        selected_state_poly$y <- selected_state_poly$y + 25
      }
      
      if (length(selected_state_poly$x) > 0) {
        leafletProxy("state_map") %>%
          addPolygons(
            lng = selected_state_poly$x,
            lat = selected_state_poly$y,
            fillColor = "transparent",
            fillOpacity = 0,
            color = "gold",
            weight = 4,
            label = input$selected_state,
            group = "state"
          )
      }
      
      # Zoom to selected state
      if (region_name == "alaska") {
        leafletProxy("state_map") %>%
          fitBounds(-170, 50, -130, 72)
      } else if (region_name == "hawaii") {
        leafletProxy("state_map") %>%
          fitBounds(-162, 18, -154, 23)
      } else if (length(selected_state_poly$x) > 0) {
        lng_bounds <- range(selected_state_poly$x, na.rm = TRUE)
        lat_bounds <- range(selected_state_poly$y, na.rm = TRUE)
        leafletProxy("state_map") %>%
          fitBounds(lng_bounds[1], lat_bounds[1], lng_bounds[2], lat_bounds[2])
      }
    }
    
    # Legend (only if something selected)
    if (species_active || state_active) {
      legend_colors <- c()
      legend_labels <- c()
      
      if (species_active) {
        legend_colors <- c(legend_colors, "forestgreen")
        legend_labels <- c(legend_labels, "Species Present")
      }
      if (state_active) {
        legend_colors <- c(legend_colors, "gold")
        legend_labels <- c(legend_labels, "Selected State")
      }
      
      leafletProxy("state_map") %>%
        addLegend("bottomright", colors = legend_colors, labels = legend_labels, opacity = 0.7)
    }
  })
  
  # Downloads
  output$download_csv <- downloadHandler(
    filename = function() paste0("plant_data_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$download_txt <- downloadHandler(
    filename = function() paste0("plant_data_", Sys.Date(), ".txt"),
    content = function(file) {
      write.table(filtered_data(), file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
}

app <- shinyApp(ui, server)
runApp(app, port = 1234, launch.browser = TRUE)
