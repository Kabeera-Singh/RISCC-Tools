library(tidyverse)
library(leaflet)
library(readxl)
library(shiny)
library(shinyjs)
library(DT)
library(maps)

plants_data <- read.csv("data/Final Regulated Plants by State June 2025 - MergedPlantsByState.csv")

state_map <- data.frame(
  StateName = c(state.name, "Puerto Rico"),
  StateAbbr = c(state.abb, "PR"),
  stringsAsFactors = FALSE
)

state_to_maps_name <- function(state_name) {
  x <- tolower(state_name)
  x <- gsub("[[:punct:]]", "", x)
  x
}

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

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css",
      rel = "stylesheet"
    )
  ),
  useShinyjs(),

  div(
    class = "app-header",
    div(
      class = "header-container",
      h1(tags$i(class = "fas fa-seedling"), " State-Based Regulated Plant Data Viewer"),
      a(href = "/", class = "home-btn",
        tags$i(class = "fas fa-home"), " Back to Home"
      )
    )
  ),

  div(
    class = "main-container",

      div(
        class = "info-card",
        h5(tags$i(class = "fas fa-info-circle"), " How to Use This Tool"),
        p("This app allows you to filter by a particular state or species name to find the data you're looking for. The map will highlight your selections, and the table below will update."),
        p(
          "Note: The data from this resource was compiled from the ",
          tags$a(href = "https://www.nationalplantboard.org/state-law--regulation-summaries.html", "National Plant Board", target = "_blank"),
          "or view the ",
          tags$a(href = "https://docs.google.com/spreadsheets/d/1XdmtsDxd4A3fURZixemEsAvCmYQ8_XM9d10WezWCWlg/edit?gid=651617479#gid=651617479", "original source", target = "_blank"),
          "data."
        )
      ),

    div(
      class = "main-layout",

      div(
        class = "sidebar",
        div(
          class = "filter-card",
          div(
            class = "filter-header",
            h5(tags$i(class = "fas fa-filter"), " Filter Options")
          ),
          div(
            class = "filter-body",
            div(
              class = "form-group",
              tags$label(
                `for` = "selected_state",
                tags$i(class = "fas fa-map-marker-alt"), " Choose a State:"
              ),
              selectInput("selected_state", label = NULL,
                choices = c("All", state_map$StateName), width = "100%"
              )
            ),
            div(
              class = "form-group",
              tags$label(
                `for` = "selected_species",
                tags$i(class = "fas fa-leaf"), " Choose a Species:"
              ),
              selectInput("selected_species", label = NULL,
                choices = NULL, width = "100%"
              )
            ),
            actionButton("reset_btn", "Reset Filters",
              icon = icon("refresh"), class = "btn-custom"
            ),
            hr(),
            div(
              class = "map-card",
              div(
                class = "map-header",
                h5(tags$i(class = "fas fa-map-location-dot"), " Regulation Map")
              ),
              div(
                class = "map-body",
                leafletOutput("state_map", height = 200)
              )
            )
          )
        )
      ),

      div(
        class = "main-panel",
        div(
          class = "results-card",
          div(
            class = "results-header",
            h5(tags$i(class = "fas fa-list"), " Filtered Data")
          ),
          div(
            class = "results-body",
            DTOutput("filtered_table"),
            hr(),
            div(
              class = "download-buttons",
              downloadButton("download_csv", "Download CSV", 
                class = "btn-custom-small", icon = icon("file-csv")),
              downloadButton("download_txt", "Download TXT", 
                class = "btn-custom-small", icon = icon("file-lines"))
            )
          )
        )
      )
    )
  ),

  tags$footer(
    class = "app-footer",
    div(
      class = "footer-container",
      p("Regulated Plant Database Viewer")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    species_choices <- sort(unique(plants_data$Clean.Scientific.Name))
    updateSelectInput(session, "selected_species", choices = c("All", species_choices))
  })

  observeEvent(input$reset_btn, {
    updateSelectInput(session, "selected_state", selected = "All")
    updateSelectInput(session, "selected_species", selected = "All")
  })

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

  output$filtered_table <- renderDT({
    df <- filtered_data() %>% select(-`Raw Scientific Name`)
    
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th('State ', span(class="info-icon", '?', 
            span(class="tooltip-text", 'U.S. state or territory abbreviation where the plant is regulated'))),
          th('Scientific Name ', span(class="info-icon", '?', 
            span(class="tooltip-text", 'Standardized scientific name after data cleaning and validation'))),
          th('Regulatory Level ', span(class="info-icon", '?', 
            span(class="tooltip-text", 'Classification of regulatory status (e.g., Noxious Weeds, Aquatic Invasive Weeds, Prohibited, Restricted)'))),
          th('USDA Symbol', span(class="info-icon", '?', 
            span(class="tooltip-text", 'Official USDA PLANTS database symbol for the species'))),
          th('Native Status ', span(class="info-icon", '?', 
            span(class="tooltip-text", 'Geographic origin: (N) = Native, (I) = Introduced/Invasive, (W) = Waif/Occasional. Format: Region(Status), e.g., L48(I) means introduced in lower 48 states')))
        )
      )
    ))
    
    datatable(df, container = sketch,
      options = list(pageLength = 10, dom = "frtip"),
      rownames = FALSE, escape = FALSE
    )
  })

  output$state_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2, maxZoom = 20)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)
  })

  observe({
    leafletProxy("state_map") %>%
      clearShapes() %>%
      clearControls()

    species_active <- (!is.null(input$selected_species) && input$selected_species != "All")
    state_active <- (input$selected_state != "All")

    if (!species_active && !state_active) {
      leafletProxy("state_map") %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      return()
    }

    if (species_active) {
      states_for_species <- plants_data %>%
        filter(Clean.Scientific.Name == input$selected_species) %>%
        pull(State) %>%
        unique()

      for (abbr in states_for_species) {
        state_name <- state_map$StateName[state_map$StateAbbr == abbr]
        if (length(state_name) == 0) next

        region_name <- state_to_maps_name(state_name)
        
        if (!(region_name %in% c(state_to_maps_name(state.name), "alaska", "hawaii"))) next
        
        poly <- tryCatch(
          maps::map("state", regions = region_name, plot = FALSE, fill = TRUE),
          error = function(e) NULL
        )

        if (!is.null(poly) && length(poly$x) > 0) {
          leafletProxy("state_map") %>%
            addPolygons(
              lng = poly$x, lat = poly$y,
              fillColor = "forestgreen", fillOpacity = 0.5,
              color = "darkgreen", weight = 1,
              label = state_name, group = "species"
            )
        }
      }
    }

    if (state_active) {
      region_name <- state_to_maps_name(input$selected_state)

      if (region_name == "puerto rico") {
        leafletProxy("state_map") %>%
          setView(lng = -66.5, lat = 18.2, zoom = 8) %>%
          addCircleMarkers(
            lng = -66.5, lat = 18.2, radius = 15,
            fillColor = "gold", fillOpacity = 0.5,
            color = "gold", weight = 4,
            label = "Puerto Rico (data available, map outline not available)"
          )
      } else if (region_name == "alaska") {
        leafletProxy("state_map") %>%
          setView(lng = -152, lat = 64, zoom = 4) %>%
          addRectangles(
            lng1 = -170, lat1 = 54, lng2 = -130, lat2 = 72,
            fillColor = "gold", fillOpacity = 0.3,
            color = "gold", weight = 4, label = "Alaska"
          )
      } else if (region_name == "hawaii") {
        leafletProxy("state_map") %>%
          setView(lng = -157, lat = 20.5, zoom = 6) %>%
          addRectangles(
            lng1 = -160.5, lat1 = 18.5, lng2 = -154.5, lat2 = 22.5,
            fillColor = "gold", fillOpacity = 0.3,
            color = "gold", weight = 4, label = "Hawaii"
          )
      } else {
        selected_state_poly <- tryCatch(
          maps::map("state", regions = region_name, plot = FALSE, fill = TRUE),
          error = function(e) list(x = numeric(0), y = numeric(0))
        )
        
        if (length(selected_state_poly$x) > 0) {
          leafletProxy("state_map") %>%
            addPolygons(
              lng = selected_state_poly$x, lat = selected_state_poly$y,
              fillColor = "gold", fillOpacity = 0.3,
              color = "gold", weight = 4,
              label = input$selected_state, group = "state"
            )

          lng_bounds <- range(selected_state_poly$x, na.rm = TRUE)
          lat_bounds <- range(selected_state_poly$y, na.rm = TRUE)
          lng_padding <- diff(lng_bounds) * 0.1
          lat_padding <- diff(lat_bounds) * 0.1
          
          leafletProxy("state_map") %>%
            fitBounds(
              lng_bounds[1] - lng_padding, lat_bounds[1] - lat_padding,
              lng_bounds[2] + lng_padding, lat_bounds[2] + lat_padding
            )
        }
      }
    }

    if (species_active || state_active) {
      legend_colors <- c()
      legend_labels <- c()

      if (species_active) {
        legend_colors <- c(legend_colors, "forestgreen")
        legend_labels <- c(legend_labels, "Species Regulated")
      }
      if (state_active) {
        legend_colors <- c(legend_colors, "gold")
        legend_labels <- c(legend_labels, "Selected State")
      }

      leafletProxy("state_map") %>%
        addLegend("bottomright", colors = legend_colors, 
          labels = legend_labels, opacity = 0.7)
    }
  })

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

shinyApp(ui, server)