library(tidyverse)
library(leaflet)
library(readxl)
library(shiny)
library(shinyjs)
library(DT)
library(readr)
library(sf)

# Load plants data (RDS preferred for speed; fallback to CSV)
if (file.exists("data/plants_data.rds")) {
  plants_data <- readRDS("data/plants_data.rds")
} else {
  plants_data <- read_csv("data/Final Regulated Plants by State June 2025 - MergedPlantsByState.csv", show_col_types = FALSE)
}

# State outlines: Census 1:20m boundaries with AK/HI/PR moved below the lower 48
# (built by scripts/build_state_shapes.R)
state_shapes <- readRDS("data/state_shapes.rds")

species_counts <- plants_data %>%
  distinct(State, Clean.Scientific.Name) %>%
  count(State, name = "n_species")
state_shapes$n_species <- species_counts$n_species[match(state_shapes$STUSPS, species_counts$State)]
state_shapes$n_species[is.na(state_shapes$n_species)] <- 0L

federal_species <- unique(plants_data$Clean.Scientific.Name[plants_data$State == "FED"])

us_bounds <- unname(sf::st_bbox(state_shapes))

state_map <- data.frame(
  StateName = c(state.name, "Puerto Rico"),
  StateAbbr = c(state.abb, "PR"),
  stringsAsFactors = FALSE
)
state_name_to_abbr <- setNames(state_map$StateAbbr, state_map$StateName)

# Map styling: Albers equal-area projection with no basemap tiles, shaded in the
# site's red palette
albers_crs <- leafletCRS(
  crsClass = "L.Proj.CRS", code = "EPSG:5070",
  proj4def = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
  resolutions = 2^(16:7)
)
no_data_color <- "#e2e2e2"
regulated_color <- "#881c1c"
selected_outline_color <- "#2c2c2c"
count_breaks <- c(-Inf, 0, 25, 50, 100, 150, Inf)
count_labels <- c("None listed", "1-25", "26-50", "51-100", "101-150", "151+")
count_colors <- c(no_data_color, "#fcbba1", "#fc9272", "#ef5a43", "#cb2a24", "#881c1c")

fit_to_bbox <- function(map, bb) {
  fitBounds(map, bb[1], bb[2], bb[3], bb[4], options = list(padding = c(8, 8)))
}

# Shades every state (species count by default, or regulated/not for a chosen
# species). A chosen state keeps full color while the others fade, and its
# outline sits in its own pane above the states so hovering never covers it.
# Works on a leaflet map or a proxy.
draw_states <- function(map, selected_state, selected_species) {
  species_active <- !is.null(selected_species) && selected_species != "All"
  state_names <- htmltools::htmlEscape(state_shapes$NAME)

  if (species_active) {
    species_rows <- plants_data[plants_data$Clean.Scientific.Name == selected_species, ]
    levels_by_state <- tapply(species_rows$Reg.Level, species_rows$State,
                              function(x) paste(unique(x), collapse = ", "))
    reg_level <- unname(levels_by_state[state_shapes$STUSPS])
    regulated <- !is.na(reg_level)
    fill <- ifelse(regulated, regulated_color, no_data_color)
    labels <- ifelse(
      regulated,
      sprintf("<strong>%s</strong><br>Regulated: %s", state_names, htmltools::htmlEscape(reg_level)),
      sprintf("<strong>%s</strong><br>Not regulated", state_names)
    )
    legend_colors <- c(regulated_color, no_data_color)
    legend_labels <- c("Regulated", "Not regulated")
    legend_title <- "Species status"
  } else {
    fill <- count_colors[cut(state_shapes$n_species, count_breaks, labels = FALSE)]
    labels <- sprintf(
      "<strong>%s</strong><br>%s", state_names,
      ifelse(state_shapes$n_species == 0, "No regulated species listed",
             paste(state_shapes$n_species, "regulated species"))
    )
    legend_colors <- count_colors
    legend_labels <- count_labels
    legend_title <- "Regulated species"
  }

  selected_shape <- state_shapes[state_shapes$NAME == selected_state, ]
  state_active <- nrow(selected_shape) > 0
  fill_opacity <- if (state_active) ifelse(state_shapes$NAME == selected_state, 1, 0.5) else 1

  map <- map %>%
    clearGroup("states") %>%
    clearGroup("selected") %>%
    clearControls() %>%
    addPolygons(
      data = state_shapes, layerId = ~STUSPS, group = "states",
      fillColor = fill, fillOpacity = fill_opacity,
      color = "#ffffff", weight = 0.8, opacity = 1,
      label = lapply(labels, htmltools::HTML),
      labelOptions = labelOptions(direction = "auto", textsize = "13px"),
      highlightOptions = highlightOptions(color = "#ffffff", weight = 3, bringToFront = TRUE)
    ) %>%
    addLegend("bottomright", colors = legend_colors, labels = legend_labels,
              title = legend_title, opacity = 1)

  if (state_active) {
    # White halo under a dark stroke keeps the outline visible on any fill
    map <- map %>%
      addPolygons(
        data = selected_shape, group = "selected",
        fill = FALSE, color = "#ffffff", weight = 6, opacity = 1,
        options = pathOptions(pane = "selected", interactive = FALSE)
      ) %>%
      addPolygons(
        data = selected_shape, group = "selected",
        fill = FALSE, color = selected_outline_color, weight = 2.5, opacity = 1,
        options = pathOptions(pane = "selected", interactive = FALSE)
      )
  }
  fit_to_bbox(map, us_bounds)
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
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-K89HMLXL9K"),
    tags$script(HTML("
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-K89HMLXL9K');
    ")),
    # Version stamp so browsers pick up stylesheet changes after a deploy
    tags$link(rel = "stylesheet", type = "text/css",
              href = paste0("style.css?v=", as.integer(file.mtime("www/style.css")))),
    tags$link(
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css",
      rel = "stylesheet"
    ),
    tags$script(HTML("
      $(document).on('shiny:disconnected', function(event) {
        if (!$('#proxy-error-banner').length) {
          $('body').prepend(
            '<div id=\"proxy-error-banner\" style=\"background:#fff3cd;border:1px solid #ffc107;padding:15px;margin:10px;border-radius:8px;text-align:center;\">' +
            '<strong>Connection Issue Detected</strong><br>' +
            'If you are on a corporate network (VPN, Zscaler, etc.), the security software may be blocking this application.<br>' +
            '<strong>Solutions:</strong> Try using a personal network, mobile hotspot, or ask IT to whitelist this domain.' +
            '</div>'
          );
        }
      });

      // GA4 custom event handler for Shiny server messages
      Shiny.addCustomMessageHandler('ga_event', function(data) {
        if (typeof gtag !== 'undefined') {
          gtag('event', data.event_name, data.params || {});
        }
      });

      // Track map engagement (first interaction only)
      var mapEngaged = false;
      $(document).on('mousedown', '.leaflet-container', function() {
        if (!mapEngaged) {
          mapEngaged = true;
          if (typeof gtag !== 'undefined') {
            gtag('event', 'map_engaged', { tool: 'regulatory_visualization' });
          }
        }
      });
    "))
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
        p("Choose a state from the dropdown to create a list of all regulated species in that state. Choose a species from the dropdown to create a list of all states where that species is regulated. The map will highlight your selections, and the table below will update."),
        p(
          "Data sources: The data from this resource was compiled from the National Plant Board Website ",
          tags$a(href = "https://www.nationalplantboard.org/state-law--regulation-summaries.html", "here", target = "_blank"),
          ". Learn more about regulatory level definitions in the Reg.Level Definitions tab ",
          tags$a(href = "https://docs.google.com/spreadsheets/d/1XdmtsDxd4A3fURZixemEsAvCmYQ8_XM9d10WezWCWlg/edit?usp=sharing", "here", target = "_blank"),
          "."
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
              icon = icon("refresh"), class = "btn-reset"
            ),
            p(class = "filter-hint",
              tags$i(class = "fas fa-hand-pointer"), " Tip: click a state on the map to select it.")
          )
        )
      ),

      div(
        class = "main-panel",
        div(
          class = "map-card",
          div(
            class = "map-header",
            h5(tags$i(class = "fas fa-map-location-dot"), " Regulation Map")
          ),
          div(
            class = "map-body",
            uiOutput("map_caption", class = "map-caption"),
            leafletOutput("state_map", height = 460),
            p(class = "map-note",
              "State boundaries: U.S. Census Bureau. Alaska, Hawaii and Puerto Rico are repositioned and not to scale.")
          )
        ),
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
              class = "download-row",
              downloadButton("download_csv", "Download CSV", 
                class = "btn-download", icon = icon("file-csv")),
              downloadButton("download_txt", "Download TXT", 
                class = "btn-download", icon = icon("file-lines"))
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
      p("Regulated Plant Database Viewer"),
      div(
        class = "citation-section",
        p(
          strong("Recommended Citation: "),
          "Singh, K., J. Salva, M. Fertakos, and B.A. Bradley. RISCC Tools: State-based regulated plant data viewer. URL: https://www.riscctools.org/regulatory-visualization/, access date."
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # GA4 Analytics Event Tracking
  ga_event <- function(event_name, params = list()) {
    params$tool <- "regulatory_visualization"
    session$sendCustomMessage("ga_event", list(event_name = event_name, params = params))
  }

  observe({
    species_choices <- sort(unique(plants_data$Clean.Scientific.Name))
    updateSelectInput(session, "selected_species", choices = c("All", species_choices))
  })

  observeEvent(input$selected_state, {
    ga_event("filter_state", list(state = input$selected_state))
  }, ignoreInit = TRUE)

  observeEvent(input$selected_species, {
    req(input$selected_species)
    ga_event("filter_species", list(species = input$selected_species))
  }, ignoreInit = TRUE)

  observeEvent(input$reset_btn, {
    ga_event("reset_filters")
    updateSelectInput(session, "selected_state", selected = "All")
    updateSelectInput(session, "selected_species", selected = "All")
    leafletProxy("state_map") %>% fit_to_bbox(us_bounds)
  })

  filtered_data <- reactive({
    df <- plants_data

    if (input$selected_state != "All") {
      selected_abbr <- state_name_to_abbr[input$selected_state]
      if (!is.na(selected_abbr)) {
        df <- df %>% filter(State == selected_abbr)
      }
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
            span(class="tooltip-text", 'Native Status is derived from the USDA PLANTS Database and is written in the format Region(Status). Regions: AK=Alaska, CAN=Canada, GL=Greenland, HI=Hawaii, L48=Lower 48 states, NA=North America (non-vascular plants and lichens only), NAV=Navassa Island, PB=Pacific Basin excluding Hawaii, PR=Puerto Rico, SPM=St. Pierre and Miquelon (France), VI=U.S. Virgin Islands. Status: (N)=Native, (I)=Introduced, (W)=Waif/Occasional. Question marks note probable status.')))
        )
      )
    ))
    
    datatable(df, container = sketch,
      options = list(pageLength = 10, dom = "frtip"),
      rownames = FALSE, escape = FALSE
    )
  })

  output$state_map <- renderLeaflet({
    leaflet(options = leafletOptions(
      crs = albers_crs, zoomSnap = 0.1, zoomDelta = 0.5,
      minZoom = 1, maxZoom = 8
    )) %>%
      addMapPane("selected", zIndex = 450) %>%
      draw_states("All", "All")
  })

  observeEvent(list(input$selected_state, input$selected_species), {
    leafletProxy("state_map") %>%
      draw_states(input$selected_state, input$selected_species)
  }, ignoreInit = TRUE)

  # Clicking a state selects it; clicking the selected state again clears it
  observeEvent(input$state_map_shape_click, {
    clicked <- state_shapes$NAME[match(input$state_map_shape_click$id, state_shapes$STUSPS)]
    if (is.na(clicked) || !(clicked %in% state_map$StateName)) return()
    ga_event("map_click_state", list(state = clicked))
    updateSelectInput(session, "selected_state",
      selected = if (identical(input$selected_state, clicked)) "All" else clicked)
  })

  output$map_caption <- renderUI({
    species <- input$selected_species
    caption <- if (!is.null(species) && species != "All") {
      tagList(
        "States regulating ", tags$em(species),
        if (species %in% federal_species) " (also federally regulated)"
      )
    } else {
      "Number of regulated plant species by state"
    }

    selected <- state_shapes[state_shapes$NAME == input$selected_state, ]
    if (nrow(selected) == 0) return(caption)
    tagList(
      caption,
      tags$span(class = "map-caption-state",
        sprintf("%s: %d regulated species", selected$NAME, selected$n_species))
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      if (input$selected_state != "All") {
        paste0(gsub(" ", "_", input$selected_state), "_regulated_invasive_plants_", Sys.Date(), ".csv")
      } else if (!is.null(input$selected_species) && input$selected_species != "All") {
        paste0(gsub(" ", "_", input$selected_species), "_regulated_states_", Sys.Date(), ".csv")
      } else {
        paste0("plant_data_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      ga_event("download_csv", list(state = input$selected_state, species = input$selected_species))
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  output$download_txt <- downloadHandler(
    filename = function() {
      if (input$selected_state != "All") {
        paste0(gsub(" ", "_", input$selected_state), "_regulated_invasive_plants_", Sys.Date(), ".txt")
      } else if (!is.null(input$selected_species) && input$selected_species != "All") {
        paste0(gsub(" ", "_", input$selected_species), "_regulated_states_", Sys.Date(), ".txt")
      } else {
        paste0("plant_data_", Sys.Date(), ".txt")
      }
    },
    content = function(file) {
      ga_event("download_txt", list(state = input$selected_state, species = input$selected_species))
      write.table(filtered_data(), file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
}

shinyApp(ui, server)