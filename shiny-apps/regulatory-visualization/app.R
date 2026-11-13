library(tidyverse)
library(leaflet)
library(readxl)
library(shiny)
library(shinyjs)
library(DT)
library(sf)

# Regulated plants by U.S. state and Canadian province. The State column holds
# codes like US-MA, CA-ON, US-FED and CA-FED (State/Province in the source
# spreadsheet). RDS is built by scripts/preprocess_data.R; the xlsx is a fallback.
if (file.exists("data/plants_data.rds")) {
  plants_data <- readRDS("data/plants_data.rds")
} else {
  plants_data <- read_excel("data/Final Regulated Plants by State July 2026.xlsx",
                            sheet = "MergedPlantsByState", na = "") %>%
    rename(State = `State/Province`) %>%
    filter(!is.na(State))
}

# State and province outlines (CEC North American Atlas), with Hawaii and Puerto
# Rico moved in near the lower 48. Built by scripts/build_state_shapes.R.
state_shapes <- readRDS("data/state_shapes.rds")
# Frames drawn around the Hawaii and Puerto Rico insets
inset_frames <- readRDS("data/inset_frames.rds")

species_counts <- plants_data %>%
  distinct(State, Clean.Scientific.Name) %>%
  count(State, name = "n_species")
state_shapes$n_species <- species_counts$n_species[match(state_shapes$STATEABB, species_counts$State)]
state_shapes$n_species[is.na(state_shapes$n_species)] <- 0L

us_federal_species <- unique(plants_data$Clean.Scientific.Name[plants_data$State == "US-FED"])
ca_federal_species <- unique(plants_data$Clean.Scientific.Name[plants_data$State == "CA-FED"])
species_choices <- c("All", sort(unique(plants_data$Clean.Scientific.Name)))

# Lambert azimuthal equal-area centred on North America (the projection of the
# source boundaries)
map_proj4 <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +R=6370997 +units=m +no_defs"

# Initial view: the extent of all shapes in projected metres. Leaflet's
# fitBounds() can't be used here: it assumes a lon/lat rectangle stays a
# rectangle, which isn't true in an azimuthal projection, and Alaska's
# Aleutians cross the 180th meridian. fit_extent_js fits this extent instead.
map_bounds <- unname(as.numeric(sf::st_bbox(sf::st_transform(state_shapes, map_proj4))))

# Adds a fitProjectedExtent(xmin, ymin, xmax, ymax, padding) method to leaflet
# maps; callable from R (including leafletProxy) through invokeMethod()
fit_extent_js <- "
  (function register() {
    if (!window.LeafletWidget) { $(register); return; }
    LeafletWidget.methods.fitProjectedExtent = function(xmin, ymin, xmax, ymax, pad) {
      var crs = this.options.crs;
      var size = this.getSize().subtract([2 * pad, 2 * pad]);
      if (size.x <= 0 || size.y <= 0) return;
      var zoom = crs.zoom(Math.min(size.x / (xmax - xmin), size.y / (ymax - ymin)));
      var snap = this.options.zoomSnap || 1;
      var center = crs.projection.unproject(L.point((xmin + xmax) / 2, (ymin + ymax) / 2));
      zoom = Math.floor(zoom / snap) * snap;
      // Zooming out stops just past the full view, so the map can't shrink
      // into a small patch in a sea of gray
      this.setMinZoom(Math.max(0, zoom - 0.3));
      this.setView(center, zoom, { animate: false });
    };
  })();
"

# Dropdown choices, grouped by country (DC has no list of its own)
selectable_shapes <- sf::st_drop_geometry(state_shapes)[state_shapes$STATEABB != "US-DC", ]
state_map <- data.frame(
  StateName = selectable_shapes$NAME,
  StateAbbr = selectable_shapes$STATEABB,
  Country = selectable_shapes$COUNTRY,
  stringsAsFactors = FALSE
)
state_name_to_abbr <- setNames(state_map$StateAbbr, state_map$StateName)
state_choices <- c(
  list("All" = "All"),
  lapply(split(state_map$StateName, state_map$Country)[c("United States", "Canada")], sort)
)

# Map styling: the equal-area projection above, no basemap tiles, shaded in
# the site's red palette
map_crs <- leafletCRS(
  crsClass = "L.Proj.CRS", code = "NA-LAEA",
  proj4def = map_proj4,
  resolutions = 2^(16:7)
)
no_data_color <- "#e2e2e2"
regulated_color <- "#881c1c"
selected_outline_color <- "#2c2c2c"
count_breaks <- c(-Inf, 0, 25, 50, 100, 150, Inf)
count_labels <- c("None listed", "1-25", "26-50", "51-100", "101-150", "151+")
count_colors <- c(no_data_color, "#fcbba1", "#fc9272", "#ef5a43", "#cb2a24", "#881c1c")

fit_to_bbox <- function(map, bb) {
  invokeMethod(map, NULL, "fitProjectedExtent", bb[1], bb[2], bb[3], bb[4], 8)
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
    reg_level <- unname(levels_by_state[state_shapes$STATEABB])
    regulated <- !is.na(reg_level)
    fill <- ifelse(regulated, regulated_color, no_data_color)
    # Places with no list in the dataset get their own wording, since we can't
    # say the species is unregulated there
    labels <- ifelse(
      regulated,
      sprintf("<strong>%s</strong><br>Regulated: %s", state_names, htmltools::htmlEscape(reg_level)),
      ifelse(state_shapes$n_species == 0,
             sprintf("<strong>%s</strong><br>No regulated species listed", state_names),
             sprintf("<strong>%s</strong><br>Not regulated", state_names))
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
      data = state_shapes, layerId = ~STATEABB, group = "states",
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
  fit_to_bbox(map, map_bounds)
}

# Notes a species' federal listing (US-FED / CA-FED) for the map caption
federal_note <- function(species) {
  in_us <- species %in% us_federal_species
  in_ca <- species %in% ca_federal_species
  if (in_us && in_ca) " (also federally regulated in the U.S. and Canada)"
  else if (in_us) " (also federally regulated in the U.S.)"
  else if (in_ca) " (also federally regulated in Canada)"
}

clean_colnames <- function(df) {
  df %>%
    rename(
      `State/Province` = State,
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
  tags$script(HTML(fit_extent_js)),

  div(
    class = "app-header",
    div(
      class = "header-container",
      h1(tags$i(class = "fas fa-seedling"), " State- and Province-Based Regulated Plant Data Viewer"),
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
        p("Choose a U.S. state or Canadian province from the dropdown to create a list of all regulated species there. Choose a species from the dropdown to create a list of all states and provinces where that species is regulated. The map will highlight your selections, and the table below will update."),
        p(
          "Data sources: U.S. state lists were compiled from the ",
          tags$a(href = "https://www.nationalplantboard.org/state-law--regulation-summaries.html", "National Plant Board", target = "_blank"),
          ", the U.S. federal list from ",
          tags$a(href = "https://www.invasive.org/species/list.cfm?id=16", "Invasive.org", target = "_blank"),
          ", and Canadian provincial lists from the ",
          tags$a(href = "https://ccipr.ca/canadian-invasive-plant-lists/", "Canadian Council on Invasive Plant Regulation (CCIPR)", target = "_blank"),
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
                tags$i(class = "fas fa-map-marker-alt"), " Choose a State or Province:"
              ),
              selectInput("selected_state", label = NULL,
                choices = state_choices, width = "100%"
              )
            ),
            div(
              class = "form-group",
              tags$label(
                `for` = "selected_species",
                tags$i(class = "fas fa-leaf"), " Choose a Species:"
              ),
              selectInput("selected_species", label = NULL,
                choices = species_choices, width = "100%"
              )
            ),
            actionButton("reset_btn", "Reset Filters",
              icon = icon("refresh"), class = "btn-reset"
            ),
            p(class = "filter-hint",
              tags$i(class = "fas fa-hand-pointer"), " Tip: click a state or province on the map to select it.")
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
            leafletOutput("state_map", height = 520),
            p(class = "map-note",
              "Boundaries: Commission for Environmental Cooperation, North American Environmental Atlas (2022). Hawaii and Puerto Rico are shown in insets, enlarged and repositioned.")
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
          "Singh, K., J. Salva, M. Fertakos, and B.A. Bradley. RISCC Tools: State- and province-based regulated plant data viewer. URL: https://www.riscctools.org/regulatory-visualization/, access date."
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
    leafletProxy("state_map") %>% fit_to_bbox(map_bounds)
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
          th('State/Province ', span(class="info-icon", '?',
            span(class="tooltip-text", 'Where the plant is regulated, as country-region code: US- for U.S. states and territories (e.g., US-MA), CA- for Canadian provinces (e.g., CA-ON). US-FED and CA-FED are the U.S. and Canadian federal lists.'))),
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
      rownames = FALSE
    )
  })

  output$state_map <- renderLeaflet({
    leaflet(options = leafletOptions(
      crs = map_crs, zoomSnap = 0.1, zoomDelta = 0.5,
      minZoom = 1, maxZoom = 8
    )) %>%
      addMapPane("selected", zIndex = 450) %>%
      addPolygons(
        data = inset_frames, group = "insets",
        fill = FALSE, color = "#8a8a8a", weight = 1, opacity = 1,
        options = pathOptions(interactive = FALSE)
      ) %>%
      draw_states("All", "All")
  })

  observeEvent(list(input$selected_state, input$selected_species), {
    leafletProxy("state_map") %>%
      draw_states(input$selected_state, input$selected_species)
  }, ignoreInit = TRUE)

  # Clicking a state or province selects it; clicking the selected state again clears it
  observeEvent(input$state_map_shape_click, {
    clicked <- state_shapes$NAME[match(input$state_map_shape_click$id, state_shapes$STATEABB)]
    if (is.na(clicked) || !(clicked %in% state_map$StateName)) return()
    ga_event("map_click_state", list(state = clicked))
    updateSelectInput(session, "selected_state",
      selected = if (identical(input$selected_state, clicked)) "All" else clicked)
  })

  output$map_caption <- renderUI({
    species <- input$selected_species
    caption <- if (!is.null(species) && species != "All") {
      tagList(
        "States and provinces regulating ", tags$em(species),
        federal_note(species)
      )
    } else {
      "Number of regulated plant species by state and province"
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