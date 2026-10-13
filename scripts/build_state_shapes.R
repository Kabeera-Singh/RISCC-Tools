#!/usr/bin/env Rscript
# Builds the state outlines used by the regulatory visualization map.
# Run from project root: Rscript scripts/build_state_shapes.R
# Requires internet access (downloads U.S. Census cartographic boundaries) and
# the sf + tigris packages.

library(sf)
library(tigris)

if (!file.exists("shiny-apps")) {
  stop("Run this script from the RISCC-Tools project root directory.")
}

regulatory_data_dir <- "shiny-apps/regulatory-visualization/data"
dir.create(regulatory_data_dir, showWarnings = FALSE, recursive = TRUE)

message("Downloading Census state boundaries (1:20m)...")
states_sf <- tigris::states(cb = TRUE, resolution = "20m", year = 2023, progress_bar = FALSE)
states_sf <- states_sf[states_sf$STUSPS %in% c(state.abb, "DC", "PR"), c("NAME", "STUSPS")]

# Move Alaska, Hawaii and Puerto Rico below the lower 48 so the whole U.S. fits
# in one view; the app draws these in an Albers projection without a basemap.
states_sf <- tigris::shift_geometry(states_sf, position = "below", preserve_area = FALSE)
states_sf <- sf::st_make_valid(sf::st_transform(states_sf, 4326))

saveRDS(states_sf, file.path(regulatory_data_dir, "state_shapes.rds"))
message("  Saved state_shapes.rds (", nrow(states_sf), " features)")
