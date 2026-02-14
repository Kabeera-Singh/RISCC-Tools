#!/usr/bin/env Rscript
# RISCC-Tools Data Preprocessing Script
# Run from project root: Rscript scripts/preprocess_data.R
# Converts CSV to RDS, pre-computes derived data, and creates lookup indexes.

library(readr)
library(dplyr)
library(maps)

# Run from project root: Rscript scripts/preprocess_data.R
if (!file.exists("shiny-apps")) {
  stop("Run this script from the RISCC-Tools project root directory.")
}

abundance_data_dir <- "shiny-apps/abundance-visualization/data"
regulatory_data_dir <- "shiny-apps/regulatory-visualization/data"

# Ensure data directories exist
dir.create(abundance_data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(regulatory_data_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Abundance Visualization Data ----

s2_csv <- file.path(abundance_data_dir, "appendixS2.csv")
if (file.exists(s2_csv)) {
  message("Processing abundance app data...")
  s2 <- read_csv(s2_csv, show_col_types = FALSE)

  # Prune to only needed columns (memory optimization)
  needed_cols <- c("Orig.Genus.species", "x", "y", "NA_L1KEY", "NA_L3KEY", "commonness",
                  "Dataset", "Year", "Qualitative", "PctCov", "AvgCovClass")
  s2_slim <- s2 %>% select(any_of(needed_cols))

  # Save slim RDS
  saveRDS(s2_slim, file.path(abundance_data_dir, "appendixS2.rds"))
  message("  Saved appendixS2.rds")

  # Pre-compute filtered forest/abundant data
  s2_abun <- s2_slim %>%
    filter(NA_L1KEY %in% c("8  EASTERN TEMPERATE FORESTS", "5  NORTHERN FORESTS")) %>%
    filter(grepl("Abundant", commonness))

  final_unique <- sort(unique(s2_abun$Orig.Genus.species))
  saveRDS(s2_abun, file.path(abundance_data_dir, "s2_abun.rds"))
  saveRDS(final_unique, file.path(abundance_data_dir, "species_list.rds"))
  message("  Saved s2_abun.rds, species_list.rds")

  # Create species index for O(1) lookup
  species_index <- split(s2_slim, s2_slim$Orig.Genus.species)
  saveRDS(species_index, file.path(abundance_data_dir, "species_index.rds"))
  message("  Saved species_index.rds")
} else {
  message("Skipping abundance data: appendixS2.csv not found")
}

# ---- 2. L3_list and L3_index (requires s7_merged.rds) ----

s7_path <- file.path(abundance_data_dir, "s7_merged.rds")
if (file.exists(s7_path)) {
  message("Processing s7/L3 data...")
  s7 <- readRDS(s7_path)

  L3_list <- data.frame(
    Species = s7$USDA.Genus.species,
    NA_L3KEY = s7$NA_L3KEY,
    stringsAsFactors = FALSE
  )

  saveRDS(L3_list, file.path(abundance_data_dir, "L3_list.rds"))

  L3_index <- split(L3_list, L3_list$NA_L3KEY)
  saveRDS(L3_index, file.path(abundance_data_dir, "L3_index.rds"))
  message("  Saved L3_list.rds, L3_index.rds")
} else {
  message("Skipping L3 data: s7_merged.rds not found")
}

# ---- 3. Regulatory Visualization Data ----

plants_csv <- file.path(regulatory_data_dir, "Final Regulated Plants by State June 2025 - MergedPlantsByState.csv")
if (file.exists(plants_csv)) {
  message("Processing regulatory app data...")
  plants <- read_csv(plants_csv, show_col_types = FALSE)
  saveRDS(plants, file.path(regulatory_data_dir, "plants_data.rds"))
  message("  Saved plants_data.rds")
} else {
  message("Skipping regulatory data: CSV not found")
}

# ---- 4. Pre-compute State Polygons ----

message("Pre-computing state polygons...")
state_polygons <- list()
for (state in tolower(state.name)) {
  poly <- tryCatch(
    maps::map("state", regions = state, plot = FALSE, fill = TRUE),
    error = function(e) NULL
  )
  if (!is.null(poly) && length(poly$x) > 0) {
    state_polygons[[state]] <- list(x = poly$x, y = poly$y, type = "polygon")
  }
}
# Add Puerto Rico as point (no polygon in maps package)
state_polygons[["puerto rico"]] <- list(x = -66.5, y = 18.2, type = "point")

saveRDS(state_polygons, file.path(regulatory_data_dir, "state_polygons.rds"))
message("  Saved state_polygons.rds")

message("Preprocessing complete.")
