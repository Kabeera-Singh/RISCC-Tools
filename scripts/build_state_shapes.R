#!/usr/bin/env Rscript
# Builds the state/province outlines used by the regulatory visualization map.
# Run from project root: Rscript scripts/build_state_shapes.R [path/to/shapefile.shp]
#
# Source: Commission for Environmental Cooperation (CEC). 2022. "North American
# Environmental Atlas - Political Boundaries" (1:10,000,000). Its STATEABB
# codes (US-MA, CA-ON, ...) match the State/Province column of the regulated
# plants spreadsheet. Get the shapefile from the shared Google Drive folder.
#
# Requires sf; rmapshaper is preferred for simplifying (keeps shared borders
# aligned) but the script falls back to sf::st_simplify without it.

library(sf)

if (!file.exists("shiny-apps")) {
  stop("Run this script from the RISCC-Tools project root directory.")
}

regulatory_data_dir <- "shiny-apps/regulatory-visualization/data"
args <- commandArgs(trailingOnly = TRUE)
shp_path <- if (length(args) >= 1) args[1] else
  file.path(regulatory_data_dir, "north-america-shapefile", "North America simplified.shp")
if (!file.exists(shp_path)) stop("Shapefile not found: ", shp_path)

message("Reading ", shp_path, " ...")
shapes <- sf::st_read(shp_path, quiet = TRUE)
# U.S. and Canada only (the data has no Mexican lists); the Virgin Islands are
# too small to see at this scale and have no list.
shapes <- shapes[shapes$COUNTRY %in% c("USA", "CAN") & shapes$STATEABB != "US-VI",
                 c("NAME", "COUNTRY", "STATEABB")]

message("Simplifying ", nrow(shapes), " features...")
if (requireNamespace("rmapshaper", quietly = TRUE)) {
  shapes <- rmapshaper::ms_simplify(shapes, keep = 0.02, keep_shapes = TRUE)
} else {
  message("  rmapshaper not installed; using sf::st_simplify")
  shapes <- sf::st_simplify(shapes, dTolerance = 3000, preserveTopology = TRUE)
}

# Move Hawaii and Puerto Rico in next to the lower 48 so the map stays compact.
# The source CRS is Lambert azimuthal equal-area in metres, so a plain offset
# keeps their shape. Alaska and Canada stay where they are.
# Each one is moved so its bounding-box centre lands on the given point (metres).
move_feature_to <- function(shapes, abb, x, y) {
  i <- which(shapes$STATEABB == abb)
  geom <- sf::st_geometry(shapes)
  bb <- sf::st_bbox(geom[i])
  geom[i] <- geom[i] + c(x - (bb[["xmin"]] + bb[["xmax"]]) / 2,
                         y - (bb[["ymin"]] + bb[["ymax"]]) / 2)
  sf::st_geometry(shapes) <- sf::st_set_crs(geom, sf::st_crs(shapes))
  shapes
}

# Keep only Hawaii's main islands; the Northwestern Hawaiian Islands stretch
# ~2,500 km and would be invisible specks anyway.
hi <- shapes$STATEABB == "US-HI"
main_islands <- sf::st_transform(sf::st_as_sfc(sf::st_bbox(
  c(xmin = -160.6, ymin = 18.5, xmax = -154.5, ymax = 22.5), crs = 4326)), sf::st_crs(shapes))
sf::st_geometry(shapes)[hi] <- sf::st_intersection(sf::st_geometry(shapes)[hi], main_islands)

shapes <- move_feature_to(shapes, "US-HI", x = -1.65e6, y = -1.85e6) # off southern California
shapes <- move_feature_to(shapes, "US-PR", x =  0.70e6, y = -2.20e6) # Gulf of Mexico

shapes <- sf::st_make_valid(sf::st_transform(shapes, 4326))
shapes$COUNTRY <- unname(c(USA = "United States", CAN = "Canada")[shapes$COUNTRY])
shapes <- shapes[order(shapes$COUNTRY, shapes$NAME), ]
rownames(shapes) <- NULL

out <- file.path(regulatory_data_dir, "state_shapes.rds")
saveRDS(shapes, out)
message("  Saved ", out, " (", nrow(shapes), " features, ",
        round(file.size(out) / 1024), " KB)")
