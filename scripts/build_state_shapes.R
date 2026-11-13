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
# Hawaii and Puerto Rico are drawn enlarged (below), so they keep more detail.
# They share no borders, so simplifying them separately leaves no gaps.
simplify <- function(x, keep, tolerance) {
  if (requireNamespace("rmapshaper", quietly = TRUE)) {
    rmapshaper::ms_simplify(x, keep = keep, keep_shapes = TRUE)
  } else {
    sf::st_simplify(x, dTolerance = tolerance, preserveTopology = TRUE)
  }
}
if (!requireNamespace("rmapshaper", quietly = TRUE)) {
  message("  rmapshaper not installed; using sf::st_simplify")
}
is_inset <- shapes$STATEABB %in% c("US-HI", "US-PR")
shapes <- rbind(simplify(shapes[!is_inset, ], keep = 0.02, tolerance = 3000),
                simplify(shapes[is_inset, ], keep = 0.15, tolerance = 500))

# Hawaii and Puerto Rico become insets next to the lower 48: enlarged so they
# can be seen and clicked, turned north-up (the projection tilts places far
# from its centre), and framed. The source CRS is Lambert azimuthal
# equal-area in metres, so these are plain affine moves. Alaska and Canada
# stay where they are.
make_inset <- function(shapes, abb, x, y, scale) {
  i <- which(shapes$STATEABB == abb)
  geom <- sf::st_geometry(shapes)
  centre <- sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(geom[i]))))[1, ]

  # Angle between local north and the map's up direction
  centre_ll <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(centre), crs = sf::st_crs(shapes)), 4326))[1, ]
  north <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(centre_ll + c(0, 0.5)), crs = 4326), sf::st_crs(shapes)))[1, ]
  theta <- atan2(north[1] - centre[1], north[2] - centre[2])
  rot <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2) * scale

  geom[i] <- (geom[i] - centre) * rot + c(x, y)
  sf::st_geometry(shapes) <- sf::st_set_crs(geom, sf::st_crs(shapes))
  shapes
}

# Frame around an inset, padded by pad metres. Only the four corners are
# needed: Leaflet projects each vertex and draws straight lines between them.
inset_frame <- function(shapes, abb, pad) {
  bb <- sf::st_bbox(shapes[shapes$STATEABB == abb, ])
  sf::st_as_sfc(sf::st_bbox(c(xmin = bb[["xmin"]] - pad, ymin = bb[["ymin"]] - pad,
                              xmax = bb[["xmax"]] + pad, ymax = bb[["ymax"]] + pad),
                            crs = sf::st_crs(shapes)))
}

# Keep only Hawaii's main islands; the Northwestern Hawaiian Islands stretch
# ~2,500 km and would be invisible specks anyway.
hi <- shapes$STATEABB == "US-HI"
main_islands <- sf::st_transform(sf::st_as_sfc(sf::st_bbox(
  c(xmin = -160.6, ymin = 18.5, xmax = -154.5, ymax = 22.5), crs = 4326)), sf::st_crs(shapes))
sf::st_geometry(shapes)[hi] <- sf::st_intersection(sf::st_geometry(shapes)[hi], main_islands)

shapes <- make_inset(shapes, "US-HI", x = -1.55e6, y = -2.05e6, scale = 1.8) # off southern California
shapes <- make_inset(shapes, "US-PR", x =  0.85e6, y = -2.27e6, scale = 3.0) # Gulf of Mexico

frames <- sf::st_sf(
  NAME = c("Hawaii", "Puerto Rico"),
  geometry = c(inset_frame(shapes, "US-HI", 90e3), inset_frame(shapes, "US-PR", 70e3))
)
frames_out <- file.path(regulatory_data_dir, "inset_frames.rds")
saveRDS(sf::st_transform(frames, 4326), frames_out)
message("  Saved ", frames_out)

shapes <- sf::st_make_valid(sf::st_transform(shapes, 4326))
shapes$COUNTRY <- unname(c(USA = "United States", CAN = "Canada")[shapes$COUNTRY])
shapes <- shapes[order(shapes$COUNTRY, shapes$NAME), ]
rownames(shapes) <- NULL

out <- file.path(regulatory_data_dir, "state_shapes.rds")
saveRDS(shapes, out)
message("  Saved ", out, " (", nrow(shapes), " features, ",
        round(file.size(out) / 1024), " KB)")
