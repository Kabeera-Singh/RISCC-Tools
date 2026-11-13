# Changelog

## 2026-11-12: Regulatory tool covers the U.S. and Canada

### Data
- The **July 2026** regulated plants spreadsheet (`Final Regulated Plants by State July 2026.xlsx`, sheet `MergedPlantsByState`) replaces the June 2025 CSV. It has 3,418 rows covering 49 U.S. jurisdictions, 7 Canadian provinces (AB, BC, MB, NS, ON, PE, SK), and the U.S. and Canadian federal lists.
- The location column is now `State/Province`, with codes like `US-MA`, `CA-ON`, `US-FED` and `CA-FED`. Downloads use that header. Inside the app the column is still called `State`.
- The map outlines come from the CEC North American Environmental Atlas (2022) "North America simplified" shapefile, matched on `STATEABB`. They replace the Census state outlines, so `tigris` and internet access are no longer needed.

### App (`shiny-apps/regulatory-visualization/app.R`)
- **Map:** shows the U.S. and Canada in a North America equal-area projection. Alaska sits in its real place, Hawaii (main islands) and Puerto Rico are moved in near the lower 48, and Mexico is not drawn. The initial view fits the projected extent through a small Leaflet method (`fitProjectedExtent`), because `fitBounds()` mis-frames azimuthal projections.
- **State/province dropdown:** grouped into "United States" and "Canada" and built from the shapes. Clicking any province or state selects it.
- **Federal lists:** the map caption says whether a species is also federally regulated in the U.S., in Canada, or in both.
- **Map labels:** in species mode, places with no list in the dataset (e.g. Quebec, Georgia) say "No regulated species listed" instead of "Not regulated".
- **Text:** the title, instructions, data sources (National Plant Board, Invasive.org, CCIPR), table tooltip, boundary credit and citation all mention states and provinces.
- **Small cleanups:** the species list is computed once at startup, table cells are HTML-escaped, and the unused `readr` import is gone.

### Build and deploy
- `scripts/build_state_shapes.R` reads the shapefile, keeps the U.S. and Canada, simplifies with `rmapshaper`, repositions HI and PR, and writes `state_shapes.rds` (~140 KB).
- `scripts/preprocess_data.R` reads the xlsx, renames the column, and warns about any code without a map outline.
- `Dockerfile`:
  - pinned to `rocker/shiny:4.5.3`
  - Shiny Server's public demo apps (`/01_hello/` etc.) removed
  - installs only the 7 packages the app loads (`sf` is now explicit)
  - no Rust, no `dependencies=TRUE`
- New `.dockerignore` keeps the raw shapefile, old CSV, notebooks and the unpublished abundance app out of the image.
- `README.MD`: covers the new data files, the shape build, and the ECR/ECS names and commands.

### Still to do (see `TODO.md`)
- Upload the xlsx, the shapefile zip and `state_shapes.rds` to the shared Google Drive folder.
- Delete the old local `state_polygons.rds` and the June 2025 CSV.
- Add an ECR lifecycle policy.

### Rebuild from scratch
```bash
Rscript scripts/build_state_shapes.R   # needs sf (+ rmapshaper)
Rscript scripts/preprocess_data.R      # needs readxl
docker build --platform linux/amd64 -t riscc-tools .
```
