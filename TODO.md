# Cleanup TODO

Follow-ups noted on 2026-10-13, after Abundance Visualization was unpublished and the regulatory map was redesigned.

- [ ] **Upload `state_shapes.rds` to the shared Google Drive folder.** The regulatory map now needs `shiny-apps/regulatory-visualization/data/state_shapes.rds`, and data folders are git-ignored. Until it's on the Drive, collaborators have to build it with `scripts/build_state_shapes.R` (see the README).
- [ ] **Delete the obsolete `state_polygons.rds`** from `shiny-apps/regulatory-visualization/data/`. The app no longer reads it.
- [ ] **Remove Shiny Server's example apps from the image.** `rocker/shiny` ships demos in `/srv/shiny-server` (`01_hello`, `sample-apps`, ...), and they are publicly reachable, e.g. `https://www.riscctools.org/01_hello/`. Add `RUN rm -rf /srv/shiny-server/*` before the app `COPY` lines in the `Dockerfile`.
- [ ] **Pin the base image.** The `Dockerfile` uses `rocker/shiny:latest`, so R and package versions can change underneath a rebuild. Pin a version (C-SNaP uses `rocker/shiny:4.5.3`).
- [ ] **Decide whether abundance visualization is coming back.** If not, delete `shiny-apps/abundance-visualization/`, the commented-out lines in `Dockerfile` and `nginx/conf.d/default.conf`, its sections in `scripts/preprocess_data.R`, its README mentions, and the `--species-abundance` token in the shared CSS.
- [ ] **Trim unused R packages and system libraries.** The regulatory app loads tidyverse, leaflet, readxl, shiny, shinyjs, DT, readr and sf. **`sf` is now required.** No app uses `httr`, `jsonlite`, `shiny.fluent`, `leafpop`, `duckdb`, `DBI`, `rrapply`, `shinydashboard`, `bslib` or `shinyWidgets` now that abundance is unpublished, and the Rust install isn't needed either. Dropping `dependencies=TRUE` (which installs every suggested package) would shrink the image further.
- [ ] **Fill in the README deploy placeholders.** The ECR repository is `riscc-tools` (us-east-2), and the ECS service is `riscc-tools-service` in cluster `default`.
- [ ] **Add an ECR lifecycle policy.** The repo has none, so every push leaves the previous ~6 GB image behind untagged. Keeping the last 3-5 images would preserve rollback while capping storage.
