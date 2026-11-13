# Cleanup TODO

Follow-ups noted on 2026-10-13, after Abundance Visualization was unpublished and the regulatory map was redesigned. Updated 2026-11-12 with the U.S. + Canada data update.

- [ ] **Upload the regulatory data files to the shared Google Drive folder.** Data folders are git-ignored, so collaborators need these from the Drive: `Final Regulated Plants by State July 2026.xlsx`, the `North America simplified` shapefile (zip), and the built `state_shapes.rds`. Without `state_shapes.rds` they have to build it with `scripts/build_state_shapes.R` (see the README).
- [ ] **Delete the obsolete local files** in `shiny-apps/regulatory-visualization/data/`: `state_polygons.rds` and the June 2025 CSV. The app no longer reads them, and `.dockerignore` keeps them out of the image.
- [x] **Remove Shiny Server's example apps from the image.** Done 2026-11-12 (`RUN rm -rf /srv/shiny-server/*` in the `Dockerfile`).
- [x] **Pin the base image.** Done 2026-11-12 (`rocker/shiny:4.5.3`).
- [ ] **Decide whether abundance visualization is coming back.** If not, delete `shiny-apps/abundance-visualization/`, the commented-out lines in `Dockerfile` and `nginx/conf.d/default.conf`, its sections in `scripts/preprocess_data.R`, its README mentions, and the `--species-abundance` token in the shared CSS. If it is, add the R packages its `app.R` loads back to the `Dockerfile` (they were trimmed on 2026-11-12) and remove it from `.dockerignore`.
- [x] **Trim unused R packages and system libraries.** Done 2026-11-12: the image installs only shiny, tidyverse, readxl, shinyjs, DT, leaflet and sf, without `dependencies=TRUE`, and the Rust install is gone.
- [x] **Fill in the README deploy placeholders.** Done 2026-11-12 (ECR repo `riscc-tools`, ECS service `riscc-tools-service` in cluster `default`; the account ID stays a placeholder).
- [ ] **Add an ECR lifecycle policy.** The repo has none, so every push leaves the previous ~6 GB image behind untagged. Keeping the last 3-5 images would preserve rollback while capping storage.
