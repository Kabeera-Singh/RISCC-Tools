# Pinned so R and package versions don't shift under a rebuild (each versioned
# rocker image also pins its package repo to a dated snapshot)
FROM rocker/shiny:4.5.3

# Install nginx, supervisor, and additional system dependencies for spatial packages
RUN apt-get update && apt-get install -y \
    nginx \
    supervisor \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    # Additional dependencies for terra and geodata
    libgeos++-dev \
    libgeos-c1v5 \
    gdal-bin \
    proj-bin \
    libproj-dev \
    cmake \
    build-essential \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Set environment variables for spatial libraries
ENV GDAL_CONFIG=/usr/bin/gdal-config
ENV PROJ_LIB=/usr/share/proj

# Packages install from the base image's default repo (Posit Package Manager pre-built
# Ubuntu binaries), which is far faster than compiling from cran.rstudio.com sources.
# Only the packages the published app loads (plus their required dependencies);
# the second command fails the build if any of them didn't install.
# If abundance-visualization comes back, add the packages its app.R loads.
RUN R -e "install.packages(c('shiny', 'tidyverse', 'readxl', 'shinyjs', 'DT', 'leaflet', 'sf'))" && \
    R -e "for (p in c('shiny', 'tidyverse', 'readxl', 'shinyjs', 'DT', 'leaflet', 'sf')) library(p, character.only = TRUE)"

# Copy Shiny Server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copy nginx configuration
COPY nginx/conf.d/default.conf /etc/nginx/nginx.conf

# Copy HTML files
COPY nginx/html/ /usr/share/nginx/html/
COPY ui/shared/css/ /usr/share/nginx/html/www/css/shared/

# Remove Shiny Server's bundled demo apps (e.g. /01_hello/) so they aren't public
RUN rm -rf /srv/shiny-server/*

# Copy Shiny apps
# Abundance visualization unpublished 2026-10-13
# COPY shiny-apps/abundance-visualization/ /srv/shiny-server/abundance-visualization/
COPY shiny-apps/regulatory-visualization/ /srv/shiny-server/regulatory-visualization/
# Abundance visualization unpublished 2026-10-13
# COPY ui/shared/css/ /srv/shiny-server/abundance-visualization/www/css/shared/
COPY ui/shared/css/ /srv/shiny-server/regulatory-visualization/www/css/shared/

# Create supervisor configuration
RUN mkdir -p /var/log/supervisor
COPY supervisor.conf /etc/supervisor/conf.d/supervisord.conf

# Set proper permissions
RUN chown -R shiny:shiny /srv/shiny-server/ && \
    chown -R www-data:www-data /usr/share/nginx/html && \
    mkdir -p /var/log/nginx && \
    touch /var/log/nginx/access.log /var/log/nginx/error.log && \
    chown www-data:www-data /var/log/nginx/*

# Expose port 80 (nginx will handle routing)
EXPOSE 80

# Use supervisor to run both nginx and shiny-server
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]