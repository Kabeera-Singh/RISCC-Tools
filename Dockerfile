FROM rocker/shiny:latest

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
    # Rust dependencies for gifski package
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install Rust (required for gifski package used by climateR)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Set environment variables for spatial libraries
ENV GDAL_CONFIG=/usr/bin/gdal-config
ENV PROJ_LIB=/usr/share/proj

# Install basic CRAN packages first (excluding problematic spatial ones)
RUN R -e "install.packages(c('shiny', 'tidyverse', 'shinyWidgets', 'DT', 'stringr', 'readxl', 'shinyjs', 'rrapply', 'data.table', 'shinydashboard', 'bslib', 'dplyr', 'ggplot2', 'remotes','DBI','duckdb'), repos='https://cran.rstudio.com/', dependencies=TRUE)"
RUN R -e "install.packages(c('httr','jsonlite'), repos='https://cran.rstudio.com/', dependencies=TRUE)"

# Install remaining spatial packages
RUN R -e "install.packages(c('leaflet', 'leafpop','shiny.fluent'), repos='https://cran.rstudio.com/', dependencies=TRUE)"

# Copy Shiny Server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copy nginx configuration
COPY nginx/conf.d/default.conf /etc/nginx/nginx.conf

# Copy HTML files
COPY nginx/html/ /usr/share/nginx/html/

# Copy Shiny apps
COPY shiny-apps/abundance-visualization/ /srv/shiny-server/abundance-visualization/
COPY shiny-apps/regulatory-visualization/ /srv/shiny-server/regulatory-visualization/

# Copy the shared colors CSS file to both apps
COPY nginx/html/www/css/shared-colors.css /srv/shiny-server/abundance-visualization/www/shared-colors.css
COPY nginx/html/www/css/shared-colors.css /srv/shiny-server/regulatory-visualization/www/shared-colors.css

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