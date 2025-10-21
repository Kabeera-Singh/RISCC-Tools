# Nginx Docker Shiny Apps

A Dockerized setup for hosting multiple Shiny applications using Nginx as a reverse proxy.

## Components
- **Nginx**: Reverse proxy routing requests to Shiny apps
- **Shiny Server**: Hosts R Shiny applications  
- **Docker**: Containerizes the entire setup

## Prerequisites
- Docker Desktop
- Visual Studio Code (for modifications)
- Accounts: Docker Hub, GitHub, Render (for deployment)

## Quick Start (No Modifications)

Run the pre-built image locally:

```bash
docker pull kabeerasingh/ngix-webpage
docker run -p 80:80 kabeerasingh/ngix-webpage
```

Open http://localhost in your browser.

## Development Setup

### 1. Clone Repository
```bash
git clone https://github.com/Kabeera-Singh/ngix-RISCC-app.git
cd ngix-RISCC-app
```

### 2. Install Data Files
**TODO**: Add instructions for installing data files from Google Drive folder

## Adding New Apps

### 1. Create App Directory
Create a folder in `shiny_apps/` for your new app (e.g., `shiny_apps/my-new-app/`)

### 2. Add App Files
- Place your Shiny files (`app.R`, `ui.R`, `server.R`) in the new directory
- Use relative file paths only
- Remove any `runApp()` calls - use `shinyApp(ui = ui, server = server)` instead

### 3. Update Configuration Files

**In VS Code, search for `plant-selection` and update these files:**

**Dockerfile** - Add this line:
```dockerfile
COPY shiny-apps/YOUR_APP_NAME/ /srv/shiny-server/YOUR_APP_NAME/
```

**nginx/conf.d/default.conf** - Copy the `plant-selection` location block and rename:
```nginx
# Location for YOUR_APP_NAME
location /YOUR_APP_NAME/ {
    proxy_pass http://shiny:3838/YOUR_APP_NAME/;
    # ... rest of proxy settings
}
```

**nginx/index.html** - Copy `plant-selection`'s href links and app-block section, then rename to your app

### 4. Add R Packages (if needed)
Add required packages to the R package installation section in the Dockerfile.

### 5. Build and Test

**Local testing:**
```bash
docker build -t nginx-shiny-app .
docker run -p 80:80 nginx-shiny-app
```

**Production build:**
```bash
docker build -t kabeerasingh/ngix-webpage .
docker push kabeerasingh/ngix-webpage
```

## Deployment

### Render Deployment
1. Create new web service on Render
2. Connect your Docker Hub image: `kabeerasingh/ngix-webpage`
3. Set port to 80
4. Deploy

### Port Configuration

- Shiny Apps: Running on port 3838 (internal)
- Nginx Reverse Proxy: Running on port 80 (external)

The port exposed to the outside world is port 80, and Nginx routes requests to the appropriate Shiny app based on the URL.