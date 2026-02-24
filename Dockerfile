# Use official R Shiny image
FROM rocker/shiny:latest

# Set timezone
ENV TZ=Australia/Brisbane

# Install system dependencies for R packages like plotly
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libxt-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libglib2.0-dev \
    libgtk2.0-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('shiny', 'dplyr', 'ggplot2','shinyjs', 'DT','plotly','shinymanager'), repos='https://cloud.r-project.org/')"

# Verify that all required packages are installed
RUN R -e "pkgs <- c('shiny','dplyr','ggplot2','shinyjs','DT','plotly','shinymanager'); missing <- pkgs[!pkgs %in% rownames(installed.packages())]; if(length(missing)) stop(paste('Missing packages:', paste(missing, collapse=','))) else cat('All packages installed successfully\n')"

# Remove default example apps from rocker/shiny
RUN rm -rf /srv/shiny-server/*

# Copy your app into the container
COPY . /srv/shiny-server/

# Make sure permissions are correct
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port 3838
EXPOSE 3838

# Start Shiny
CMD ["/usr/bin/shiny-server"]
