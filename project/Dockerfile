FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'later','dplyr'), repos='https://cran.rstudio.com/')"

# Copy all app files
COPY . /srv/shiny-server/

# Make sure data folder exists
RUN mkdir -p /srv/shiny-server/project/data

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
```