FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

RUN R -e "install.packages(c('shiny', 'later'), repos='https://cran.rstudio.com/')"

COPY . /srv/shiny-server/

RUN mkdir -p /srv/shiny-server/data

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
