FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev

RUN Rscript -e "install.packages('later', repos='https://cran.rstudio.com/', dependencies=TRUE)"

RUN Rscript -e "install.packages('dplyr', repos='https://cran.rstudio.com/', dependencies=TRUE)"

RUN rm -rf /srv/shiny-server/*

COPY . /srv/shiny-server/

RUN ls -la /srv/shiny-server/

WORKDIR /srv/shiny-server

RUN mkdir -p /srv/shiny-server/data

EXPOSE 3838

CMD ["Rscript", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
