FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

RUN R -e "install.packages(c('sFROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

RUN Rscript -e "install.packages('shiny', repos='https://cran.rstudio.com/', dependencies=TRUE)" && \
    Rscript -e "install.packages('later', repos='https://cran.rstudio.com/', dependencies=TRUE)" && \
    Rscript -e "library(shiny); cat('shiny OK\n')" && \
    Rscript -e "library(later); cat('later OK\n')"

COPY . /app

WORKDIR /app

RUN mkdir -p /app/data

EXPOSE 3838

CMD ["Rscript", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]hiny', 'later'), repos='https://cran.rstudio.com/')"

COPY . /app

WORKDIR /app

RUN mkdir -p /app/data

EXPOSE 3838

CMD ["Rscript", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
