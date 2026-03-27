FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev

RUN Rscript -e "install.packages('shiny', repos='https://cran.rstudio.com/', dependencies=TRUE)"

RUN Rscript -e "install.packages('later', repos='https://cran.rstudio.com/', dependencies=TRUE)"

COPY . /app

WORKDIR /app

RUN mkdir -p /app/data

EXPOSE 3838

CMD ["Rscript", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]