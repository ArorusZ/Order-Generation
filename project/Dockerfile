FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

RUN R -e "install.packages(c('shiny', 'later'), repos='https://cran.rstudio.com/')"

RUN echo 'run_as shiny;\n\
server {\n\
  listen 3838;\n\
  location / {\n\
    site_dir /srv/shiny-server;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}' > /etc/shiny-server/shiny-server.conf

RUN rm -f /srv/shiny-server/index.html
RUN rm -rf /srv/shiny-server/sample-apps

COPY . /srv/shiny-server/

RUN mkdir -p /srv/shiny-server/data

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
