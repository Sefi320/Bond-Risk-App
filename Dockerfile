FROM rocker/shiny-verse:latest


RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libquantlib0-dev

# Install R packages
RUN R -e "install.packages(c('tidyquant', 'RQuantLib', 'arrow', 'bslib', 'corrplot', 'DT', 'plotly', 'slider'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')"


COPY Bond-Risk-App/ /srv/shiny-server/


EXPOSE 3838

CMD ["/usr/bin/shiny-server"]