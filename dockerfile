FROM openanalytics/r-base

MAINTAINER Scuderi Renzo "renzo.scuderi@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# basic shiny functionality and packages used
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'lpSolveAPI', 'markdown', 'ggplot2', 'gridExtra', 'dplyr', 'ggfortify'), repos='https://cloud.r-project.org/')"


# copy the app to the image
RUN mkdir foodtruck
COPY foodtruck foodtruck


EXPOSE 3838

# run the app
CMD ["R", "-e", "shiny::runApp('foodtruck', port = 3838,host = '0.0.0.0')"]
