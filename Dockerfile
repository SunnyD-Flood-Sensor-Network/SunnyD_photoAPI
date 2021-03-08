FROM rocker/r-ver:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    odbc-postgresql \
    libmagick++-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# install renv & restore packages
RUN install2.r lubridate dplyr DBI RPostgres dbx pool plumber dbplyr magick raster httr jsonlite googledrive exifr stringr

RUN groupadd -r plumber && useradd --no-log-init -r -g plumber plumber

ADD plumber.R /home/plumber/plumber.R
ADD entrypoint.R /home/plumber/entrypoint.R

EXPOSE 8000
EXPOSE 5432

WORKDIR /home/plumber
USER plumber
CMD Rscript entrypoint.R