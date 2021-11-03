library(lubridate)
library(dplyr)
library(RPostgres)
library(DBI)
library(dbx)
library(pool)
library(plumber)
library(dbplyr)
library(magick)
library(httr)
library(jsonlite)
library(googledrive)
library(exifr)
library(stringr)

# source("/Users/adam/Documents/SunnyD/sunnyday_postgres_keys.R")

# Connect to database
con <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("POSTGRESQL_DATABASE"),
  host = Sys.getenv("POSTGRESQL_HOSTNAME"),
  port = Sys.getenv("POSTGRESQL_PORT"),
  password = Sys.getenv("POSTGRESQL_PASSWORD"),
  user = Sys.getenv("POSTGRESQL_USER")
)

camera_id_list <- con %>%
  tbl("camera_locations") %>%
  pull(camera_ID)

api_keys <- con %>%
  tbl("api_keys") %>%
  pull("keys")

parser_jpeg <- function(...) {
  parser_read_file(function(tmpfile) {
    magick::image_read(tmpfile, ...)
  })
}

register_parser("jpeg", parser_jpeg, fixed = c("image/jpeg", "image/jpg"))

# Login to google drive using service account
googledrive::drive_auth(path = Sys.getenv("GOOGLE_JSON_KEY"))

pr("plumber.R") %>% pr_run(host='0.0.0.0', port = 8000)
