#* @apiTitle SunnyD Flooding Photo API
#* @apiDescription This API contains functions for handling photos from the SunnyD Flooding project, which measures and models urban coastal flooding
#* @apiContact list(name = "API Support", email = "gold@unc.edu")
#* @apiVersion 1.0.0

#----------------- Expose mounted storage "/data" as the "public folder" that will store images sent to API
#* @assets /data
list()

#------------------------- Upload .jpegs or .jpgs -----------------------
#* @post /upload_picture
#* @parser multi
#* @parser jpeg
#* @param key API key
#* @param file:file A file
#* @param camera_ID ID of camera
#* @param timezone EST if not specified
#* Upload a jpeg
function(key, camera_ID, file, timezone = "EST") {
  if (key %in% api_keys & camera_ID %in% camera_id_list) {

    # Write the uploaded file to a temporary file
    tmpfile <- tempfile(fileext = ".jpg")
    magick::image_write(file[[1]], tmpfile)

    # Write a reduced resolution copy to the public facing storage
    magick::image_write(file[[1]] %>% magick::image_scale(geometry ="1000") , paste0("/data/",camera_ID,".jpg"))

    # Read the EXIF data from the picture to see when it was taken and to convert time zones
    exif_tib <- exifr::read_exif(tmpfile,
                                 tags = c("FileName","FileSize","DateTimeOriginal"))

    exif_tib <- exif_tib %>%
      mutate(original_tz = timezone) %>%
      mutate(DateTimeOriginalUTC = lubridate::with_tz(lubridate::ymd_hms(DateTimeOriginal, tz = timezone), tzone = "UTC")) %>%
      mutate(drive_filename = paste0(camera_ID,"_",stringr::str_remove_all(DateTimeOriginalUTC, stringr::regex("\\W+"))), .before = SourceFile) %>%
      mutate(camera_ID = camera_ID, .before = SourceFile) %>%
      mutate(high_water = F)

    # Create the error message object to populate later
    error_message <- NA

    # Check if folder exists already by asking drive
    date_string <- exif_tib %>% pull(DateTimeOriginalUTC) %>% lubridate::as_date() %>% as.character()

    # Get folder ID of the individual camera's folder
    camera_folder_id <- googledrive::drive_get(path = paste0("Images/",camera_ID,"/"),
                                         shared_drive = as_id(Sys.getenv("GOOGLE_DRIVE_FOLDER_ID")),
                                         )

    # Query the camera_folder to see if there is a folder matching the current date
    date_folder_id <- googledrive::drive_ls(pattern = date_string,
                                            type = "folder",
                                            path = as_id(camera_folder_id))

    # If there is no folder for the date of the current picture, make one
    if(nrow(date_folder_id) == 0){
      suppressMessages(date_folder_id <- googledrive::drive_mkdir(name = date_string,
                               path = as_id(camera_folder_id),
                               overwrite = F))
    }

    # If the current photo name is not in the database, upload it to the cloud!
    if(!exif_tib$drive_filename %in% c(con %>% tbl("photo_info") %>% pull(drive_filename))){
      suppressMessages(error_message <- try(
      googledrive::drive_upload(media = tmpfile,
                   path = as_id(date_folder_id),
                   name =  paste0(exif_tib$drive_filename,".jpg"))
      )
      )
    }

    # If we don't have an error message - the "error_message" object is a "dribble" indicating a succesful upload, write the filename to the database
    if(googledrive::is_dribble(error_message)) {
      dbx::dbxUpsert(
        conn = con,
        table = "photo_info",
        records = exif_tib,
        where_cols = c("drive_filename"),
        skip_existing = T
      )

      unlink(tmpfile)
      rm(tmpfile)
      unlink(file)
      rm(file)
      rm(exif_tib)
      rm(error_message)
      rm(date_string)
      rm(camera_folder_id)
      rm(date_folder_id)

      return("SUCCESS!")
    }

    if(!googledrive::is_dribble(error_message)) {
      unlink(tmpfile)
      rm(tmpfile)
      unlink(file)
      rm(file)
      rm(exif_tib)
      rm(error_message)
      rm(date_string)
      rm(camera_folder_id)
      rm(date_folder_id)

      return(paste0("ERROR! IMAGE NOT WRITTEN TO DRIVE!"))
    }





  }

}

#------------------------- Get info of latest picture for a site -----------------------
#* @get /get_latest_picture_info
#* @param key API key
#* Get latest picture info
function(key, camera_ID) {

  if (key %in% api_keys & camera_ID %in% camera_id_list) {

      return(con %>%
               tbl("photo_info") %>%
               filter(camera_ID == camera_ID) %>%
               filter(DateTimeOriginalUTC == max(DateTimeOriginalUTC, na.rm=T)) %>%
               collect())
    }
}

