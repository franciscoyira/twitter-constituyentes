update_rds_df <- function(filename) {

  df_name <- fs::path_ext_remove(filename)

  download_new_version <- TRUE

  is_in_data <- fs::file_exists(here("data", filename))

  if (is_in_data) {
    # Compare modification times
    date_modif_local <-
      read_rds(here("data", paste0(df_name, "_modiftime.rds")))

    date_modif_s3 <- rds_files_df %>%
      filter(Key == filename) %>%
      pull(LastModified) %>%
      lubridate::as_datetime()

    is_local_more_recent <- date_modif_s3 <= date_modif_local

    if (is_local_more_recent) download_new_version <- FALSE

  }

  if (download_new_version) {
    # Download the dataset itself
    save_object(filename,
                bucket = "fyac-final-data-twitter-constituyentes",
                here("data", filename))

    # Update the modified date
    write_rds(date_modif_s3, here("data", paste0(df_name, "_modiftime.rds")))
  }

}
