update_rds_df <- function(filename, df_rds) {

  df_name <- fs::path_ext_remove(filename)

  download_new_version <- TRUE

  is_in_data <- fs::file_exists(here::here("data", filename))

  date_modif_s3 <- df_rds %>%
    dplyr::filter(Key == filename) %>%
    dplyr::pull(LastModified) %>%
    lubridate::as_datetime()

  if (is_in_data) {
    # Compare modification times
    date_modif_local <-
      readr::read_rds(here::here("data", paste0(df_name, "_modiftime.rds")))

    is_local_more_recent <- date_modif_s3 <= date_modif_local

    if (is_local_more_recent) download_new_version <- FALSE

  }

  if (download_new_version) {
    # Download the dataset itself
    aws.s3::save_object(filename,
                bucket = "fyac-final-data-twitter-constituyentes",
                here::here("data", filename))

    # Update the modified date
    readr::write_rds(date_modif_s3, here::here("data", paste0(df_name, "_modiftime.rds")))
  }

}
