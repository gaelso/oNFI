

#url <- "globbiomass.org/wp-content/uploads/GB_Maps/N00E100_agb.zip"

download_santoro <- function(path_data, url){

  ## Get file name from URL
  server_filename <- str_remove(url, ".*/")
  file_ext        <- str_sub(server_filename, start = -4, end = -1)
  sans_ext        <- str_remove(server_filename, file_ext)
  dest_name       <- "Santoro_agb"
  dest_dir        <- file.path(path_data, dest_name)


  ## Check if the data is already downloaded
  check1 <- dest_name %in% list.files(path_data)
  check2 <- paste0(sans_ext, ".tif") %in% list.files(dest_dir)

  if (!check1) dir.create(file.path(path_data, "Santoro_agb"), showWarnings = F)

  ## If not, downloading
  if (!check2) {

    message(paste0("Downloading and extracting Santoro et al. 2018 tile ",  server_filename, "..."))

    time1 <- Sys.time()

    utils::download.file(
      url      = url,
      destfile = file.path(dest_dir, server_filename)
    )

    ## If zipfile, unzip and remove zipfile
    if (file_ext == ".zip") {
      utils::unzip(
        zipfile = file.path(dest_dir, server_filename),
        exdir   = dest_dir
      )
      unlink(file.path(dest_dir, server_filename))
    }

    time2 <- Sys.time()
    dt    <- round(as.numeric(time2-time1, units = "secs"))
    message(paste0("...Done", " - ", dt, " sec."))

  }

} ## END function download_santoro()
