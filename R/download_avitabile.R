#' Download Avitabile et al. 2016 biomass map to target directory
#'
#' Download and extract the biomass map developed by Avitabile et al., presented in the 2016 article:
#' An integrated pan-tropical biomass map using multiple reference datasets.
#' https://doi.org/10.1111/gcb.13139
#'
#' @param path_data path to the destination folder for the data
#' @param url URL to download the data
#'
#' @examples
#'
#' # Create a data directory
#' getwd()
#' dir.create("data", showWarnings = FALSE)
#'
#' download_data(path_data = "data")
#'
#' @export
download_avitabile <- function(path_data, url = "http://lucid.wur.nl/storage/downloads/high-carbon-ecosystems/Avitabile_AGB_Map.zip"){

  ## Get file name from URL
  server_filename <- str_remove(url, ".*/")
  file_ext        <- str_sub(server_filename, start = -4, end = -1)
  sans_ext        <- str_remove(server_filename, file_ext)

  ## Check if the data is already downloaded
  check1 <- sans_ext %in% list.files(path_data)
  check2 <- paste0(sans_ext, ".tif") %in% list.files(file.path(path_data, sans_ext))

  ## If not, downloading
  if (!(check1 & check2)) {

    message(paste0("Downloading and extracting Avitabile et al. 2016 ",  server_filename, "..."))

    time1 <- Sys.time()

    utils::download.file(
      url      = url,
      destfile = file.path(path_data, server_filename)
    )

    ## If zipfile, unzip and remove zipfile
    if (file_ext == ".zip") {
      utils::unzip(
        zipfile = file.path(path_data, server_filename),
        exdir   = path_data
      )
      unlink(file.path(path_data, server_filename))
    }

    time2 <- Sys.time()
    dt    <- round(as.numeric(time2-time1, units = "secs"))
    message(paste0("...Done", " - ", dt, " sec."))

  }

} ## END function download_avitabile()


