#' Download Aboveground biomass maps to target directory
#'
#' Download_data() download a dataset from a specified URL to a target directory
#' and return a path to
#'
#'
#' @param path_data path to the destination folder for the data
#' @param data_name name of the data file / zipfile
#' @param url URL to download the data
#' @param zipfile TRUE or FALSE. If TRUE, data_name should end with .zip and
#' the file will unzipped in a subdirectory of path_data.
#'
#' @examples
#'
#' # Create a data directory
#' getwd()
#' dir.create("data", showWarnings = FALSE)
#'
#' # Download the Biomass map from Avitabile et al. 2016 article:
#' # An integrated pan-tropical biomass map using multiple reference datasets
#' url       <- "http://lucid.wur.nl/storage/downloads/high-carbon-ecosystems/Avitabile_AGB_Map.zip"
#'
#' download_data(path_data = "data", data_name = "Avitabile_AGB_Map.zip", url = url, zipfile = TRUE)
#'
#' @export
download_agb <- function(path_data, data_name, url, zipfile = FALSE){

  ## Check if the data is already dowloaded
  if (zipfile) {
    check_data <- length(list.files(file.path(path_data, str_remove(data_name, ".zip")))) != 0
  } else {
    check_data <- data_name %in% list.files(path_data)
  }

  ## If not, downloading
  if (!check_data) {

    message(paste0("Downloading ",  data_name, "..."))

    time1 <- Sys.time()

    utils::download.file(
      url      = url,
      destfile = file.path(path_data, data_name)
    )

    ## If zipfile, unzip and remove zipfile
    if (zipfile) {
      utils::unzip(
        zipfile = file.path(path_data, data_name),
        exdir   = path_data
      )
      unlink(file.path(path_data, data_name))
    }

    time2 <- Sys.time()
    dt    <- round(as.numeric(time2-time1, units = "secs"))
    message(paste0("...Done", " - ", dt, " sec."))

  }

} ## END function download_data()

# ## Test
# path_data <- "data"
# data_name <- "Avitabile_AGB_Map.zip"
# url       <- "http://lucid.wur.nl/storage/downloads/high-carbon-ecosystems/Avitabile_AGB_Map.zip"
# zipfile   <- TRUE

