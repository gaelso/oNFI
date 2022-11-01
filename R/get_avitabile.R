


#' Download Avitabile raster biomass data
#'
#' @description This function makes the biomass map from Avitabile et al. 2016
#'              (An integrated pan-tropical biomass map using multiple reference
#'              datasets) available to use in R. It requires a directory path, checks
#'              if the data is already there, if not download the data, and load in
#'              the R environment as a terra::rast object. Additionally if the user
#'              provide a spatial area boundaries (as simple feature object, see sf)
#'              the raster biomass data is clipped to the sf object extent.
#'
#' @param path_data a path to download or load the biomass map.
#' @param progress_id,session in a Shiny context, links to a shinyWidgets::progressBar() object
#' @param sf_aoi a simple feature object to clip the raster data
#' @param url URL for downloading the map. Defaults to:
#'            http://lucid.wur.nl/storage/downloads/high-carbon-ecosystems/Avitabile_AGB_Map.zip

#'
#' @return a raster object of type terra::rast()
#'
#' @importFrom shinyWidgets updateProgressBar
#'
#' @examples
#' \dontrun{
#' path_data <- tempdir(dir.create("data", showWarnings = F))
#'
#' rs_avitabile <- get_avitabile(path_data = path_data)
#' plot(rs_avitabile)
#'
#' unlink(path_data)
#'}
#'
#' @export
get_avitabile <- function(path_data, progress_id = NULL, session = NULL, sf_aoi = NULL,
                          url = "http://lucid.wur.nl/storage/downloads/high-carbon-ecosystems/Avitabile_AGB_Map.zip"){


  ## + Download if necessary ----
  ## Get file name from URL
  server_filename <- stringr::str_remove(url, ".*/")
  file_ext        <- stringr::str_sub(server_filename, start = -4, end = -1)
  sans_ext        <- stringr::str_remove(server_filename, file_ext)

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

  ## Update progress
  if (!is.null(progress_id)) shinyWidgets::updateProgressBar(session = session, id = progress_id, value = 50)



  ## + Load raster ----
  rs <- terra::rast(
    x = file.path(path_data, "Avitabile_AGB_Map/Avitabile_AGB_Map.tif")
  )

  names(rs) <- "agb_avitabile"

  ## + Crop to AOI
  if (!is.null(sf_aoi)){

    sf_aoi_wgs84 <- sf_aoi %>% sf::st_transform(crs = 4326)

    #sf_aoi2 <- sf_aoi %>% st_buffer(dist = 1)

    rs_out <- terra::crop(rs, terra::vect(sf_aoi_wgs84))

    #rs_out_proj <- terra::project(rs_out, "ESRI:54017", method = "near")

  } else {

    rs_out <- rs

  }

  rs_out

}
