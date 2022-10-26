
##
## Main function to get tiles, download if necessary and crop to AOI ########
##

#' Download Santoro 2018 tiles based on users' area of interest
#'
#' @description This function makes the biomass map from Santoro et al. 2018
#'              (GlobBiomass global above-ground biomass and growing stock volume datasets)
#'              available to use in R. It requires a directory path, checks
#'              if the data is already there, if not download the data, and load in
#'              the R environment as a terra::rast object. As the data is split into tiles,
#'              a spatial area boundaries object (as simple feature object, see sf) is required
#'              to determine which tiles to download. The raster biomass data is
#'              clipped to the sf object extent. Alternatively users can specify a tile name.
#'
#' @param path_data a path to download or load the biomass map.
#' @param progress_id,session in a Shiny context, links to a shinyWidgets::progressBar() object
#' @param sf_aoi a simple feature object with the boundaries of the area of interest. if NULL, a
#'               tile name is expected.
#' @param tile_name In the absence of AOI spatial data (sf_aoi = NULL), a tile name should be specified. See
#'                  https://globbiomass.org/wp-content/uploads/GB_Maps/Globbiomass_global_dataset.html.
#'                  The tile_name is with the form "NxxExx_agb.zip" with xx the longitude and latitude
#'                  tile breaks
#' @param url URL for downloading the map. Defaults to: globbiomass.org/wp-content/uploads/GB_Maps/.
#'            The url is missing the tile_name that comes from either the AOI data or the tile_name.
#'
#' @return a raster object of type terra::rast()
#'
#' @importFrom purrr walk map
#'
#' @examples
#' \dontrun{
#' path_data <- tempdir(dir.create("data", showWarnings = F))
#'
#' rs_santoro <- get_santoro(path_data = path_data, tile_name = "N00E100_agb")
#' plot(rs_santoro)
#'
#' unlink(path_data)
#' }
#'
#' @export
get_santoro <- function(path_data, progress_id = NULL, session = NULL, sf_aoi = NULL,
                        tile_name = "N00E140_agb", url = "globbiomass.org/wp-content/uploads/GB_Maps/"){

  ## + Checks ----
  ## Check inputs
  if (is.null(sf_aoi) & is.null(tile_name)) stop("Need either an AOI boundary simple feature or a tile name")

  ## + Get tiles ----
  ## Transform CRS to get tiles
  if(!is.null(sf_aoi)) sf_aoi_wgs84 <- sf::st_transform(sf_aoi, crs = 4326)

  ## Get tile names
  santoro_tiles <- ifelse(is.null(sf_aoi), tile_name,  get_santoro_tiles(sf_aoi = sf_aoi_wgs84))

  ## Update Progress
  if (!is.null(progress_id)) shinyWidgets::updateProgressBar(session = session, id = progress_id, value = 20)

  ## + Download tiles if necessary ----
  purrr::walk(santoro_tiles, function(x){
    download_santoro(path_data = path_data, url = paste0(url, x, ".zip"))
  })

  ## Update Progress
  if (!is.null(progress_id)) shinyWidgets::updateProgressBar(session = session, id = progress_id, value = 50)

  ## + Read data ----
  ## List tiles
  santoro_filelist <- list.files(file.path(path_data, "Santoro_agb"), pattern = "_agb.tif")
  santoro_files    <- santoro_filelist[match(paste0(santoro_tiles, ".tif"), santoro_filelist)]

  ## Load files
  rs_list <- purrr::map(santoro_files, function(x){

    rs <- terra::rast(file.path(paste0(path_data, "/Santoro_agb"), x))

    if (!is.null(sf_aoi)) {
      check <- terra::intersect(terra::ext(rs), terra::vect(sf_aoi_wgs84))
      if (!is.null(check))  rs_out <- terra::crop(rs, terra::vect(sf_aoi_wgs84)) else rs_out <- rs
    } else {
      rs_out <- rs
    }

  })

  rs_list <- rs_list[!sapply(rs_list, is.null)]

  ## Update Progress
  if (!is.null(progress_id)) shinyWidgets::updateProgressBar(session = session, id = progress_id, value = 80)

  ## + Prepare final raster object ----
  ## Merging elements
  if (length(rs_list) == 1) {
    rs_out <- rs_list[[1]]
  } else if (length(rs_list) == 2) {
    rs_out <- terra::merge(rs_list[[1]], rs_list[[2]])
  } else if (length(rs_list) == 3) {
    rs_tmp <- terra::merge(rs_list[[1]], rs_list[[2]])
    rs_out <- terra::merge(rs_tmp, rs_list[[3]])
    rs_tmp <- NULL
  } else if (length(rs_list) == 4) {
    rs_tmp1 <- terra::merge(rs_list[[1]], rs_list[[2]])
    rs_tmp2 <- terra::merge(rs_list[[3]], rs_list[[4]])
    rs_out  <- terra::merge(rs_tmp1, rs_tmp2)
    rs_tmp1 <- NULL
    rs_tmp2 <- NULL
  }

  names(rs_out) <- "agb_santoro"

  if (!is.null(sf_aoi)) {

    rs_out_proj <- terra::project(rs_out, "ESRI:54017", method = "near")

  } else {

    rs_out_proj <- rs_out

  }

  rs_out_proj

} ## END function get_santoro()



##
## Helper functions ############################################################
##

#' Get tile names for downloading Santoro 2018 biomass raster data
#'
#' @param sf_aoi a spatial simple feature object with boundaries of an area of interest.
#'
#' @noRd
get_santoro_tiles <- function(sf_aoi){

  ## Check input
  if(sf::st_crs(sf_aoi)$input != "EPSG:4326") stop("AOI CRS should be 4326 to get tiles")

  url1 <- NULL
  url2 <- NULL
  url3 <- NULL
  url4 <- NULL

  aoi_bbox <- sf_aoi %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_bbox()

  x1 <- floor(aoi_bbox$xmin / 40) * 40 - 20
  y1 <- ceiling(aoi_bbox$ymax / 40) * 40
  x_chr <- if_else(x1 == 0, "00", as.character(abs(x1)))
  y_chr <- if_else(y1 == 0, "00", as.character(abs(y1)))
  x_dir <- if_else(x1 < 0, "W", "E")
  y_dir <- if_else(y1 < 0, "S", "N")

  url1 <- paste0(y_dir, y_chr, x_dir, x_chr, "_agb")

  x2 <- floor(aoi_bbox$xmax / 40) * 40 - 20
  y2 <- ceiling(aoi_bbox$ymin / 40) * 40
  x_chr <- if_else(x2 == 0, "00", as.character(abs(x2)))
  y_chr <- if_else(y2 == 0, "00", as.character(abs(y2)))
  x_dir <- if_else(x2 < 0, "W", "E")
  y_dir <- if_else(y2 < 0, "S", "N")

  url2 <- paste0(y_dir, y_chr, x_dir, x_chr, "_agb")

  if (url1 == url2) url2 <- NULL

  if (x1 != x2 & y1 != y2) {

    x_chr <- if_else(x1 == 0, "00", as.character(abs(x1)))
    y_chr <- if_else(y2 == 0, "00", as.character(abs(y2)))
    x_dir <- if_else(x1 < 0, "W", "E")
    y_dir <- if_else(y2 < 0, "S", "N")

    url3 <- paste0(y_dir, y_chr, x_dir, x_chr, "_agb")

    x_chr <- if_else(x2 == 0, "00", as.character(abs(x2)))
    y_chr <- if_else(y1 == 0, "00", as.character(abs(y1)))
    x_dir <- if_else(x2 < 0, "W", "E")
    y_dir <- if_else(y1 < 0, "S", "N")

    url4 <- paste0(y_dir, y_chr, x_dir, x_chr, "_agb")

  }

  ## Output
  c(url1, url2, url3, url4)

} ## END function get_santoro_tiles()


#' Download Santoro 2018 tiles
#'
#' @param path_data a path to download the data
#' @param url URL to download santoro data
#'
#' @noRd
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
