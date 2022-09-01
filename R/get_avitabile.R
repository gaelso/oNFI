


get_avitabile <- function(path_data, sf_aoi = NULL, url = "http://lucid.wur.nl/storage/downloads/high-carbon-ecosystems/Avitabile_AGB_Map.zip"){


  ## Check AOI CRS
  if(!is.null(sf_aoi)){

    epsg_value <- st_crs(sf_aoi)$srid %>% str_remove("EPSG:") %>% as.numeric()
    if (!(epsg_value %in% 32600:32800)) stop("AOI CRS should be in metric unit and WGS 84 UTM zone")

  }


  ## + Download if necessasry ----
  ## Get file name from URL
  server_filename <- str_remove(url, ".*/")
  file_ext        <- str_sub(server_filename, start = -4, end = -1)
  sans_ext         <- str_remove(server_filename, file_ext)

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

  ## + Load raster ----
  rs <- terra::rast(
    x = file.path(path_data, "Avitabile_AGB_Map/Avitabile_AGB_Map.tif")
  )

  names(rs) <- "agb_avitabile"

  ## + Crop to AOI
  if (!is.null(sf_aoi)){

    sf_aoi_wgs84 <- sf_aoi %>% st_transform(crs = 4326)

    #sf_aoi2 <- sf_aoi %>% st_buffer(dist = 1)

    rs_out <- terra::crop(rs, terra::vect(sf_aoi_wgs84))

    rs_out_proj <- terra::project(rs_out, st_crs(sf_aoi)$srid, method = "near")

  } else {

    rs_out_proj <- rs

  }

  rs_out_proj

}
