
load_crop_avitabile <- function(path_data, sf_aoi){

  rs <- terra::rast(
    x = file.path(path_data, "Avitabile_AGB_Map/Avitabile_AGB_Map.tif")
  )

  names(rs) <- "agb_avitabile"

  sf_aoi2 <- sf_aoi %>% st_buffer(dist = 1)

  rs_out <- terra::crop(rs, terra::vect(sf_aoi2))

}
