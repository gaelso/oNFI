

#' Coefficient of variation and additional info from raster file
#'
#' @param rs a SpatRaster object
#' @param agb_min a minimum value for the data to be included in the calculations
#'
#' @importFrom terra ncell, clamp, global, ext, crop, project, res
#' @importFrom dplyr tibble
#'
#' @noRd
get_CV_AGB <- function(rs, agb_min = 0){

  ## Change name
  #names(rs) <- "agb"

  ## stats
  pix_count <- terra::ncell(rs)

  if (agb_min > 0) {

    rs_clamp <- terra::clamp(rs, lower = agb_min, value=FALSE)

    agb_mean <- terra::global(rs_clamp, fun = "mean", na.rm = TRUE)
    agb_sd   <- terra::global(rs_clamp, fun = "sd"  , na.rm = TRUE)

  } else {

    agb_mean <- terra::global(rs, fun = "mean", na.rm = TRUE)
    agb_sd   <- terra::global(rs, fun = "sd"  , na.rm = TRUE)

  }

  # rs_proj  <- terra::project(rs, "ESRI:54017", method = "near")
  # pix_area <- terra::res(rs_proj)[1]^2 / 100^2

  ## Get pixel area from a sample of the data in the center region of the raster +/- 1 deg
  rs_ext <- terra::ext(rs)
  rs_ext2 <- c(
    (rs_ext[1] + rs_ext[2]) / 2 - 1,
    (rs_ext[1] + rs_ext[2]) / 2 + 1,
    (rs_ext[3] + rs_ext[4]) / 2 - 1,
    (rs_ext[3] + rs_ext[4]) / 2 + 1
    )

  test <- terra::crop(rs, rs_ext2)

  rs_proj  <- terra::project(test, "ESRI:54017", method = "near")
  area_init <- terra::res(rs_proj)[1]^2 / 100^2

  dplyr::tibble(
      pix_count = pix_count,
      area_init = area_init,
      agb_mean  = as.numeric(agb_mean),
      agb_sd    = as.numeric(agb_sd),
      cv_init   = round(agb_sd / agb_mean * 100, 1)
      )

  }
