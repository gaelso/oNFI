
#' Title
#'
#' @param aoi a simple feature (see the package 'sf')
#'
#' @return
#' @export
#'
#' @examples
get_santoro_tile <- function(aoi){

  url1 <- NULL
  url2 <- NULL
  url3 <- NULL
  url4 <- NULL

  aoi_bbox <- aoi %>%
    st_transform(crs = 4326) %>%
    st_bbox()

  x1 <- floor(aoi_bbox$xmin / 40) * 40 - 20
  y1 <- ceiling(aoi_bbox$ymax / 40) * 40
  x_chr <- if_else(x == 0, "00", as.character(abs(x1)))
  y_chr <- if_else(y == 0, "00", as.character(abs(y1)))
  x_dir <- if_else(x < 0, "W", "E")
  y_dir <- if_else(y < 0, "S", "N")

  url1 <- paste0(y_dir, y_chr, x_dir, x_chr, "_agb.zip")

  x2 <- floor(aoi_bbox$xmax / 40) * 40 - 20
  y2 <- ceiling(aoi_bbox$ymin / 40) * 40
  x_chr <- if_else(x == 0, "00", as.character(abs(x2)))
  y_chr <- if_else(y == 0, "00", as.character(abs(y2)))
  x_dir <- if_else(x < 0, "W", "E")
  y_dir <- if_else(y < 0, "S", "N")

  url2 <- paste0(y_dir, y_chr, x_dir, x_chr, "_agb.zip")

  if (url1 == url2) url2 <- NULL

  if (x1 != x2 & y1 != y2) {

    x_chr <- if_else(x == 0, "00", as.character(abs(x1)))
    y_chr <- if_else(y == 0, "00", as.character(abs(y2)))
    x_dir <- if_else(x < 0, "W", "E")
    y_dir <- if_else(y < 0, "S", "N")

    url3 <- paste0(y_dir, y_chr, x_dir, x_chr, "_agb.zip")

    x_chr <- if_else(x == 0, "00", as.character(abs(x2)))
    y_chr <- if_else(y == 0, "00", as.character(abs(y1)))
    x_dir <- if_else(x < 0, "W", "E")
    y_dir <- if_else(y < 0, "S", "N")

    url4 <- paste0(y_dir, y_chr, x_dir, x_chr, "_agb.zip")


  }

  ## Output
  c(url1, url2, url3, url4)

}
