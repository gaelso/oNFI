

## Download font and make it available for R --------------------------------
#' Download google font
#'
#' @param path_data path to download te font
#' @param font_name Name of the font
#' @param font_url URL to download the font
#'
#' @importFrom utils download.file unzip
#'
#' @noRd
add_font <- function(path_data, font_name = "Lora", font_url = "https://fonts.google.com/download?family=Lora"){

  dir.create(file.path(path_data, "fonts"), showWarnings = F)

  ## Download and extract font
  if (!dir.exists(paste0(path_data, "/fonts/", font_name))) {
    utils::download.file(
      url = font_url,
      destfile = paste0(path_data, "/fonts/", font_name, ".zip"),
      mode = "wb"
    )
    utils::unzip(
      zipfile = paste0(path_data, "/fonts/", font_name, ".zip"),
      exdir   = paste0(path_data, "/fonts/", font_name)
    )
    unlink(paste0(path_data, "/fonts/", font_name, ".zip"))
  } ## End if download font

}



## Add map features to ggplot -----------------------------------------------
#' Add map features to ggplot
#'
#' @param font A font to customize the map and other text elements
#'
#' @noRd
add_ggspatial <- function(font = "LoraIt"){

  list(theme(text = element_text(family = font)),

       ggspatial::annotation_scale(
         location = "tl",
         bar_cols = c("grey60", "white"),
         text_family = font),

       ggspatial::annotation_north_arrow(
         location = "tl",
         which_north = "true",
         pad_x = unit(0.2, "in"),
         pad_y = unit(0.3, "in"),
         style = ggspatial::north_arrow_nautical(
           fill = c("grey40", "white"),
           line_col = "grey20",
           text_family = font))
  )

}

## Create a spatial data frame with around 0.01 deg resolution --------------
#' Create a spatial data frame with around 0.01 deg resolution
#'
#' @param rs a terra SpatRaster object
#'
#' @noRd
make_df <- function(rs){

  rs_fct <- round(0.01 / terra::res(rs)[1], 3)

  if (rs_fct > 1) {

    df <- terra::aggregate(rs, fact =  rs_res) %>%
      terra::as.data.frame(xy = TRUE, na.rm = T) %>%
      dplyr::as_tibble()

  } else {

    df <- rs %>%
      terra::as.data.frame(xy = TRUE, na.rm = T) %>%
      dplyr::as_tibble()

  }

  df

}

