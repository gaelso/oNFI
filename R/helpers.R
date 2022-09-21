

## Download font and make it available for R --------------------------------
#' Download google font
#'
#' @param path_data
#' @param font_name
#' @param font_url
#'
#' @return
#' @export
#'
#' @examples
add_font <- function(path_data, font_name = "Lora", font_url = "https://fonts.google.com/download?family=Lora"){

  dir.create(file.path(path_data, "fonts"), showWarnings = F)

  ## Download and extract font
  if (!dir.exists(paste0(path_data, "/fonts/", font_name))) {
    download.file(
      url = font_url,
      destfile = paste0(path_data, "/fonts/", font_name, ".zip"),
      mode = "wb"
    )
    unzip(
      zipfile = paste0(path_data, "/fonts/", font_name, ".zip"),
      exdir   = paste0(path_data, "/fonts/", font_name)
    )
    unlink(paste0(path_data, "/fonts/", font_name, ".zip"))
  } ## End if download font

}



## Add map features to ggplot -----------------------------------------------
#' Add map features to ggplot
#'
#' @param font
#'
#' @return
#' @export
#'
#' @examples
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
