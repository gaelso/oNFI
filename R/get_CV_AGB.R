

#' Coefficient of variation and additional info from raster file
#'
#' @param df a raster data in data frame format (using for ex. terra::as.data.frame())
#' @param agb_min a minimum value for the data to be included in the calculations
#'
#' @importFrom dplyr pull if_else summarise
#' @importFrom rlang .data
#' @importFrom stats sd
#'
#' @noRd
get_CV_AGB <- function(df, agb_min = 20){

  ## Checks
  if(!is.numeric(pull(df[3]))) stop("'df' doesn't contain AGB as numerical values in column 3")

  if(names(df)[1] != "x" | !is.numeric(pull(df[1]))) stop("'df' doesn't contain x coordinates in column 1, wrong column name or wrong data format")

  if(names(df)[2] != "y" | !is.numeric(pull(df[2]))) stop("'df' doesn't contain y coordinates in column 2, wrong column name or wrong data format")

  ## Calc CV
  names(df)[3] <- "agb"

  df %>%
    mutate(agb = if_else(is.na(.data$agb), 0, .data$agb)) %>%
    filter(.data$agb >= agb_min) %>%
    summarise(
      n_pix = dplyr::n(),
      agb_mean = round(mean(.data$agb), 2),
      agb_sd = round(stats::sd(.data$agb), 2),
    ) %>%
    mutate(
      cv_init = round(.data$agb_sd / .data$agb_mean * 100, 1)
    )

  }
