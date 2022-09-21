

#' Title
#'
#' @param df
#' @param agb_min
#'
#' @return
#' @export
#'
#' @examples
get_CV_AGB <- function(df, agb_min = 20){

  ## Checks
  if(!is.numeric(pull(df[3]))) stop("'df' doesn't contain AGB as numerical values in column 3")

  if(names(df)[1] != "x" | !is.numeric(pull(df[1]))) stop("'df' doesn't contain x coordinates in column 1, wrong column name or wrong data format")

  if(names(df)[2] != "y" | !is.numeric(pull(df[2]))) stop("'df' doesn't contain y coordinates in column 2, wrong column name or wrong data format")

  ## Calc CV
  names(df)[3] <- "agb"

  df %>%
    mutate(agb = if_else(is.na(agb), 0, agb)) %>%
    filter(agb >= agb_min) %>%
    summarise(
      n_pix = n(),
      agb_mean = round(mean(agb), 2),
      agb_sd = round(sd(agb), 2),
    ) %>%
    mutate(
      cv_init = round(agb_sd / agb_mean * 100, 1)
    )

  }
