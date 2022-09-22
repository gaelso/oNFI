#' Calculate CV based on plot design and initial CV for approach 1
#'
#' @description Calculate a Coefficient of Variation based on an initial CV value,
#'              the unit area used for data collection of the initial and final CVs.
#'
#' @param cv_init An initial coefficient of variation in %.
#' @param area_init The unit area size in ha of the sampling unit used to calculate the initial CV.
#' @param area_opti The unit area size in ha of the sampling unit used for the CV to calculate.
#'
#' @return Numeric value
#'
#' @examples
#' calc_CV_a1(cv_init = 94, area_init = 0.9, area_opti = 0.1)
#'
#' @export
calc_CV_a1 <- function(cv_init, area_init, area_opti) {

  ## If initial unit area > 1 ha, consider it 1 ha as
  ## CV may not increase once plots are large enough
  area_init_cor <- dplyr::if_else(area_init > 1, 1, area_init)

  ## Lynch 2017 formula
  ## https://academic.oup.com/forestry/article/90/2/211/2605853
  round(sqrt(cv_init^2 * (area_init_cor / area_opti)^0.5))

}
