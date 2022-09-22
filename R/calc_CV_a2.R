#' Calculate CV based on CV model (approach 2)
#'
#' @description Calculate a Coefficient of Variation based on a CV model with plot
#'              design characteristics as input variables.
#'
#' @param n number of subplots.
#' @param d distance between subplot in m.
#' @param a1 subplot outer boundaries size in ha.
#' @param a2 subplot internal nested boundaries for small trees in ha.
#' @param cv_params a table with columns beta0, ..., beta4 and a row with the parameters values.
#'
#' @return Numeric value
#'
#' @examples
#'
#' params <- data.frame(
#'   beta0 = 1.02 ,
#'   beta1 = -0.15,
#'   beta2 = 0.016,
#'   beta3 = -0.12,
#'   beta4 = 0
#' )
#'
#' calc_CV_a2(n = 5, d = 80, a1 = (pi * (18/100)^2), a2 = (pi * (18/100)^2), cv_params = params)
#'
#' @export
calc_CV_a2 <- function(n, d, a1, a2, cv_params){

  round(cv_params$beta0 * n^cv_params$beta1 * d^cv_params$beta2 * a1^cv_params$beta3 * a2^cv_params$beta4 * 100)

}
