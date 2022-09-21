
#' Calculate CV based on CV model (approach 2)
#'
#' @param n
#' @param d
#' @param a1
#' @param a2
#' @param cv_params
#'
#' @return
#' @export
#'
#' @examples

calc_CV_a2 <- function(n, d, a1, a2, cv_params){

  round(cv_params$beta0 * n^cv_params$beta1 * d^cv_params$beta2 * a1^cv_params$beta3 * a2^cv_params$beta4 * 100)

}
