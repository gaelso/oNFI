

calc_CV_a2 <- function(n, d, a, cv_params){

  round(cv_params$beta0 * n^cv_params$beta1 * d^cv_params$beta2 * a^cv_params$beta3 * 100)

}
