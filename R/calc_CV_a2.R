

calc_CV_a2 <- function(n, d, a1, a2, cv_params){

  round(cv_params$beta0 * n^cv_params$beta1 * d^cv_params$beta2 * a1^cv_params$beta3 * a2^cv_params$beta4 * 100)

}
