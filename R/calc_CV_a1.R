
calc_CV_a1 <- function(cv_init, area_init, area_opti) {

  ## If initial unit area > 1 ha, consider it 1 ha as
  ## CV may not increase once plots are large enough
  area_init_cor <- dplyr::if_else(area_init > 1, 1, area_init)

  ## Lynch 2017 formula
  ## https://academic.oup.com/forestry/article/90/2/211/2605853
  sqrt(cv_init^2 * (area_init_corr / area_opti)^0.5)

}
