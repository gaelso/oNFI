
## Based on Picard 2007 (https://core.ac.uk/download/pdf/52632663.pdf)
## total_time = n_plot * (unit_time_measure + unit_time_travel)
## time_travel = 1 / march_speed * sqrt(area_forest / n_plot)
## time_measure = time_measure + time_delineate
##              = area_plot * unit_time_measure + plot_perimeter * unit_time_delineate
##              = area_plot * unit_time_measure + 2 * sqrt(pi * area_plot) * unit_time_delineate
## Adapted to NFI, plots become subplots and need to add local authorization and travel to plots:
##  - time_travel_subplots = average_distance * subplot_count / march_speed
##    + For L shaped plot, average_distance = subplot_distance * (subplot_count - 1) * 2 / subplot_count
##  - time_travel_plot = sqrt(area_country / n_plot) / car_speed ## Formula to convert nb of plots to grid spacing.
##    + Can be improved with average transportation from lodging to plot + transportation from office to lodging every week or two.
##  - time_authorization = time to get authorization from local village and recruit workers if necessary

#' Calculate unit plot time based on plot design
#'
#' @description Calculate the time for the different forest operations to measure one forest plot.
#'              Based on Sylla and Picard 2007: Guide methodologique des evaluations rapides de
#'              bois energie and Scott 1993: Optimal design of a plot cluster for monitoring.
#'              Optimized for shiny_NFI_optimize() Shiny application. See the example for the exact
#'              column names in each input.
#'
#' @param unit_times A table with unit times in hours for different forest measurement operations.
#' @param plot_design Plot design variables, i.e. number of subplots, distance between them, subplot radius, etc.
#' @param nest_design Unit time to measure trees in nested subplots for various tree sizes.
#'
#' @return
#'
#' @examples
#' time_input <- data.frame(
#'   drive_time    = 1,  ## h
#'   walk_time     = 1,  ## h
#'   auth_time     = 1,  ## h
#'  march_speed   = 2,  ## km/h
#'  working_hours = 9,  ## h/day
#'   working_days  = 21  ## days/month
#' )
#'
#' params_input <- data.frame(
#'   subplot_count       = 5,
#'   distance_multiplier = 2, ## multiplier of plot radius as input for subplot distance
#'   nest1_radius        = 18,
#'   nest2_radius        = 10,
#'   plot_shape          = "L",
#'   allowable_error     = 10
#' )
#'
#' nest_input <- data.frame(
#'   nested_level = c("lvl1", "lvl2", "lvl3"),
#'   dbh_min      = c(30, 10, 2),
#'   tree_density = c(300, 1000, 1500), ## nb trees / ha
#'   time_measure = c(3, 2, 0.5)  ## in nb min /tree
#' )
#'
#' calc_time(unit_times = time_input, plot_design = params_input, nest_design = nest_input)
#'
#' @export
calc_time <- function(unit_times, plot_design, nest_design) {

  ## Add variables to plot_design
  ## Subplot average distance
  plot_design <- plot_design %>%
    mutate(
      subplot_distance     = distance_multiplier * nest1_radius,
      subplot_avg_distance = dplyr::case_when(
        subplot_count == 1   ~ as.integer(subplot_distance),
        plot_shape    == "L" ~ as.integer(subplot_distance * (subplot_count - 1) * 2 / subplot_count),
        TRUE ~ as.integer(subplot_distance)
      )
    )



  ## Fix lvl3 radius to 2 m
  plot_design$nest3_radius <- 2

  ## Time travel to plot
  time_travel  <- (unit_times$drive_time + unit_times$walk_time) * 2

  ## Time to get authorization
  time_auth <- unit_times$auth_time

  ## Time to measure trees = subplot_count * subplot_area (ha) * tree_density (tree/ha) * time_measure (min/tree)
  lvl1 <- nest_design %>% filter(nested_level == "lvl1")
  lvl2 <- nest_design %>% filter(nested_level == "lvl2")
  lvl3 <- nest_design %>% filter(nested_level == "lvl3")

  time_measure_lvl1 <- pi * (plot_design$nest1_radius / 100)^2 * lvl1$tree_density * lvl1$time_measure / 60
  time_measure_lvl2 <- pi * (plot_design$nest2_radius / 100)^2 * lvl2$tree_density * lvl2$time_measure / 60
  time_measure_lvl3 <- pi * (plot_design$nest3_radius / 100)^2 * lvl3$tree_density * lvl3$time_measure / 60

  time_measure <- (time_measure_lvl1 + time_measure_lvl2 + time_measure_lvl3) * plot_design$subplot_count

  ## Time travel subplots.
  time_walk <- plot_design$subplot_avg_distance * plot_design$subplot_count / (unit_times$march_speed * 1000)


  ## Total plot time
  time_plot <- time_travel + time_auth + time_measure + time_walk

  dplyr::tibble(
    time_plot    = time_plot,
    time_travel  = time_travel,
    time_auth    = time_auth,
    time_measure = time_measure,
    time_walk    = time_walk
  )

} ## End function calc_time()

