
## Globals #################################################################

library(shiny)

cv_approach <- "a2"

cv_params <- tibble(
    beta0 = 1.02,
    beta1 = -0.15,
    beta2 = 0.016,
    beta3 = -0.12,
    beta4 = 0
  )


nested_plot <- tibble(
  nested_level = c("lvl1", "lvl2", "lvl3"),
  dbh_min      = c(30, 10, 2),
  tree_density = c(300, 1000, 1500),
  time_measure = c(3, 2, 0.5)
)

unit_times <- tibble(
  drive_time   = 0.5,
  walk_time    = 1,
  march_speed  = 2,
  auth_time    = 2,
  working_hour = 9,
  working_day  = 21
)

list_params <- list(
  subplot_count       = c(1, 3, 5),
  distance_multiplier = 2:4,
  nest1_radius        = 15:25,
  nest2_radius        = 8:12,
  plot_shape          = "L",
  allowable_error     = "10"
)

prepa_combi <- expand.grid(list_params) %>%
  dplyr::as_tibble()

n_combi = nrow(prepa_combi)

combi <- prepa_combi %>%
  dplyr::mutate(dplyr::across(where(is.double), as.integer)) %>%
  dplyr::mutate(dplyr::across(where(is.factor), as.character)) %>%
  dplyr::mutate(
    id               = 1:n_combi,
    subplot_area     = round(pi * .data$nest1_radius^2 / 100^2, 3),
    subplot_area2    = round(pi * .data$nest2_radius^2 / 100^2, 3),
    subplot_distance = .data$distance_multiplier * .data$nest1_radius
  ) %>%
  mutate(
    subplot_avg_distance = dplyr::case_when(
      .data$subplot_count == 1 ~ as.integer(.data$subplot_distance),
      .data$plot_shape == "L"  ~ as.integer(.data$subplot_distance * (.data$subplot_count - 1) * 2 / .data$subplot_count),
      TRUE ~ NA_integer_
    )
  ) %>%
  dplyr::select(.data$id, dplyr::everything())



## UI #####################################################################

ui <- fluidPage(

  shinyjs::useShinyjs(),  # Include shinyjs

  shiny::withMathJax(),

  ## section below allows in-line LaTeX via $ in mathjax.
  ## See https://stackoverflow.com/questions/54876731/inline-latex-equations-in-shiny-app-with-mathjax
  ## and https://stackoverflow.com/questions/60639117/r-shiny-with-mathjax-how-to-avoid-parentheses-being-automatically-placed-in-mat
  tags$div(HTML("<script type='text/x-mathjax-config'>
          MathJax.Hub.Config({
          tex2jax: {inlineMath: [['$','$']]}
          });
          </script>
          ")),

  submod_opti_calc_UI("test")

) ## END fluidPage


## Server #################################################################

server <- function(input, output, session) {

  ## + Initiate reactive values list to be passed between modules =========
  ## See https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
  rv <- reactiveValues(
    cv_model = reactiveValues(
      cv_approach = cv_approach,
      cv_params = cv_params
    ),
    time = reactiveValues(
      nested_plot = nested_plot,
      unit_times = unit_times
      ),
    opti = reactiveValues(
      prepa_combi = prepa_combi,
      n_combi = n_combi,
      combi = combi
    ),
    results  = reactiveValues()
  )

  submod_opti_calc_server("test", rv = rv)

}

## App call ###############################################################
shinyApp(ui, server)


