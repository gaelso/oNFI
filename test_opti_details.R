
library(shiny)


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

  mod_params_UI("tab_params")

) ## END fluidPage


## Server #################################################################
server <- function(input, output, session) {

  ## + Initiate reactive values list to be passed between modules =========
  ## See https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
  rv <- reactiveValues(
    cv_model = reactiveValues(
      cv_approach = "a2",
      cv_params   = tibble(
        beta0 = 1.02,
        beta1 = -0.15,
        beta2 = 0.016,
        beta3 = -0.12,
        beta4 = 0
      )
    ),
    time = reactiveValues(
      nested_plot = tibble(
        nested_level = c("lvl1", "lvl2", "lvl3"),
        dbh_min      = c(30, 10, 2),
        tree_density = c(300, 1000, 1500),
        time_measure = c(3, 2, 0.5)
      ),
      unit_times = tibble(
        drive_time   = 0.5,
        walk_time    = 1,
        march_speed  = 2,
        auth_time    = 2,
        working_hour = 9,
        working_day  = 21
      )
    ),
    params   = reactiveValues(),
    results  = reactiveValues()
  )

  mod_params_server("tab_params", rv = rv)

}

## App call ###############################################################
shinyApp(ui, server)


