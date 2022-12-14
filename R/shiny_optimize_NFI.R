#' Shiny application: Optimization tool for (National) Forest Inventory design
#'
#' @description Starts a Shiny application that guide users through optimizing Forest
#'              inventory design for cost and precision based on different approaches to forest
#'              coefficient of variation for a key variable, user defined unit times for forest
#'              operations and a range of plot design parameters.
#'
#' @param ... arguments to pass to shinyApp
#'
#' @import shiny
#'
#' @importFrom shiny shinyApp
#' @importFrom stringr str_remove
#' @importFrom sysfonts font_add
#' @importFrom showtext showtext_auto
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter slice case_when across as_tibble select left_join bind_rows arrange
#' @importFrom tidyselect everything starts_with
#' @importFrom purrr map_dfr
#' @importFrom readr write_csv
#' @importFrom utils globalVariables
#' @importFrom stats median na.omit
#' @importFrom rlang .data
#'
#' @examples
#' if (interactive()) {
#'
#' shiny_optimize_NFI()
#'
#' }
#'
#' @export
shiny_optimize_NFI <- function(...) {

  ## GLOBAL #################################################################

  ## + Libraries
  # library(shiny)
  # library(shinyjs)
  # library(shinyWidgets)
  # library(shinyFiles)
  # library(sf)
  # library(terra)
  # library(extrafont)
  # library(tidyverse)
  # library(stringr)
  # library(ggspatial)
  # library(ggpubr)
  # library(showtext)

  if ("fonts" %in% list.files()) {

    font_files <- list.files("fonts", recursive = TRUE) %>% stringr::str_remove(".*/")

    if ("Lora-Italic.ttf" %in% font_files) {
      path_lora <- list.files("fonts", recursive = TRUE, full.names = TRUE, pattern = "Lora-Italic.ttf")
    } else {
      add_font(path_data = tempdir())
      path_lora <- list.files(tempdir(), recursive = TRUE, full.names = TRUE, pattern = "Lora-Italic.ttf")
    }

  } else {

    add_font(path_data = tempdir())

    path_lora <- list.files(tempdir(), recursive = TRUE, full.names = TRUE, pattern = "Lora-Italic.ttf")

  }

  sysfonts::font_add("LoraIt", path_lora)

  showtext::showtext_auto()

  options(shiny.maxRequestSize = 10*1024^2)
  options(timeout = max(300, getOption("timeout")))

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


    ## Set max width
    tags$head(tags$style(HTML("
                               body {
                                  max-width: 1600px !important;
                                  margin: 0 auto;
                               }

                               "))),

    titlePanel(
      title = div(img(src="assets/banner_en.png", width = '100%')),
      windowTitle = "Optimize NFI"
      ),

    navbarPage(
      id = "navbar", title = NULL, selected = "home",
      tabPanel(title = "Home"        , value = "home"    , icon = icon("campground"), mod_home_UI("tab_home")  ),
      tabPanel(title = "CV model"    , value = "cv_model", icon = icon("map")       , mod_CV_UI("tab_cv")      ),
      tabPanel(title = "Unit times"  , value = "time"    , icon = icon("table")     , mod_time_UI("tab_time")  ),
      tabPanel(title = "Optimization", value = "opti"    , icon = icon("toggle-on") , mod_opti_UI("tab_opti")  ),
      tabPanel(title = "Results"     , value = "results" , icon = icon("chart-line"), mod_results_UI("tab_res"))
    ) ## END navbarPage
  ) ## END fluidPage


  ## Server #################################################################
  server <- function(input, output, session) {

    ## + Initiate reactive values list to be passed between modules =========
    ## See https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
    rv <- reactiveValues(
      cv_model = reactiveValues(),
      time     = reactiveValues(),
      opti     = reactiveValues(),
      results  = reactiveValues()
      )



    ## + Module server functions ============================================
    mod_home_server("tab_home", rv = rv)

    mod_CV_server("tab_cv", rv = rv)

    mod_time_server("tab_time", rv = rv)

    mod_opti_server("tab_opti", rv = rv)

    mod_results_server("tab_res", rv = rv)



    ## + Trans modules events ===============================================
    observeEvent(rv$to_cv, {
      updateTabsetPanel(session, "navbar", "cv_model")
    })

    observeEvent(rv$to_time, {
      updateTabsetPanel(session, "navbar", "time")
    })

    observeEvent(rv$to_opti, {
      updateTabsetPanel(session, "navbar", "opti")
    })

    observeEvent(rv$to_results, {
      updateTabsetPanel(session, "navbar", "results")
    })

  }

  ## App call ###############################################################
  shinyApp(ui, server, ...)

} ## END function shiny_optimize_NFI()

