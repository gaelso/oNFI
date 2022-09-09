


shiny_optimize_NFI <- function(...) {

  ## GLOBAL #################################################################

  ## + Libraries
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(shinyFiles)
  library(sf)
  library(terra)
  library(extrafont)
  library(tidyverse)
  library(stringr)
  library(ggspatial)
  library(ggpubr)
  library(showtext)

  if ("fonts" %in% list.files()) {

    font_files <- list.files("fonts", recursive = TRUE) %>% str_remove(".*/")

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

  ## UI #####################################################################
  ui <- fluidPage(

    shinyjs::useShinyjs(),  # Include shinyjs

    withMathJax(),

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
                                  max-width: 1200px !important;
                                  margin: 0 auto;
                               }

                               "))),

    titlePanel(
      title = div(img(src="assets/banner_en.png", width = '100%')),
      windowTitle = "Optimize NFI"
      ),

    navbarPage(
      id = "navbar", title = NULL, selected = "home",
      tabPanel(title = "Home"    , value = "home"    , icon = icon("campground"), mod_home_UI("tab_home")        ),
      tabPanel(title = "CV model", value = "CV_model", icon = icon("map")       , mod_CV_model_UI("tab_CV_model")),
      tabPanel(title = "Parameters", value = "params", icon = icon("table")     , params_UI("tab_params"))
    ) ## END navbarPage
  ) ## END fluidPage


  ## Server #################################################################
  server <- function(input, output, session) {

    ## + Initiate reactive values list to be passed between modules =========
    rv <- reactiveValues(
      CV_model = reactiveValues(
        cv_approach  = NULL,
        file_path    = NULL,
        sf_aoi       = NULL,
        rs_avitabile = NULL,
        df_avitabile = NULL,
        cv_avitabile = NULL,
        rs_santoro   = NULL,
        df_santoro   = NULL,
        cv_santoro   = NULL,
        cv_mixed     = NULL
        ),
      params = reactiveValues(
        cv_params     = NULL,
        nested_params = NULL,
        time_params   = NULL,
        opti_params   = NULL
        )
      )



    ## + Module server functions ============================================
    mod_home_server("tab_home", rv = rv)

    mod_CV_model_server("tab_CV_model", rv = rv)

    params_server("tab_params", rv = rv)



    ## + Trans modules events ===============================================
    observeEvent(rv$to_CV, {
      updateTabsetPanel(session, "navbar", "CV_model")
    })

    observeEvent(rv$to_params, {
      updateTabsetPanel(session, "navbar", "params")
    })

  }

  ## App call ###############################################################
  shinyApp(ui, server, ...)

} ## END function shiny_optimize_NFI()

