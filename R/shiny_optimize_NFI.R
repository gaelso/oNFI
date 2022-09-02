


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

  add_font(path_data = tempdir())

  sysfonts::font_add("LoraIt", file.path(tempdir(), "fonts/Lora/static/Lora-Italic.ttf"))
  showtext::showtext_auto()

  options(shiny.maxRequestSize = 10*1024^2)

  ## UI #####################################################################
  ui <- fluidPage(

    useShinyjs(),  # Include shinyjs

    titlePanel(
      title = div(img(src="assets/banner_en.png", width = '100%')),
      windowTitle = "Optimize NFI"
      ),

    navbarPage(
      id = "navbar", title = NULL, selected = "home",
      tabPanel(title = "Home"   , value = "home"   , icon = icon("campground"), home_UI("tab_home")      ),
      tabPanel(title = "CV model", value = "CV_model", icon = icon("map")     , CV_model_UI("tab_CV_model"))
    ) ## END navbarPage
  ) ## END fluidPage


  ## Server #################################################################
  server <- function(input, output, session) {

    ## Initiate reactive values list to be passed between modules
    rv <- reactiveValues(
      ## CV model related values
      CV_model = reactiveValues(
        file_path = NULL,
        sf_aoi = NULL,
        rs_avitabile = NULL,
        df_avitabile = NULL,
        cv_avitabile = NULL,
        rs_santoro = NULL,
        df_santoro = NULL,
        cv_santoro = NULL,
        cv_params = NULL
      ))

    ## Module server functions
    home_server("tab_home", rv = rv)

    CV_model_server("tab_CV_model", rv = rv)

    ## Trans modules events
    observeEvent(rv$to_CV_model, {
      updateTabsetPanel(session, "navbar", "CV_model")
    })

  }

  ## App call ###############################################################
  shinyApp(ui, server, ...)

} ## END function shiny_optimize_NFI()
