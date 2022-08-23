


shiny_optimize_NFI <- function(...) {

  ## GLOBAL #################################################################

  ## + Libraries
  library(shiny)
  library(sf)
  library(terra)
  library(extrafont)
  library(tidyverse)
  library(stringr)

  ## UI #####################################################################
  ui <- fluidPage(
    titlePanel(
      title = div(img(src="assets/banner_en.png", width = '100%')),
      windowTitle = "Optimize NFI"
      ),
    navbarPage(
      id = "navbar", title = NULL, selected = "home",
      tabPanel(title = "Home", value = "home", icon = icon("campground"), home_UI("tab_home"))
    ) ## END navbarPage
  ) ## END fluidPage


  ## Server #################################################################
  server <- function(input, output, session) {

  }
  shinyApp(ui, server, ...)
}
