


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
    tabPanel(
      title = div(img(src="banner_en.png", width = '100%')),
      windowTitle = "PRAP Monitoring"
      ),
    navbarPage(
      id = "navbar", title = NULL, selected = "home",
      tabPanel(
        title = i18n$t("Home"),
        value = "home",
        icon = icon("campground"),
        home_UI("tab_home")
        )
    ) ## END navbarPage
  ) ## END fluidPage


  ## Server #################################################################
  server <- function(input, output, session) {
    monthFeedbackServer("tab1", reactive(input$month))
    birthstoneServer("tab2", reactive(input$month))
  }
  shinyApp(ui, server, ...)
}
