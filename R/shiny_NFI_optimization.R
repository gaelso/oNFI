


shiny_optimize_NFI <- function(...) {

  ## GLOBAL

  ## + Libraries
  library(shiny)
  library(sf)
  library(terra)
  library(extrafont)
  library(tidyverse)
  library(stringr)


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
        home_UI("tab1")),


    tabPanel("Birthstone", birthstoneUI("tab2"))
  )
  server <- function(input, output, session) {
    monthFeedbackServer("tab1", reactive(input$month))
    birthstoneServer("tab2", reactive(input$month))
  }
  shinyApp(ui, server, ...)
}
