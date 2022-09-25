
library(shiny)

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
    tabPanel(title = "Home"        , value = "home"    , icon = icon("campground"), mod_home_UI("tab_home")    ),
    tabPanel(title = "CV model"    , value = "cv_model", icon = icon("map")       , mod_CV_UI("tab_cv")        ),
    tabPanel(title = "Unit times"  , value = "time"    , icon = icon("table")     , mod_time_UI("tab_time")    )
  ) ## END navbarPage
) ## END fluidPage


## Server #################################################################
server <- function(input, output, session) {

  ## + Initiate reactive values list to be passed between modules =========
  ## See https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
  rv <- reactiveValues(
    cv_model = reactiveValues(),
    time     = reactiveValues(),
    params   = reactiveValues(),
    results  = reactiveValues()
  )



  ## + Module server functions ============================================
  mod_home_server("tab_home", rv = rv)

  mod_CV_server("tab_cv", rv = rv)

  mod_time_server("tab_time", rv = rv)


  ## + Trans modules events ===============================================
  observeEvent(rv$to_cv, {
    updateTabsetPanel(session, "navbar", "cv_model")
  })

  observeEvent(rv$to_time, {
    updateTabsetPanel(session, "navbar", "time")
  })

}

## App call ###############################################################
shinyApp(ui, server)


