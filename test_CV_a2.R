
library(shiny)

options(shiny.maxRequestSize = 10*1024^2)

## UI #####################################################################
ui <- fluidPage(

  shinyjs::useShinyjs(),  # Include shinyjs

  shiny::withMathJax(),

  tags$div(HTML("<script type='text/x-mathjax-config'>
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$']]}
            });
            </script>
            ")),

  submod_CV_a2_UI("test")

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

  rv_cv <- reactiveValues()

  ## + Module server functions ============================================
  submod_CV_a2_server("test", rv = rv)

}

## App call ###############################################################
shinyApp(ui, server)

