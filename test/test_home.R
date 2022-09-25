
## Test home

mod_home <- function(...) {

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
      tabPanel(title = "Home", value = "home", icon = icon("campground"), mod_home_UI("tab_home"))
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

  }

  ## App call ###############################################################
  shinyApp(ui, server, ...)

}

mod_home()


