
mod_results_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h3("Optimization results", align = "center"),

    br(),



    ##
    ## Check messages #######################################################
    ##

    hr(),

    textOutput(outputId = ns("test_approach")),

    tableOutput(outputId = ns("test_cv")),

    br(),

    textOutput(outputId = ns("test_nested")),

    textOutput(outputId = ns("test_time")),

    br(),

    textOutput(outputId = ns("test_combi")),

    hr(),



    ##
    ## Results graphs #######################################################
    ##

    h4("CV vs forest inventory measurement for the tested parameters"),

    br(),

    fluidRow(
      column(6, div(
        plotOutput(outputId = ns("gr_subplot_count"), height = 400),
        style = "padding: 0.375em; border: 1px solid #e3e3e3;
        border-radius: 4px; width: 600px; margin: 0px auto;"
      )),

      # column(6, div(
      #   plotOutput(outputId = ns("gr_subplot_dist"), height = 400),
      #   style = "padding: 0.375em; border: 1px solid #e3e3e3;
      #   border-radius: 4px; width: 600px; margin: 0px auto;"
      # )),

    )





  ) ## END tagList

} ## END function params_UI()
