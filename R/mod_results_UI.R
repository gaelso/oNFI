
#' Optimization results module server function
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_results_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h3("Optimization results", align = "center"),

    br(),



    # ##
    # ## Check messages #######################################################
    # ##
    #
    # hr(),
    #
    # textOutput(outputId = ns("test_approach")),
    #
    # tableOutput(outputId = ns("test_cv")),
    #
    # br(),
    #
    # textOutput(outputId = ns("test_nested")),
    #
    # textOutput(outputId = ns("test_time")),
    #
    # br(),
    #
    # textOutput(outputId = ns("test_combi")),
    #
    # hr(),



    ##
    ## Results graphs #######################################################
    ##

    h4("Select final plot design parameters"),

    #br(),

    ## + User chosen parameter for filtering ================================

    wellPanel(

      fluidRow(
        column(6, sliderInput(
          inputId = ns("subplot_count"),
          label = "Number of subplots",
          value = 3,
          min = 1,
          max = 12,
          step = 1
        )),
        column(6, sliderInput(
          inputId = ns("distance_multiplier"),
          label = "Distance between subplot centers (x subplot radius)",
          value = 3,
          min = 2,
          max = 8,
          step = 1
        ))
      ),

      fluidRow(
        column(6, sliderInput(
          inputId = ns("nest1_radius"),
          label = "Radius of the large subplot for big trees (m)",
          value = 18,
          min = 10,
          max = 30,
          step = 1
        )),
        column(6, sliderInput(
          inputId = ns("nest2_radius"),
          label = "Radius of the medium subplot for small trees (m)",
          value = 10,
          min = 0,
          max = 20,
          step = 1
        ))
      )
    ), ## End wellPanel

    br(),

    ## + graphs ============================================================

    h4("CV and inventory time for each parameter"),

    p("In each figure, one parameter variation is shown and all the parameters are
      fixed to the selected value."),

    p("Warning: two y-axis graphs are misleading in nature as the 'relation' between
    the y-axis is not proved.", style = "color: #ffc107; font-style: italic;"),

    fluidRow(
      column(6, plotOutput(outputId = ns("gr_subplot_count"))),
      column(6, plotOutput(outputId = ns("gr_nest1_radius")))
    ),

    fluidRow(
      column(6, plotOutput(outputId = ns("gr_subplot_distance"))),
      column(6, plotOutput(outputId = ns("gr_nest2_radius")))
    ),

    br(),


    ##
    ## Measurement time of the selected design ###############################
    ##

    h4("Detailed inventory time for the selected design"),

    tableOutput(outputId = ns("table_time")),

    br(),



    ##
    ## Show table of lowest CV, time and compromise #########################
    ##

    h4("10 selected plot designs options for minimal variance or minimal time"),

    tableOutput(outputId = ns("table_opti"))







  ) ## END tagList

} ## END function params_UI()
