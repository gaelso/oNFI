
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

    h4("Expected coefficient of variation vs measurement time"),

    br(),

    sidebarLayout(



      ## + User chosen parameter for filtering ================================

      sidebarPanel(

        h5(strong("Select final parameters")),

        sliderInput(
          inputId = ns("subplot_count"),
          label = "Number of subplots",
          value = 3,
          min = 1,
          max = 12,
          step = 1
        ),

        sliderInput(
          inputId = ns("distance_multiplier"),
          label = "Distance between subplot centers (x subplot radius)",
          value = 3,
          min = 2,
          max = 8,
          step = 1
        ),

        sliderInput(
          inputId = ns("nest1_radius"),
          label = "Radius of the large subplot for big trees (m)",
          value = 18,
          min = 10,
          max = 30,
          step = 1
        ),

        sliderInput(
          inputId = ns("nest2_radius"),
          label = "Radius of the medium subplot for small trees (m)",
          value = 10,
          min = 0,
          max = 20,
          step = 1
        ),

        width = 4

      ),



      # + graphs ============================================================

      mainPanel(

        # fluidRow(
        #   column(6, div(
        #     plotOutput(outputId = ns("gr_subplot_count"), height = 300),
        #     style = "padding: 0.375em; border: 1px solid #e3e3e3;
        # border-radius: 4px; width: 450px; margin: 0px auto;"
        #   )),
        #
        #   column(6, div(
        #     plotOutput(outputId = ns("gr_subplot_distance"), height = 300),
        #     style = "padding: 0.375em; border: 1px solid #e3e3e3;
        #     border-radius: 4px; width: 450px; margin: 0px auto;"
        #   ))
        # )
        fluidRow(
          column(6, plotOutput(outputId = ns("gr_subplot_count"))),
          column(6, plotOutput(outputId = ns("gr_subplot_distance")))
        ),
        fluidRow(
          column(6, plotOutput(outputId = ns("gr_nest1_radius"))),
          column(6, plotOutput(outputId = ns("gr_nest2_radius")))
        )
      )
    ),



    ##
    ## Show table of lowest CV, time and compromise #########################
    ##



  ) ## END tagList

} ## END function params_UI()
