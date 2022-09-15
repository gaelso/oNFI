
mod_params_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h3("Forest inventory optimization parameters", align = "center"),

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

    hr(),



    ##
    ## Introduction #########################################################
    ##

    h4("Introduction"),

    ## Toggle on/off the introduction
    checkboxInput(
      inputId = ns("toggle_intro"),
      label = "Show the introduction",
      value = FALSE
    ),

    wellPanel(
      id = ns("params_intro"),

      p(strong("Overview")),

      p("This section is dedicated to optimization parameters. For each parameter,
        several values can be tested. The optimization script will then create
        all the possible combinations of all the parameters, calculate their CV
        and cost and return the optimal designs.")

    ),

    br(),


    ##
    ## Parameters interface #################################################
    ##

    h4("Parameters to be tested"),

    br(),

    ## Message if previous steps not completed
    wellPanel(
      id = ns("check_approach"),

      p("First choose a CV model approach",
        style = "color: #dc3545; font-style: italic; text-align: center;"),  ## Bootstrap danger color

      p("If you selected approach 1, make sure you complete all the steps to
        get an initial CV from the biomass maps.",
        style = "color: #dc3545; font-style: italic; text-align: center;"),  ## Bootstrap danger color

    ), ## End check_approach


    ## if approach chosen, show the interface
    shinyjs::hidden(div(
      id = ns("params_setup"),

      sidebarLayout(



        ## + Parameter inputs ===============================================

        sidebarPanel(

          sliderInput(
            inputId = ns("subplot_count"),
            label = "Number of subplots",
            value = c(1,5),
            min = 1,
            max = 12,
            step = 1
          ),

          sliderInput(
            inputId = ns("distance_multiplier"),
            label = "Distance between subplot centers (x subplot radius)",
            value = c(2,4),
            min = 2,
            max = 8,
            step = 1
          ),

          sliderInput(
            inputId = ns("nest1_radius"),
            label = "Radius of the large subplot for big trees (m)",
            value = c(15,20),
            min = 10,
            max = 30,
            step = 1
          ),

          sliderInput(
            inputId = ns("nest2_radius"),
            label = "Radius of the medium subplot for small trees (m)",
            value = c(8,12),
            min = 0,
            max = 20,
            step = 1
          ),

          radioButtons(
            inputId = ns("plot_shape"),
            label = "Plot shape",
            choices = c("L"), ## full list but not implemented yet c("C", "L", "P", "R", "S")
            selected = "L",
            inline = TRUE
          ),

          checkboxGroupInput(
            inputId = ns("allowable_error"),
            label = "Allowable error (%)",
            choices = c(1, 5, 10, 20),
            selected = 10,
            inline = TRUE
          ),

          br(),

          p(id = ns("msg_shape_L"),
            "Number of subplots selected not compatible with the plot shape chosen",
            style = "color: #dc3545; font-style: italic;"),  ## Bootstrap danger color

          actionButton(inputId = ns("calc_opti"), label = "Launch optimization script"),

          width = 4
        ), ## END sidebarPanel



        ## + Show progress and result overview ==============================

        mainPanel(

          ## * * Show parameters in R format --------------------------------
          p(strong("Checking parameters in R format:")),

          fluidRow(
            column(4, p("Number of subplots")),
            column(4, p("Distance between subplots")),
            column(4, p("Radius large subplot")),
          ),

          fluidRow(
            column(4, verbatimTextOutput(outputId = ns("out_subplot_count"))),
            column(4, verbatimTextOutput(outputId = ns("out_distance_multiplier"))),
            column(4, verbatimTextOutput(outputId = ns("out_nest1_radius"))),
          ),

          fluidRow(
            column(4, p("Radius medium subplot")),
            column(4, p("Plot shape")),
            column(4, p("Allowable error")),
          ),

          fluidRow(
            column(4, verbatimTextOutput(outputId = ns("out_nest2_radius"))),
            column(4, verbatimTextOutput(outputId = ns("out_plot_shape"))),
            column(4, verbatimTextOutput(outputId = ns("out_allowable_error")))
          ),

          br(),

          textOutput(outputId = ns("nb_combi")),

          br(),

          ## * * Show optimization progress ---------------------------------
          shinyjs::hidden(div(
            id = ns("opti_progress"),

            p(strong("Optimization script progress")),

            shinyWidgets::progressBar(
              id = ns("prog_opti"),
              value = 0,
              total = 10,
              title = "Optimization process",
              display_pct = TRUE
            ),

          )),

          ## * * Show preliminary results -----------------------------------
          shinyjs::hidden(div(
            id = ns("box_to_results"),

            div(
              plotOutput(outputId = ns("gr_cv_cost"), height = 400),
              style = "padding: 0.375em; border: 1px solid #e3e3e3;
            border-radius: 4px; width: 600px;
            margin: 0px auto;"
            ),

            br(),

            downloadButton(outputId = ns("download_results"), label = 'Download the results'),

            br(),

            h4(icon("arrow-right"),
               "Continue to:",
               HTML("&nbsp;"),
               actionButton(ns("btn_to_results"), "Results")
            )

          )),



          width = 8

        )

      ) ## END sidebar layout


    )) ## END div params_setup

  ) ## END tagList

} ## END function params_UI()
