


mod_time_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h3("Unit time for the different forest inventory operations", align = "center"),

    br(),


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
      id = ns("time_intro"),

    p("This section is dedicated to unit times necessary to perform the main
      inventory operations, such as time to measure trees, time to get to the
      plots etc. The user inputs are split in two boxes."),

    p(strong("Box 1")),

    p("Box 1 focuses on tree measurements following a nested circular subplot
      design with three nested levels. Level 1 is the largest circle dedicated
      to large trees (usually bigger than 30 cm diameter at breast height - DBH),
      level 2 is a smaller circle for small trees (from tree minimum DBH to the
      threshold for big trees), and level 3 is a small circle for seedlings."),

    p(strong("Box 2")),

    p("Box 2 focuses on unit times for transportation, general working hours and
      time to get authorization from local authorities before going to measure
      the plots.")

    ),



    ##
    ## Inputs ###############################################################
    ##


    fluidRow(

      ## + Nested plot inputs ===============================================

      column(6, wellPanel(
        id = ns("nested_plot"),

        fluidRow(

          ## + + Level 1 big trees --------------------------------------------
          column(4, div(

            h4("Level 1 subplot for big trees"),

            numericInput(
              inputId = ns("lvl1_dbh"),
              label = "DBH min. (cm)",
              value = 30,
              min = 1,
              max = 100
            ),

            numericInput(
              inputId = ns("lvl1_density"),
              label = "Tree density for DBH range (# trees/ha)",
              value = 300,
              min = 1,
              max = 10000
            ),

            numericInput(
              inputId = ns("lvl1_time"),
              label = "Unit time to measure 1 tree (min/tree)",
              value = 3,
              min = 0.1,
              max = 10
            )

          )),

          ## + + Level 2 small trees ----------------------------------------
          column(4, div(

            h4("Level 2 subplot for small and medium trees"),

            numericInput(
              inputId = ns("lvl2_dbh"),
              label = "DBH min. (cm)",
              value = 10,
              min = 1,
              max = 100
            ),

            numericInput(
              inputId = ns("lvl2_density"),
              label = "Tree density for DBH range (# trees/ha)",
              value = 1000,
              min = 1,
              max = 10000
            ),

            numericInput(
              inputId = ns("lvl2_time"),
              label = "Unit time to measure 1 tree (min/tree)",
              value = 2,
              min = 0.1,
              max = 5
            )

          )),

          ## + + Level 3 seedlings ------------------------------------------
          column(4, div(

            h4("Level 3 subplot for seedlings"),

            numericInput(
              inputId = ns("lvl3_dbh"),
              label = "DBH min. (cm)",
              value = 2,
              min = 1,
              max = 100
            ),

            numericInput(
              inputId = ns("lvl3_density"),
              label = "Tree density for DBH range (# seedlings/ha)",
              value = 1500,
              min = 1,
              max = 10000
            ),

            numericInput(
              inputId = ns("lvl3_time"),
              label = "Unit time to measure 1 seedling (min/seedling)",
              value = 0.5,
              min = 0.1,
              max = 3,
              step = 0.1
            )

          ))

        ), ## END fluidRow nested plot

        hr(),

        p("The default parameters come from a Forest Inventory in the tropics and
        are intended for testing purposes only. The table below recaps where default
          parameters are used."),

        tableOutput(outputId = ns("nested_check")),

      )),

      ## + Other time inputs ================================================
      column(6, wellPanel(
        id = ns("other_time"),





      ))

    ), ## END fluidRow inputs


    ## Move to next section #################################################

    shinyjs::hidden(div(
      id = ns("box_cv_to_params"),

      h4(icon("arrow-right"),
         "Continue to Step 2:",
         HTML("&nbsp;"),
         actionButton(ns("btn_to_params"), "Optimization parameters")
      )

    ))

  ) ## END tagList

} ## END function module UI
