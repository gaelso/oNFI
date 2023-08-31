#' Inventory unit time module UI function
#'
#' @description Shiny module to set unit times for forest inventory operations.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_time_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h3("Unit times for the different forest inventory operations", align = "center"),

    br(),



    ##
    ## Check messages #######################################################
    ##

    hr(),

    textOutput(outputId = ns("test_approach")),

    tableOutput(outputId = ns("test_cv")),

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
      id = ns("time_intro"),

      p(strong("Overview")),

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

    br(),



    ## + Section titles ======================================================

    fluidRow(
      column(6, h4("Nested plot unit times")),
      column(6, h4("Other unit times"))
    ),


    br(),



    ##
    ## Message if CV not done ################################################
    ##

    wellPanel(
      id = ns("require_cv"),

      p("First choose a CV model approach",
        style = "color: #dc3545; font-style: italic; text-align: center;"),  ## Bootstrap danger color

      p("If you selected approach 1, make sure you complete all the steps to
        get an initial CV from the biomass maps.",
        style = "color: #dc3545; font-style: italic; text-align: center;"),  ## Bootstrap danger color

    ), ## End check_approach



    ##
    ## Time units to set #####################################################
    ##

    shinyjs::hidden(div(
      id = ns("content_time"),

      fluidRow(


        ## + Nested plot inputs =============================================

        column(6, wellPanel(
          id = ns("nested_plot"),

          ## + + Level 1 big trees ----------------------------------------
          h4("Level 1 subplot for big trees"),

          fluidRow(
            column(4, p(strong("Tree minimum DBH (cm)"))),
            column(4, p(strong("Tree density (#/ha)"))),
            column(4, p(strong("Time to measure (min/tree)")))
          ),

          fluidRow(

            column(4, numericInput(
              inputId = ns("lvl1_dbh"),
              label = NULL,
              value = 30,
              min = 1,
              max = 100
            )),

            column(4, numericInput(
              inputId = ns("lvl1_density"),
              label = NULL,
              value = 300,
              min = 1,
              max = 10000
            )),

            column(4, numericInput(
              inputId = ns("lvl1_time"),
              label = NULL,
              value = 3,
              min = 0.1,
              max = 10
            ))

          ),

          br(),

          ## + + Level 2 small trees --------------------------------------
          h4("Level 2 subplot for small trees"),

          fluidRow(
            column(4, p(strong("Tree minimum DBH (cm)"))),
            column(4, p(strong("Tree density (#/ha)"))),
            column(4, p(strong("Time to measure (min/tree)")))
          ),

          fluidRow(

            column(4, numericInput(
              inputId = ns("lvl2_dbh"),
              label = NULL,
              value = 10,
              min = 1,
              max = 100
            )),

            column(4, numericInput(
              inputId = ns("lvl2_density"),
              label = NULL,
              value = 1000,
              min = 1,
              max = 10000
            )),

            column(4, numericInput(
              inputId = ns("lvl2_time"),
              label = NULL,
              value = 2,
              min = 0.1,
              max = 5
            ))

          ),

          br(),

          ## + + Level 3 seedlings ----------------------------------------
          h4("Level 3 subplot for seedlings"),

          fluidRow(
            column(4, p(strong("Seedling minimum DBH (cm)"))),
            column(4, p(strong("Seedling density (#/ha)"))),
            column(4, p(strong("Seedling to measure (min/tree)")))
          ),

          fluidRow(

            column(4, numericInput(
              inputId = ns("lvl3_dbh"),
              label = NULL,
              value = 2,
              min = 1,
              max = 100
            )),

            column(4, numericInput(
              inputId = ns("lvl3_density"),
              label = NULL,
              value = 1500,
              min = 1,
              max = 10000
            )),

            column(4, numericInput(
              inputId = ns("lvl3_time"),
              label = NULL,
              value = 0.5,
              min = 0.1,
              max = 3,
              step = 0.1
            ))

          ),

          hr(),

          ## + + Default values check -------------------------------------
          p("This table recaps where default
            parameters are used."),

          tableOutput(outputId = ns("nested_check"))

        )), ## END wellPanel nested plots


        ## + Other time inputs ==============================================

        column(6, wellPanel(
          id = ns("other_time"),

          sliderInput(
            inputId = ns("drive_time"),
            label = "Average driving time to plot area (hr)",
            value = 0.5,
            min = 0.1,
            max = 3,
            step = 0.1
          ),

          sliderInput(
            inputId = ns("walk_time"),
            label = "Average walking time to plot (hr)",
            value = 1,
            min = 0.1,
            max = 3,
            step = 0.1
          ),

          sliderInput(
            inputId = ns("march_speed"),
            label = "March speed between subplots (km/hr)",
            value = 2,
            min = 0.5,
            max = 6,
            step = 0.5
          ),

          sliderInput(
            inputId = ns("auth_time"),
            label = "Time to get authorization for each plot (hr)",
            value = 2,
            min = 1,
            max = 6,
            step = 1
          ),

          sliderInput(
            inputId = ns("working_hour"),
            label = "Number of working hours per day",
            value = 9,
            min = 1,
            max = 12,
            step = 1
          ),

          sliderInput(
            inputId = ns("working_day"),
            label = "Number of working days per month",
            value = 21,
            min = 10,
            max = 26,
            step = 1
          ),

          ##!!!
          radioButtons(
            inputId = ns("complex_time"),
            label = "Simple time or more advanced time function?",
            choices = c("Simple", "Advanced"), ## full list but not implemented yet c("C", "L", "P", "R", "S")
            selected = "Simple",
            inline = TRUE
          ),

          hr(),

          p("This table recaps where default
            parameters are used."),

          tableOutput(outputId = ns("other_check1")),

          tableOutput(outputId = ns("other_check2"))

        )) ## END wellPanel other unit times

      ), ## END fluidRow inputs



      ##
      ## Move to next section #################################################
      ##

      h4(icon("arrow-right"),
         "Continue to Step 3:",
         HTML("&nbsp;"),
         actionButton(ns("btn_to_opti"), "Optimization parameters")
         )

    )) ## END div content_time

  ) ## END tagList

} ## END function module UI
