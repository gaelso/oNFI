
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

    shinyjs::hidden(div(id = ns("params_setup"), fluidRow(


        ## + Step 2: Nested plot conditions =================================

        column(width = 4, wellPanel(
          h4("2. Nested plot conditions")

          )),



        ## + Step 3: Unit times =============================================

        column(width = 4, wellPanel(
          h4("3. Unit times")
        )),


        ## + Step 4: Optimization parameters ================================
        column(width = 4, wellPanel(
          h4("4. Optimization parameters")

        ))

    ))), ## End params_setup



    ## Show params quick consequences #######################################

    shinyjs::hidden(
      tabsetPanel(
        id = ns("params_show"),

        tabPanel("CV"),
        tabPanel("Nested plot"),
        tabPanel("Unit times"),
        tabPanel("Optimization")

      ),

      ## !!! TO BE REMOVED !!!
      p("!!! Placeholder for showing info based on selected parameters !!!")

    ) ## End params show

  ) ## END tagList

} ## END function params_UI()
