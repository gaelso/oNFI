
mod_params_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h3("Forest inventory optimization parameters", align = "center"),

    br(),

    ## !!! For testing only !!!
    hr(),

    textOutput(outputId = ns("test_approach")),

    tableOutput(outputId = ns("test_cv")),

    hr(),



    ## Default message ######################################################

    div(id = ns("check_approach"), fluidRow(

      wellPanel(
        p("Make sure you fill in the CV model approach and complete the approach 1
          if you don't have any CV model to apply before coming to this section",
          style = "color:red; font-style:italic; text-align:center;")
      )

    )), ## End check_approach



    ## Parameters interface #################################################

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
