
params_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h3("Forest inventory optimization parameters", alignn = "center"),

    br(),


    ## Default message ######################################################
    div(id = ns("check_approach"), fluidRow(

      wellPanel(
        p("Make sure you fill in the CV model approach and complete the approach 1
          if you don't have any CV model to apply before coming to this section",
          style = "color:red; font-style:italic; text-align:center;")
      )

    )), ## End check_approach

    ## Parameters interface #################################################
    shinyjs::hidden(div(id = ns("params_setup"), wellPanel(

      ## Steps 1 and 2 ######################################################
      fluidRow(

        ## + Step 1: Model parameters =======================================
        column(width = 6, wellPanel(
          h4("1. CV model parameters"),

          ## + + IF Approach 1 for CV ---------------------------------------
          conditionalPanel(
            rv$CV_model$cv_approach == "a1", ns = ns,

            tableOutput(outputId = ns("CV_a1")),

            br(),

            p("To refine the grid spacing, please provide the estimated forest cover of you AOI in %."),

            numericInput(ns("forest_cover"), "Forest cover in %", value = 50, min = 0, max = 100),

            ), ## END conditionalPanel A1

          ## + + IF Approach 2 for CV ---------------------------------------
          conditionalPanel(
            rv$CV_model$cv_approach == "a1", ns = ns,

            )

          )), ## END wellPanel step 1



        ## + Step 2: Nested plot conditions =================================
        column(width = 6, wellPanel(
          h4("2. Nested plot conditions")

          ))

      ), ## End fluidRow



      ## Steps 3 and 4 ######################################################
      fluidRow(

        ## + Step 3: Unit times =============================================
        column(width = 6, wellPanel(
          h4("3. Unit times")
        )),


        ## + Step 4: Optimization parameters ================================
        column(width = 6, wellPanel(
          h4("4. Optimization parameters")

        ))
      ),




    ))) ## End params_setup







  ) ## END tagList

} ## END function params_UI()
