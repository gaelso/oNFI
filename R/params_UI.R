
params_UI <- function(id){

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



        ## + Step 1: Model parameters =======================================

        # column(width = 3, wellPanel(
        #   h4("1. CV model parameters"),
        #
        #   ## + + IF Approach 1 for CV ---------------------------------------
        #   shinyjs::hidden(div(id = ns("params_a1"),
        #
        #     h5(HTML(paste0("Inputs from biomass maps for ", strong("approach 1"), " CV model:"))),
        #
        #     p("$$CV_{opti} = \\sqrt{CV_{init} \\times \\left( \\frac{ area_{CV_{init}} }{ area_{CV_{opti}} } \\right)^{0.5} }$$"),
        #
        #     tableOutput(outputId = ns("CV_a1")),
        #
        #     hr(),
        #
        #     p(HTML(paste0("Based on the spatial data uploaded, the AOI total area
        #                   in km", tags$sup(2), " is:"))),
        #
        #     textOutput(outputId = ns("area_aoi_a1")),
        #
        #
        #     )), ## END A1 params
        #
        #   ## + + IF Approach 2 for CV ---------------------------------------
        #   shinyjs::hidden(div(id = ns("params_a2"),
        #
        #     h5(strong("Inputs parameters for approach 2 CV model:")),
        #
        #     p("$$CV_{opti} = \\beta_{0} \\times n^{\\beta_{1}} \\times d^{\\beta_{2}} \\times A^{\\beta_{3}}$$"),
        #
        #     p("With: $n$ the number of subplots, $d$ the distance between subplots and $A$ the subplot size in ha,
        #       $\\beta_{0}$, $\\beta_{1}$, $\\beta_{2}$ and $\\beta_{3}$ the model parameters to fill in:"),
        #
        #     fluidRow(
        #       column(3, numericInput(inputId = ns("beta0"), label = "$\\beta_{0}$",
        #                              value = 1.02, min = 0, max = 10, step = 0.001)),
        #       column(3, numericInput(inputId = ns("beta1"), label = "$\\beta_{1}$",
        #                              value = -0.15, min = -1, max = 1, step = 0.001)),
        #       column(3, numericInput(inputId = ns("beta2"), label = "$\\beta_{2}$",
        #                              value = 0.016, min = -0.1, max = 0.1, step = 0.001)),
        #       column(3, numericInput(inputId = ns("beta3"), label = "$\\beta_{3}$",
        #                              value = -0.12, min = -1, max = 1, step = 0.001))
        #       ),
        #
        #     hr(),
        #
        #     h5(strong("Additional input parameters:")),
        #
        #     numericInput(inputId = ns("aoi_area_a2"), label = HTML(paste0("area of interest in km", tags$sup(2))), value = 0)
        #
        #     )), ## END A2 params
        #
        #   ## + + Collect forest cover percentage in any case ----------------
        #   numericInput(ns("forest_cover"), "AOI forest cover in %", value = 50, min = 0, max = 100, step = 1)
        #
        #   )), ## END wellPanel step 1



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
