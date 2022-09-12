

mod_CV_sub_a2_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h4(strong("Input parameters for approach 2:")),

    h4("$$CV_{opti} = \\beta_{0} \\times n^{\\beta_{1}} \\times d^{\\beta_{2}} \\times a^{\\beta_{3}}$$"),

    p("With: $n$ the number of subplots, $d$ the distance between subplots,
        $a$ the subplot size in ha and $\\beta_{0}$, $\\beta_{1}$, $\\beta_{2}$,
        $\\beta_{3}$ the model's parameters."),

    sidebarLayout(



      ## + User inputs ====================================================

      sidebarPanel(

        h4("Fill in the model parameters:"),

        p("The CV model parameters are pre-filled with default values from a Forest
          Inventory in the tropics. They are intended for testing purposes and should
          be replaced with values from your area of interest or similar conditions.",
          style = "color: #17a2b8; font-style: italic;"), ## bootstrap info color

        fluidRow(

          column(3, numericInput(inputId = ns("beta0"), label = "$\\beta_{0}$",
                                 value = 1.02, min = 0, max = 10, step = 0.001)),

          column(3, numericInput(inputId = ns("beta1"), label = "$\\beta_{1}$",
                                 value = -0.15, min = -1, max = 1, step = 0.001)),

          column(3, numericInput(inputId = ns("beta2"), label = "$\\beta_{2}$",
                                 value = 0.016, min = -0.1, max = 0.1, step = 0.001)),

          column(3, numericInput(inputId = ns("beta3"), label = "$\\beta_{3}$",
                                 value = -0.12, min = -1, max = 1, step = 0.001))

        ),

        width = 8),


      ## + Model test results =============================================

      mainPanel(

        p("This table recaps where default parameters are used:"),

        tableOutput(outputId = ns("a2_check")),

        p(strong("Parameters test:")),

        textOutput(outputId = ns("a2_test")),

        width = 4) ## End Mainpanel A2 results

    ) ## END sidebar layout

  )
}
