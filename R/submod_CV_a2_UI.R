

#' CV approach 2 submodule UI function
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
submod_CV_a2_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h4(strong("Input parameters for approach 2:")),

    h4("$$CV_{opti} = \\beta_{0} \\times n^{\\beta_{1}} \\times d^{\\beta_{2}} \\times a_{1}^{\\beta_{3}} \\times a_{2}^{\\beta_{4}}$$"),

    p("With: $n$ the number of subplots, $d$ the distance between subplots,
        $a_{1}$ and $a_{2}$ the subplot size in ha for level 1 and 2 of nested subplots,
        and $\\beta_{0} ... \\beta_{4}$ the model's parameters."),


    ## + User inputs ====================================================

    wellPanel(

      h4("Fill in the model parameters:"),

      p("The CV model parameters are pre-filled with default values from a Forest
        Inventory in the tropics. They are intended for testing purposes and should
        be replaced with values from your area of interest or similar conditions.",
        style = "color: #17a2b8; font-style: italic;"), ## bootstrap info color

      fluidRow(

        column(2, offset = 1, numericInput(inputId = ns("beta0"), label = "$\\beta_{0}$",
                               value = 1.02, min = 0, max = 10, step = 0.001)),

        column(2, numericInput(inputId = ns("beta1"), label = "$\\beta_{1}$",
                               value = -0.15, min = -1, max = 1, step = 0.001)),

        column(2, numericInput(inputId = ns("beta2"), label = "$\\beta_{2}$",
                               value = 0.016, min = -0.1, max = 0.1, step = 0.001)),

        column(2, numericInput(inputId = ns("beta3"), label = "$\\beta_{3}$",
                               value = -0.12, min = -1, max = 1, step = 0.001)),

        column(2, numericInput(inputId = ns("beta4"), label = "$\\beta_{4}$",
                               value = 0, min = -1, max = 1, step = 0.001))

        ),

      ),


    ## + Model test results =============================================

    fluidRow(

      column(6, div(
        p("This table recaps where default parameters are used:"),

        tableOutput(outputId = ns("a2_check"))

      )),

      column(6, div(
        p(strong("Parameters test:")),

        textOutput(outputId = ns("a2_test"))

      ))

    )

  )
}
