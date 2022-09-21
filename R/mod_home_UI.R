

#' Home module UI function
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_home_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    br(),

    h2("Welcome to oNFI", align = 'center'),

    h2("the Optimization tool for Forest Inventory Design", align = 'center'),

    br(), br(),



    ##
    ## Introduction #########################################################
    ##

    wellPanel(
      h3("Context"),

      h4("Forest inventory design"),

      p("Forest inventory design refers to creating a robust statistical design
        that will allow us to measure forest characteritiscs in as few forest
        plots as possible to get robust forest estimates with a known uncertainty.
        We need to estimate:"),

      HTML("<ol>
        <li>How many forest plots are necessary to measure to get reliable
            estimates and where are they located, this is the <strong>sampling
            design</strong>.</li>
        <li>What will the plots look like (how many subplots, what distance
            between them, what size), this is the <strong>plot design</strong>.</li>
        <li>What statistical formulas are required to calculate our variable of
            interest (for example, average biomass at national level), this is the
            <strong>estimation design</strong>.</li>
        </ol>"),

      br(),

      h4("Sampling size"),

      p("The forest inventory ", strong("sampling size"), ", i.e. the number
        of plots to measure to get our forest statistics, can be estimated
        with prior knowledge on at least one of the forest variables of interest."),

      p("Although forest inventories and in particular National Forest inventories
        are multi-purpose in nature, one of the variables (for example: biomass,
        merchantable volume, forest area, etc.), one of these variables must be
        chosen to estimate the optimal number of plots."),

      p("For this variable of interest, we need a estimated average and standard
        deviation. They can be obtained from a small scale preliminary inventory,
        or in the absence of any ground data, it could be obtained from biomass
        maps. With this information the number of plots to measure is (Stein
        method 1945):"),

      p(
        "$$n = \\left( \\frac{ \\hat{CV_{i}} \\times t_{n_{i} - 1}^{1 - \\frac{\\alpha}{2}} }{ E } \\right)^{2}$$"
        ),

      p("With $n$ the number of plots, $\\hat{CV_{i}}$ the coefficient of
        variation for the forest variable $i$ from a preliminary inventory,
        $t_{n_{i} - 1}^{1 - \\frac{\\alpha}{2}}$ the student law for the
        confidence level $(1 - \\alpha)$ and $E$ the allowable error"),

      br(),

      h4("Optimization"),

      p("The above formula gives an initial sampling size, but doesn't take
        into consideration the ",  strong(em("plot design")), ". In particular,
        the ",  strong("number of subplots"), " and their ",  strong("size"),
        " has an impact on both the forest variables' CV and the cost of the
        forest inventory, as it impacts the number of plots to measure and
        the time for measuring forest attributes. The ", strong("distance"),
        " between subplots is also important due to the forest autocorrelation.
        It means that spreading subplots further apart increases the chance to
        capture more variability but also increases the costs of the forest
        inventory."),

      p("In this context, the optimization process aims to test various plot
        designs, calculate their sampling size, coefficient of variation
        and costs and select the best designs for either minimizing the CV,
        the costs or finding a good trade-off between both."),

      ),



    ##
    ## How the app works ####################################################
    ##

    wellPanel(

      h3("How the app works"),

      p("The optimization tool requires two sets of user inputs: one for modelling
        the coefficient of variation from your main forest variable, and one for
        setting the unit times of the main forest inventory operations."),

      p("Two approaches are available for modelling CV:"),

      p("(1) in the absence of an existing model, the tool downloads biomass raster data
        maps and requires an area of interest spatial data file to clip the biomass
        maps. Then a CV is calculated from the biomass , using the pixel size as plot
        size. This step is slightly slower due to the spatial data analysis implemented
        to get an initial CV, but doesn't require any prior knowledge on the forest."),

      p("(2) With an exisitng model, users can simply enter the model parameters."),

      p("Once user inputs are set, the optimization parameters can be filled in, then
        the optimization process tests all the parameters' combinations returns the
        optimal designs."),

    ),



    ##
    ## Inputs description ###################################################
    ##

    h3("User inputs"),

    fluidRow(
      column(4, wellPanel(
        h4("1. Coefficient of variation"),
        br(),
        p("To be improved: this section presents the biomass maps and how they are used to
          calc CV.", align = "center"),
        class="bg1")),

      column(4, wellPanel(
        h4("2. Unit times"),
        br(),
        p("To be improved: Unit times is divided into times for measuring trees,
          based on nested subplot levels and time for other operations such as
          transportation, requiring authorization, etc.", align = "center"),
        class="bg2")),

      column(4, wellPanel(
        h4("3. Optimization parameters"),
        br(),
        p("To be improved: This is where users choose the different design
          paramters to be tested.", align = "center"),
        class="bg3")),

    ), ## End fluidRow



    ##
    ## To next page button ##################################################
    ##

    fluidRow(
      h4(icon("arrow-right"), "Continue to Step 1:", HTML("&nbsp;"),
        actionButton(ns("btn_to_cv"), "CV model")
        )
      ) ## END fluidRow

  ) ## END tagList

} ## END function home_UI()
