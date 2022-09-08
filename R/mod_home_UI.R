

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

    ## + Introduction -------------------------------------------------------
    fluidRow(
      wellPanel(
        h3("In a Nutshell, how the app works"),
        h4("Forest inventory design..."),
        p("... refers to creating a robust statistical design that will allow
         us to measure forest characteritiscs in as few forest plots as possible
         to get robust forest estimates with a known uncertainty. We need to estimate"),
        HTML("<ol>
          <li>How many forest plots are necessary to measure to get reliable
              estimates, this is the <strong>sampling design</strong>.</li>
          <li>What will the plots looks like (how many subplots, what distance
              between them, what size), this is the <strong>plot design</strong>.</li>
          <li>What statistical formulas are required to calculate our variable of
              interest (for example, average biomass at national level), this is the
              <strong>estimation design</strong>.</li>
          </ol>"),
        br(),
        h4("The optimization tool..."),
        p("... let you choose between optimizing forest inventory design for
        forest aboveground biomass using pan-tropical biomass maps or a set of
        user defined parameters. Additional screens prompt for other input parameters
        dealing with cost and forest conditions, then optimize sampling and plot designs
        for your area of interest. The optimization function aims to achieve either
        minimal variance, minimal cost or a balanced solution."),
        br(),
        h4("The application builds on four sets of user inputs described below."),
        )
    ), ## END fluidRow()

    ## + Description inputs -------------------------------------------------
    fluidRow(
      column(6, wellPanel(
        h3("1. Biomass variability"),
        br(),
        p("!!! This section presents the biomass maps and how they are used to
          calc CV !!!", align = "center"),
        class="bg1")),

      column(6, wellPanel(
        h3("2. Nested plot conditions"),
        br(),
        p("!!! This section aims to characterize forest condfitions for different
          tree diameter classes to optimize the respective size of 2 nested subplots for
          small and big trees (seedling fixed to 2.5 m radius circle for now) !!!", align = "center"),
        class="bg2"))
    ), ## End fluidRow

    ## + Description inputs cont. -------------------------------------------
    fluidRow(
      column(6, wellPanel(
        h3("3. Unit times"),
        br(),
        p("!!! This section deals with different unit times necessary to calculate
          the total cost of forest measurements !!!", align = "center"),
        class="bg3")),

      column(6, wellPanel(
        h3("4. Optimization parameters"),
        br(),
        p("!!! List of parameters to be tested, the more parameters the more
          total number of designs need to be tested !!!", align = "center"),
        class="bg4"))
    ), ## End fluidRow

    ## + To next page button ------------------------------------------------
    fluidRow(
      h4(icon("arrow-right"), "Continue to Step 1:", HTML("&nbsp;"),
        actionButton(ns("btn_to_CV"), "CV model")
        )
      ) ## END fluidRow

  ) ## END tagList

} ## END function home_UI()
