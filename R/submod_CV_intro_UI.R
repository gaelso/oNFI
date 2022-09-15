

submod_CV_intro_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h4("Introduction"),

    ## Toggle on/off the introduction
    checkboxInput(
      inputId = ns("toggle_intro"),
      label = "Show the introduction",
      value = FALSE
    ),

    fluidRow(

      ## + Overview =========================================================
      column(4, wellPanel(
        id = ns("intro1"),

        h5(strong("Overview")),

        p("Presentation formula from Stein method 1945:
          $$n = \\left( \\frac{ \\hat{CV_{i}} \\times t_{n_{i} - 1}^{1 - \\frac{\\alpha}{2}} }{ E } \\right)^{2}$$"),
        p("This section focus on how CV can be derived from plot dimensions."),
        p("Two approaches are proposed here:"),
        HTML("<ol>
          <li>CV variation with plot size, CV obtain from biomass maps.</li>
          <li>CV as a function of plot size and distance from subplot from user inputs
          (Values can be modeled from large research forest plots for example).</li>
          </ol>"),

        class = "bg_explanations")),

      ## + Approach 1 =======================================================
      column(4, wellPanel(
        id = ns("intro2"),

        h5(strong("Approach 1")),

        p("In the absence of a CV model, approach 1 requires uploading an area of interest shapefile.
          Two biomass maps are then used to get a CV and a plot area (in this case pixel size): ",
          a("Avitabile et al. 2016", href = "https://doi.org/10.1111/gcb.13139"), "and ",
          a("Santoro et al. 2018",
            href = "https://globbiomass.org/wp-content/uploads/GB_Maps/Globbiomass_global_dataset.html"),
          "."),

        p("The forest inventory CV is then a function of the plot design and
          the initial CV from the biomass maps (",
          a("Lynch 2017", href = "https://doi.org/10.1093/forestry/cpw038"), "):"),

        p("$$CV_{opti} = \\sqrt{CV_{init} \\times \\left( \\frac{ A_{init} }{ A_{opti} } \\right)^{0.5} }$$"),

        p("With $A_{i}$ plot size or pixel size in ha of respectively the forest inventory
      or raster spatial data used to calculate $CV_{i}$. In case of forest inventory
      $A_{i} = n_{i} \\times a_{i}$, with $n_{i}$ the number of subplots in inventory $i$
      and $a_{i}$ the subplot size in ha."),

        p("This method takes longer time as it requires spatial analysis to get an initial CV for biomass.
          On the other hand it doesn't require any prior knowledge on the target forests."),

        class = "bg_explanations")),

      ## + Approach 2 =======================================================
      column(4, wellPanel(
        id = ns("intro3"),

        h5(strong("Approach 2")),

        p("If you already have a CV model, any variable can be used for design optimization.
        The application only needs the model parameters ",
          a("(Scott 1993)", href = "https://www.nrs.fs.fed.us/pubs/jrnl/1993/ne_1993_scott-c_001.pdf"), ":"),

        p("$$CV_{opti} = \\beta_{0} \\times n^{\\beta_{1}} \\times d^{\\beta_{2}} \\times a^{\\beta_{3}}$$"),

        p("With: $n$ the number of subplots, $d$ the distance between subplots,
        $a$ the subplot size in ha and $\\beta_{0}$, $\\beta_{1}$, $\\beta_{2}$,
        $\\beta_{3}$ the model's parameters."),

        class="bg_explanations"))

    ) ## END fluidRow intro

  )
}
