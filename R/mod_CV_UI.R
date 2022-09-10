
mod_CV_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h3("Coefficient of Variation for your main forest inventory variable", align = "center"),

    br(),


    ##
    ## Introduction #########################################################
    ##

    mod_CV_sub_intro_UI(ns("cv_intro")),

    # h4("Introduction"),
    #
    # ## Toggle on/off the introduction
    # checkboxInput(
    #   inputId = ns("toggle_intro"),
    #   label = "Show the introduction",
    #   value = FALSE
    #   ),
    #
    # fluidRow(
    #
    #   column(4, wellPanel(
    #     id = ns("intro1"),
    #
    #     h5(strong("Overview")),
    #
    #     p("Presentation formula from Stein method 1945:
    #       $$n = \\left( \\frac{ \\hat{CV_{i}} \\times t_{n_{i} - 1}^{1 - \\frac{\\alpha}{2}} }{ E } \\right)^{2}$$"),
    #     p("This section focus on how CV can be derived from plot dimensions."),
    #     p("Two approaches are proposed here:"),
    #     HTML("<ol>
    #       <li>CV variation with plot size, CV obtain from biomass maps.</li>
    #       <li>CV as a function of plot size and distance from subplot from user inputs
    #       (Values can be modeled from large research forest plots for example).</li>
    #       </ol>"),
    #
    #   class = "bg_explanations")),
    #
    #
    #   column(4, wellPanel(
    #     id = ns("intro2"),
    #
    #     h5(strong("Approach 1")),
    #
    #     p("In the absence of a CV model, approach 1 requires uploading and area of interest shapefile.
    #       Two biomass maps are then used to get a CV and a plot area (in this case pixel size): ",
    #       a("Avitabile et al. 2016", href = "https://doi.org/10.1111/gcb.13139"), "and ",
    #       a("Santoro et al. 2018",
    #         href = "https://globbiomass.org/wp-content/uploads/GB_Maps/Globbiomass_global_dataset.html"),
    #       "."),
    #
    #     p("The forest inventory CV is then adapted to different plot sizes with the following formula (",
    #       a("Lynch 2017", href = "https://doi.org/10.1093/forestry/cpw038"), "):"),
    #
    #     p("$$CV_{opti} = \\sqrt{CV_{init} \\times \\left( \\frac{ A_{init} }{ A_{opti} } \\right)^{0.5} }$$"),
    #
    #     p("With $A_{i}$ plot size or pixel size in ha of respectively the forest inventory
    #   or raster spatial data used to calculate $CV_{i}$. In case of forest inventory
    #   $A_{i} = n_{i} \\times a_{i}$, with $n_{i}$ the number of subplots in inventory $i$
    #   and $a_{i}$ the subplot size in ha."),
    #
    #     p("This method takes longer time as it requires spatial analysis to get an initial CV for biomass.
    #       On the other hand it doesn't any prior knowledge on the target forests."),
    #
    #     class = "bg_explanations")),
    #
    #
    #   column(4, wellPanel(
    #     id = ns("intro3"),
    #
    #     h5(strong("Approach 2")),
    #
    #     p("If you already have a CV model, any main variable can be selected and the application
    #       only needs the model parameters ",
    #       a("(Scott 1993)", href = "https://www.nrs.fs.fed.us/pubs/jrnl/1993/ne_1993_scott-c_001.pdf"), ":"),
    #
    #     p("$$CV_{opti} = \\beta_{0} \\times n^{\\beta_{1}} \\times d^{\\beta_{2}} \\times a^{\\beta_{3}}$$"),
    #
    #     p("With: $n$ the number of subplots, $d$ the distance between subplots,
    #     $a$ the subplot size in ha and $\\beta_{0}$, $\\beta_{1}$, $\\beta_{2}$,
    #     $\\beta_{3}$ the model's parameters."),
    #
    #     class="bg_explanations"))
    #
    # ), ## END fluidRow intro



    ##
    ## Select approach ######################################################
    ##

    h4("Select an approach"),

    wellPanel(
      div(
        shinyWidgets::radioGroupButtons(
          inputId = ns("approach"),
          label = "",
          choiceNames = c(
            'Approach 1: Biomass map' ,
            'Approach 2: CV model'
          ),
          choiceValues = c("a1", "a2"),
          selected = "a1",
          checkIcon = list(
            yes = icon("ok",
                       lib = "glyphicon")),
          justified = FALSE
        ),
        align = "center"),

      div(
        actionButton(inputId = ns("start_CV"), label = "Continue")
      ),

      class="bg1"), ## END wellPanel approach



    ##
    ## Approach 1: AGB map Sidebar Layout ###################################
    ##

    shinyjs::hidden(div(
      id = ns("layout_a1"),

      h4(strong("Input parameters for approach 1:")),

      h4("$$CV_{opti} = \\sqrt{CV_{init} \\times \\left( \\frac{ A_{init} }{ A_{opti} } \\right)^{0.5} }$$"),

      p("With $A_{i}$ plot size or pixel size in ha of respectively the forest inventory
      or raster spatial data used to calculate $CV_{i}$. In case of forest inventory
      $A_{i} = n_{i} \\times a_{i}$, with $n_{i}$ the number of subplots in inventory $i$
      and $a_{i}$ the subplot size in ha."),

      p("Approach 1 uses pan-tropical biomass maps to calulate $CV_{init}$ and $A_{init}$
        so that for each combination of plot characteristics of the optimization process
        $A_{opti}$ a CV is calculated and compared to the costs of implementing this plot
        design."),


      sidebarLayout(



        ## + User inputs ====================================================

        sidebarPanel(

          ## + + Step 1: Set a path to data ---------------------------------
          div(
            id = ns("step_path_data"),

            h4("Select a folder"),

            p("Select a destination folder for spatial data. If you don't the data
          will be downloaded in a temporary folder."),

            shinyFiles::shinyDirButton(
              id = ns('folder'),
              label = 'Select a folder',
              title = 'Please select a folder',
              FALSE
            ),

            div(
              p("Path:"),
              textOutput(ns("show_path")),
              style = "padding: 0.375em; border: 1px solid #e3e3e3;
            border-radius: 4px; font-weight: bold; text-align: center;
            margin-top: 5px; margin-bottom: 5px;"
            )

          ),

          hr(),

          ## + + Step 2: Get Area of Interest -------------------------------
          div(
            id = ns("step_aoi_file"),

            h4("Select an Area of Interest (AOI)"),

            p("Accepted file types are geoJSON ('.geoJSON') and GeoPackage ('.GPKG').
            The maximum file size allowed is 10 Mo."),

            fileInput(ns("AOI"), "Upload an AOI shapefile:",
                      multiple = F, accept = c(".geoJSON", ".GPKG")),

            div(
              plotOutput(outputId = ns("map_aoi"), height = 120),
              style = "padding: 0.375em; border: 1px solid #e3e3e3;
            border-radius: 4px; width: 160px;
            margin: 5px auto;"
              )

            ),

          hr(),

          ## + + Step 3: Default values -------------------------------------
          div(
            id = ns("step_agb_min"),

            ## !!! TO BE MOVED TO POPUP EXPLANATION
            # p("The maps provide AGB estimates for all land use types, with values
            #   ranging from 0 to around 700 ton/ha. As forest lands are expected to
            #   have a minimum AGB, this value can be selected below to reduce the CV
            #   estimate from the maps. Default to 0, meaning all land is considered
            #   in the calculation of CV"),

            numericInput(
              inputId = ns("agb_min"),
              label = "Pick a minimum AGB value for forest (in ton/ha)",
              value = 0,
              min = 0,
              max = 100
            )

          ),

          hr(),

          ## + + Step 4: Launch calculations --------------------------------
          p(id = ns("msg_step_path_data"),
            "If you don't select a folder, the data will be downloaded in a temporary folder.",
            style = "color: #17a2b8; font-style: italic;"), ## bootstrap info color

          shinyjs::hidden(p(
            id = ns("msg_step_path_data_ok"),
            "Folder selected!",
            style = "color: #28a745; font-style: italic;")),  ## Bootstrap success color

          p(id = ns("msg_step_aoi_file"),
            "A valid spatial file is required to launch the calculations",
            style = "color: #dc3545; font-style: italic;"),  ## Bootstrap danger color

          shinyjs::hidden(p(
            id = ns("msg_step_aoi_file_ok"),
            "Valid spatial file loaded!",
            style = "color: #28a745; font-style: italic;")),  ## Bootstrap success color

          p(id = ns("msg_step_agb_min"),
            "Keeping 0 as minimum AGB increases the risk of including non-forest land in the CV calculations.",
            style = "color: #17a2b8; font-style: italic;"), ## bootstrap info color

          shinyjs::hidden(p(
            id = ns("msg_step_agb_min_ok"),
            "Minimum AGB  set!",
            style = "color: #28a745; font-style: italic;")),  ## Bootstrap success color

          shinyjs::disabled(actionButton(inputId = ns("calc_CV"), label = "Calculate CV")),



          width = 4), ## END Sidebar panel



        ## + Progress bars and results ======================================

        mainPanel(

          br(),

          ## + + Progress bars ----------------------------------------------
          shinyjs::hidden(div(
            id = ns("panel_a1_progress"),

            shinyWidgets::progressBar(
              id = ns("prog_checks"),
              value = 0,
              title = "Check input",
              display_pct = TRUE
            ),

            shinyWidgets::progressBar(
              id = ns("prog_avit"),
              value = 0,
              title = "Prepare Avitabile et al. 2016 raster data",
              display_pct = TRUE
            ),

            shinyWidgets::progressBar(
              id = ns("prog_sant"),
              value = 0,
              title = "Prepare Santoro et al. 2018 raster data",
              display_pct = TRUE
            ),

            shinyWidgets::progressBar(
              id = ns("prog_CV"),
              value = 0,
              title = "Calculate initial CV for optimization",
              display_pct = TRUE
            ),

          )), ## END progress div

          ## + + Show results -----------------------------------------------
          shinyjs::hidden(div(
            id = ns("box_progress_to_results"),

            actionButton(inputId = ns("btn_show_results"), label = "Show results")

          )), ## END to results


          ## + + Maps and CV tables -----------------------------------------
          shinyjs::hidden(div(
            id = ns("panel_a1_results"),

            fluidRow(

              h5(strong("Aboveground biomass maps for the area of interest:")),

              plotOutput(outputId = ns("map_agb"), height = 600),

              hr(),

              h5(strong("Initial Coefficient of variation:")),

              p("Initial CV values for forest inventory optimization following
                Avitabile et al. 2016, Santoro et al. 2018 and a mixed approach
                (average CV, highest area)."),

              ## !!! TO BE IMPROVED !!!
              tableOutput(outputId = ns("CV_table")),

              hr(),

              h5(strong("Area of AOI from uplodaed shapefile")),

              ## !!! TO BE IMPROVED !!!
              textOutput(outputId = ns("area_aoi"))

            ) ## END fluidRow

          )), ## END results div

          width = 8) ## End mainPanel

      ) ## End sidebarLayout

    )), ## End approach 1



    ##
    ## Approach 2: CV Params ################################################
    ##

    shinyjs::hidden(div(
      id = ns("layout_a2"),

      h4(strong("Input parameters for approach 2:")),

      h4("$$CV_{opti} = \\beta_{0} \\times n^{\\beta_{1}} \\times d^{\\beta_{2}} \\times a^{\\beta_{3}}$$"),

      p("With: $n$ the number of subplots, $d$ the distance between subplots,
        $a$ the subplot size in ha and $\\beta_{0}$, $\\beta_{1}$, $\\beta_{2}$,
        $\\beta_{3}$ the model's parameters."),

      sidebarLayout(



        ## + User inputs ====================================================

        sidebarPanel(

          h4("Fill in the model parameters:"),

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

          p("!!! Placeholder for basic simulation using CV model parameters !!!"),

          textOutput(outputId = ns("a2_test")),

          width = 4) ## End Mainpanel A2 results

      ) ## END sidebar layout


      # hr(),
      #
      # h5(strong("Additional input parameters:")),
      #
      # numericInput(
      #   inputId = ns("aoi_area_a2"),
      #   label = HTML(paste0("area of interest in km", tags$sup(2))),
      #   value = 0
      #   )

    )), ## End approach 2



    ## Move to next section #################################################

    shinyjs::hidden(div(
      id = ns("box_CV_to_params"),

      h4(icon("arrow-right"),
         "Continue to Step 2:",
         HTML("&nbsp;"),
         actionButton(ns("btn_to_params"), "Optimization parameters")
         )

    ))

  ) ## END tagList

} ## END function CV_model_UI()
