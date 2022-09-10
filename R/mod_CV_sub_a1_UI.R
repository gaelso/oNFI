

mod_CV_sub_a1_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

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

  )
}
