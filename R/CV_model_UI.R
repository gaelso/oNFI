
CV_model_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    h3("Coefficient of Variation for your main forest inventory variable", alignn = "center"),

    br(),



    ## Introduction #########################################################
    h4("Introduction"),

    ## Toggle on/off the introduction
    checkboxInput(inputId = ns("toggle_intro"), label = "Show the introduction", value = FALSE),

    fluidRow(

      wellPanel(id = ns("intro1"), div(

        p("Presentation formula from Stein method 1945: n = (CV1_hat x t(1- alpha/2, n1 - 1) / E)^2"),
        p("This section focus on CV and it's variability for different plot design."),
        p("two approaches are proposed here:"),
        HTML("<ol>
          <li>CV variation with plot size, CV obtain from Biomass maps.</li>
          <li>CV as a function of plot size and distance from subplot from user inputs
          (Values can be modeled from large research forest plots for example).</li>
          </ol>")
      ), class = "bg_explanations"),

      column(6, wellPanel(id = ns("intro2"), div(
        p("In the absence of a CV model, approach 1 requires selecting a country or area of interest.
          Two biomass maps are then used to get a CV and a plot area (in this case pixel size): ",
          a("Avitabile et al. 2016, An integrated pan-tropical biomass map using
            multiple reference datasets", href = "https://doi.org/10.1111/gcb.13139"), "and ",
          a("Santoro et al. 2018, A detailed portrait of the forest aboveground
            biomass pool for the year 2010 obtained from multiple remote sensing observations",
            href = "https://globbiomass.org/wp-content/uploads/GB_Maps/Globbiomass_global_dataset.html")),
        p("the forest inventory CV is then adapted to different plot size with the formula:"),
        p("CV2 = sqrt(CV^2 * sqrt(plot_area1/plot_area2)) ",
          a("lynch 2017", href = "https://doi.org/10.1093/forestry/cpw038")),
        p("This method takes longer time as it requires spatial analysis to get an initial CV for biomass.
          On the other hand it doesn't any prior knowledge on the taget forests.")
      ), class = "bg_explanations")),

      column(6, wellPanel(id = ns("intro3"), div(
        p("If you have a CV model, instead any main variable can be selected and the application
          only needs the model parameters beta0, beta1, beta2 and beta3 for the following model:"),
        p("CV = beta0 * subplot_count^beta1 * subplot_distance^beta2 * subplot_area^beta3",
          a("(Scott 1993)", href = "https://www.nrs.fs.fed.us/pubs/jrnl/1993/ne_1993_scott-c_001.pdf"))
      ), class="bg_explanations")),

    ), ## END fluidRow()



    ## Select approach ######################################################
    h4("Select an approach"),

    fluidRow(

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
        class="bg1"),

    ), ## END fluidRow


    ## Approach 1: AGB map Sidebar Layout ###################################
    shinyjs::hidden(div(id = ns("AGB_map"), sidebarLayout(

      ## + Define sidebarPanel ==============================================
      sidebarPanel(

        ## + + Step 1: Set a path to data -----------------------------------
        div(
          h4("Select a folder"),
          p("Select a destination folder for spatial data. If you don't the data
          will be downloaded in a temporary folder."),

          shinyFiles::shinyDirButton(id = ns('folder'), label = 'Select a folder', title = 'Please select a folder', FALSE),

          div(
            p("Path:"),
            textOutput(ns("show_path")),
            style = "padding: 0.375em; border: 1px solid #e3e3e3;
            border-radius: 4px; font-weight: bold; text-align: center;
            margin-top: 5px; margin-bottom: 5px;"
          ),

          ## !!! For testing
          # textOutput(ns("show_path2")),

          actionButton(inputId = ns("to_step2"), label = "Continue")

        ),

        hr(),

        ## + + Step 2: Get Area of Interest ---------------------------------
        conditionalPanel(
          condition = "input.to_step2", ns = ns,

          h4("Select an Area of Interest (AOI)"),

          p("Accepted file types are geoJSON ('.geoJSON') and GeoPackage ('.GPKG').
            The maximum file size allowed is 10 Mo."),

          fileInput(ns("AOI"), "Upload an AOI shapefile:",
                    multiple = F, accept = c(".geoJSON", ".GPKG")),

          div(
            plotOutput(outputId = ns("map_aoi"), height = 100),
            #align = "center",
            style = "padding: 0.375em; border: 1px solid #e3e3e3;
            border-radius: 4px; font-weight: bold; text-align: center;
            margin-top: 5px; margin-bottom: 5px; align: center;"
          ),

          ## Next step button
          shinyjs::disabled(actionButton(inputId = ns("to_step3"), label = "Continue")),
          p(id = ns("msg_to_step3"),
            "A valid spatial file is required to move to the next step",
            style = "color: red; font-style: italic;"
          )

        ), ## END conditionalPanel

        hr(),

        ## + + Step 3: Default values ---------------------------------------
        conditionalPanel(
          condition = "input.to_step3", ns = ns,

          ## !!! TO BE MOVED TO POPUP EXPLANATION
          # p("The maps provide AGB estimates for all land use types, with values
          #   ranging from 0 to around 700 ton/ha. As forest lands are expected to
          #   have a minimum AGB, this value can be selected below to reduce the CV
          #   estimate from the maps. Default to 0, meaning all land is considered
          #   in the calcuation of CV"),

          numericInput(ns("agb_min"), "Pick a minimum AGB value for forest (in ton/ha)", value = 0, min = 0, max = 100),

          hr(),

          actionButton(inputId = ns("to_step4"), label = "Continue"),

        ), ## END conditionalPanel

        hr(),

        ## + + Step 4: Launch calculations ----------------------------------
        conditionalPanel(
          condition = "input.to_step4", ns = ns,

          actionButton(inputId = ns("calc_CV"), label = "Calculate CV"),

        ), ## END conditionalPanel

        ## Width sidebar
        width = 4

      ),

      ## + Define mainPanel =================================================
      mainPanel(
        h4(strong("Area of Interest and Coefficient of Variation")),

        br(),

        ## + + Progress bars ------------------------------------------------
        shinyjs::hidden(div(
          id = ns("CV_progress"),

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

        )), ## END progress

        ## + + Show results -------------------------------------------------
        shinyjs::hidden(div(
          id = ns("CV_to_results"),

          actionButton(inputId = ns("show_CV"), label = "Show results")

        )), ## END to results


        ## + + Maps and CV tables -------------------------------------------
        shinyjs::hidden(div(
          id = ns("CV_results"),

          fluidRow(

            p("Aboveground biomass maps for the area of interest:"),

            plotOutput(outputId = ns("map_agb"), height = 600),

            hr(),

            p("Initial CV values for forest inventory optimization following Avitabile
            et al. 2016, Santoro et al. 2018 and a mixed approach (average CV, highest area):"),

            ## !!! TO BE IMPROVED !!!
            tableOutput(outputId = ns("CV_table")),

            br(),

            p("Based on the input area of interest spatial data and the specified forest
            cover percentage the AOI area and forest area are (in km^2):"),

            ## !!! TO BE IMPROVED !!!
            tableOutput(outputId = ns("area_table")),

          ) ## END fluidRow

        )), ## END results div

        shinyjs::hidden(div(
          id = ns("CV_to_params"),

        fluidRow(
          h4(icon("arrow-right"), "Continue to Step 2:", HTML("&nbsp;"),
             actionButton(ns("to_params"), "Optimization parameters")
          )
        ) ## END fluidRow

        )),

        width = 8

      ) ## End mainPanel

    ))), ## End sidebarLayout



    ## Approach 2: CV Params ################################################
    shinyjs::hidden(wellPanel(
      id = ns("CV_params"),

      p("!!! Placeholder for Scott's approach !!!")

    ))


  ) ## END tagList

} ## END function CV_model_UI()
