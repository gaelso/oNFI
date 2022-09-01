
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

      wellPanel(div(
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
          )
      ), class="bg1"),

    ), ## END fluidRow


    ## Approach 1: AGB map Sidebar Layout ###################################
    shinyjs::hidden(div(id = ns("AGB_map"), sidebarLayout(

      ## + Define sidebarPanel ----------------------------------------------
      sidebarPanel(

        ## + + Step1: Set a path to data ----
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

        ## + + Step 2: Get Area of Interest ----
        conditionalPanel(
          condition = "input.to_step2", ns = ns,

          h4("Select an Area of Interest (AOI)"),

          ## !!! Not implemented, get the ata ffrom GADM automatically
          # p("Select a country from the list below or upload an AOI shapefile."),
          #
          # p("If you pick from the list below, the approximative country boundaries
          #   are taken from", a("GADM", href = "https://gadm.org/data.html")),
          #
          # selectInput(ns("input_country"), "Select country:", choices = c("<empty>", "Timor Leste"), selected = "<empty>"),
          #
          # br(),
          # !!!

          p("Please upload the spatial boundaries of your
            AOI. Accepted file types are geoJSON ('.geoJSON') and GeoPackage ('.GPKG').
            The maximum file size allowed is 10 Mo."),

          fileInput(ns("AOI"), "Upload an AOI shapefile:",
                    multiple = F, accept = c(".geoJSON", ".GPKG")),

          actionButton(inputId = ns("to_step3"), label = "Continue"),

          div(
            plotOutput(outputId = ns("map_aoi"), height = 100),
            #align = "center",
            style = "padding: 0.375em; border: 1px solid #e3e3e3;
            border-radius: 4px; font-weight: bold; text-align: center;
            margin-top: 5px; margin-bottom: 5px; align: center;"
          )

        ), ## END conditionalPanel

        hr(),

        ## ++ Step 3: Default values ----
        conditionalPanel(
          condition = "input.to_step3", ns = ns,

          # !!! Force both products for now
          # h4("Select the Biomass map to use for the CV"),
          #
          # checkboxGroupInput(
          #   inputId = ns("map_select"),
          #   label ="Biomass map",
          #   choiceNames = c(
          #     "Avitabile et al. 2016, 1 km res., 2000-2010",
          #     "Santoro et al. 2018, 100m res., 2010"
          #     ),
          #   choiceValues = c("avitabile2016", "santoro2018")
          #   ),

          p("The maps provide AGB estimates for all land use types, with values
            ranging from 0 to around 700 ton/ha. As forest lands are expected to
            have a minimum AGB, this value can be selected below to reduce the CV
            estimate from the maps. Default to 0, meaning all land is considered
            in the calcuation of CV"),

          numericInput(ns("agb_min"), "Pick a minimum AGB value for forest (in ton/ha)", value = 0, min = 0, max = 100),

          br(),

          p("To refine the grid spacing, please provide the estimated forest cover of you AOI in %."),

          numericInput(ns("forest_cover"), "Forest cover in %", value = 50, min = 0, max = 100),

          hr(),

          ## + + Launch calculations
          actionButton(inputId = ns("calc_CV"), label = "Calculate CV"),

        ),

        ## Width sidebar
        width = 4

      ),

      ## + Define mainPanel -------------------------------------------------
      mainPanel(
        h4(strong("Area of Interest and Coefficient of Variation")),

        br(),

        fluidRow(

          p("!!! placeholder for leaflet to display AGB map and AOI boundaries"),

          plotOutput(outputId = ns("map_avitabile"), height = 300),

          hr(),

          plotOutput(outputId = ns("map_santoro"), height = 300),

          hr(),

          tableOutput(outputId = ns("CV_table"))

        ),

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
