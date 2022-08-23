
AGB_map_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(

    ## Sidebar Layout #######################################################
    sidebarLayout(

      ## + Define sidebarPanel ----------------------------------------------
      sidebarPanel(

        ## + + Step1: Set a path to data ----
        div(
          p("In this section we will need to download two biomass maps, respectively from",
            a("Avitabile et al. 2016, An integrated pan-tropical biomass map using
              multiple reference datasets",
              href = "https://doi.org/10.1111/gcb.13139"),
            "and ",
            a("Santoro et al. 2018, A detailed portrait of the forest aboveground
              biomass pool for the year 2010 obtained from multiple remote sensing observations",
              href = "https://globbiomass.org/wp-content/uploads/GB_Maps/Globbiomass_global_dataset.html"
              )),
          p("Before continuing, if you want to keep these data, please select a destination path.
            If you don't the data will be downloaded in a temporary folder and will need to be downloaded
            again every time you restart R and the application"),

          shinyDirButton(id = ns('folder'), label = 'Select a folder', title = 'Please select a folder', FALSE),

          div("Folder selected",
              verbatimTextOutput(ns("folder_path")),
              verbatimTextOutput(ns('rawInputValue')),
              style = "padding: 0.375em; border: 1px solid #e3e3e3;
                  border-radius: 4px; font-weight: bold; text-align: center;
                  margin-top: 5px;"),

          actionButton(inputId = ns("to_step2"), label = "Continue")
        ),

        br(),

        hr(),

        ## + + Step 2: Get Area of Interest ----
        conditionalPanel(
          condition = ns("to_step2"),

          p("Select country from list or upload AOI shapefile."),

          p("If you pick from the list below, the approximative country boundaries
            are taken from", a("GADM", href = "https://gadm.org/data.html")),

          selectInput(ns("input_country"), "Select country:", choices = c(NA, "Timor Leste"), selected = NA),

          br(),

          p("Alternatively you can upload a shapefile with the boundaries of your
            are of interest. Accepted file types are shapefiles '.shp' loaded as
            zipfile ('.zip'), geoJSON files ('.geoJSON') and GeoPackage files ('.GPKG').
            Since a simple AOI boundaires is expected the maximum file size allowed is 50 Mo."),

          fileInput(ns("input_shp"), "Upload an AOI shapefile", multiple = F, accept = c(".zip", ".geoJSON", ".GPKG")),

        ),

        width = 4

      ),

      ## + Define mainPanel -------------------------------------------------

      mainPanel(
          h4(strong("Clip existing biomass maps with your area of interest")),

          br(),

          fluidRow(

            p("!!! placeholder for leaflet to display AGB map and AOI boundaries")

          ),

          width = 8

        ) ## End mainPanel

    ) ## End sidebarLayout

  ) ## END tagList

} ## END function AGB_map_UI()
