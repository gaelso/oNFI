
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

        div("Select country from list or upload AOI shapefile"),

        selectInput(ns("input_country"), "Select country:", choices = c("Timor Leste")),

        br(),

        fileInput(ns("input_shp"), "Upload an AOI shapefile"),

        width = 3

      ),

      ## + Define mainPanel -------------------------------------------------

      mainPanel(
          h4(strong("Clip existing biomass maps with your area of interest")),

          br(),

          fluidRow(

            p("!!! placeholder for leaflet to display AGB map and AOI boundaries")

          ),

          width = 9

        ) ## End mainPanel

    ) ## End sidebarLayout

  ) ## END tagList

} ## END function AGB_map_UI()
