#' sub-module UI function to Optimize the NFI design
#'
#' @description Shiny sub-module to calculate the number of plots, CV and time cost for all
#'              combinations of input parameters.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
submod_opti_calc_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ## UI elements wrapped in a tagList() function
  tagList(


    ##
    ## Start calculations ####################################################
    ##

    wellPanel(

      actionButton(inputId = ns("start_calc"), label = "Launch optimization script"),

      align = "center"

    ),


    ##
    ## Show progress of the optimization script ###############################3
    ##

    shinyjs::hidden(div(
      id = ns("opti_progress"),

      p(strong("Optimization script progress")),

      shinyWidgets::progressBar(
        id = ns("prog_opti"),
        value = 0,
        total = 10,
        title = "Optimization process",
        display_pct = TRUE
        ),

      )),


    ##
    ## Show preliminary results ##############################################
    ##

    shinyjs::hidden(div(
      id = ns("summary_results"),

      div(
        plotOutput(outputId = ns("gr_cv_cost"), height = 400),
        style = "padding: 0.375em; border: 1px solid #e3e3e3;
              border-radius: 4px; width: 600px;
              margin: 0px auto;"
      ),

      br(),

      downloadButton(outputId = ns("download_results"), label = 'Download the results')

    ))

  ) ## END tagList

}

