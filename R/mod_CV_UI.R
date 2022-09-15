
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

    submod_CV_intro_UI(ns("cv_intro")),


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
        actionButton(inputId = ns("start_cv"), label = "Continue")
      ),

      class="bg1"), ## END wellPanel approach



    ##
    ## Approach 1: AGB map Sidebar Layout ###################################
    ##

    shinyjs::hidden(div(
      id = ns("layout_a1"),

      submod_CV_a1_UI(ns("cv_a1")),

    )), ## End approach 1



    ##
    ## Approach 2: CV Params ################################################
    ##

    shinyjs::hidden(div(
      id = ns("layout_a2"),

      submod_CV_a2_UI(ns("cv_a2")),

    )), ## End approach 2



    ## Move to next section #################################################

    shinyjs::hidden(div(
      id = ns("box_cv_to_time"),

      h4(icon("arrow-right"),
         "Continue to Step 2:",
         HTML("&nbsp;"),
         actionButton(ns("btn_to_time"), "Unit times")
         )

    ))

  ) ## END tagList

} ## END function module UI
