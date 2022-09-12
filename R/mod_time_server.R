


mod_time_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv_cv <- reactiveValues()



    ##
    ## Introduction #######################################################
    ##

    ## Show/hide Introduction
    observeEvent(input$toggle_intro, {
      shinyjs::toggle(id = "time_intro1", anim = T, animType = "slide")
    })



    ##
    ## Nested plot input ####################################################
    ##



  }) ## END module server function

}
