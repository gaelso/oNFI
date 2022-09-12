

mod_CV_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv_cv <- reactiveValues()



    ##
    ## Introduction #########################################################
    ##

    mod_CV_sub_intro_server("cv_intro", rv = rv)



    ##
    ## Approach selection ###################################################
    ##

    ## Reset/initiate approach based panels
    observeEvent(input$start_cv, {

      rv$cv_model$cv_approach <- input$approach
      rv$cv_model$start_cv <- input$start_cv



      ## + Re-initiate panels and steps =====================================

      if(rv$cv_model$cv_approach == "a1") {

        ## Re-initiate panels
        shinyjs::reset("layout_a1")
        shinyjs::show("layout_a1")
        shinyjs::hide("layout_a2")
        shinyjs::hide("box_cv_to_params")

      } else if (rv$cv_model$cv_approach == "a2") {

        ## Re-initiate panels
        shinyjs::reset("layout_a2")
        shinyjs::show("layout_a2")
        shinyjs::hide("layout_a1")
        shinyjs::show("box_cv_to_params")

      }

      ## + Reset local reactive values ======================================

      rv_cv$file_path    <- NULL
      rv_cv$sf_aoi       <- NULL
      rv_cv$rs_avitabile <- NULL
      rv_cv$df_avitabile <- NULL
      rv_cv$cv_avitabile <- NULL
      rv_cv$rs_santoro   <- NULL
      rv_cv$df_santoro   <- NULL
      rv_cv$cv_santoro   <- NULL
      rv_cv$cv_mixed     <- NULL

    })



    ##
    ## Panel A1 server ######################################################
    ##

    mod_CV_sub_a1_server("cv_a1", rv = rv, rv_cv = rv_cv)



    ##
    ## Panel A2 server ######################################################
    ##

    mod_CV_sub_a2_server("cv_a2", rv = rv)



    ##
    ## Change tab ###########################################################
    ##

    observe({
      req(rv$cv_model$cv_mixed)
      shinyjs::show("box_cv_to_params")
    })

    observeEvent(input$btn_to_params, {
      rv$to_params <- input$btn_to_params
    })

  }) ## END module server function

}
