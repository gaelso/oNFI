

mod_params_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## !!! For testing only
    output$test_approach <- renderText({ paste0("Approach selected: ", rv$cv_model$cv_approach) })

    output$test_cv <- renderTable({ rv$cv_model$cv_mixed })



    ## Check if CV model approach selected ################################
    observe({

      req(rv$cv_model$cv_approach)

      if (rv$cv_model$cv_approach == "a2") {
        shinyjs::hide("check_approach")
        shinyjs::show("params_setup")
        shinyjs::show("params_a2")
      } else {
        shinyjs::show("check_approach")
        shinyjs::hide("params_setup")
        shinyjs::hide("params_a2")
      }

    })

    observe({

      req(rv$cv_model$cv_approach)
      req(rv$cv_model$cv_mixed)

      if (rv$cv_model$cv_approach == "a1") {
        shinyjs::hide("check_approach")
        shinyjs::show("params_setup")
        shinyjs::show("params_a1")
      } else {
        shinyjs::show("check_approach")
        shinyjs::hide("params_setup")
        shinyjs::hide("params_a2")
      }

    })

    ## box 1: CV model ####################################################


  }) ## END module server function

}
