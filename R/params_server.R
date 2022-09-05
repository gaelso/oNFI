

params_server <- function(id, rv) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      ns <- session$ns

      ## !!! For testing only
      output$test_approach <- renderText({ paste0("Approach selected: ", rv$CV_model$cv_approach) })

      output$test_CV <- renderTable({ rv$CV_model$cv_mixed })



      ## Check if CV model approach selected ################################
      observe({

        req(rv$CV_model$cv_approach)

        if (rv$CV_model$cv_approach == "a2") {
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

        req(rv$CV_model$cv_approach)
        req(rv$CV_model$cv_mixed)

        if (rv$CV_model$cv_approach == "a1") {
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


    } ## END module server function
  )
} ## END function params_server()
