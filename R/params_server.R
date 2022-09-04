

params_server <- function(id, rv) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      ## Check if CV model approach selected ################################
      observe({

        if (is.null(rv$CV_model$cv_approach) | is.null(rv$CV_model$cv_mixed)) {
          shinyjs::show("check_approach")
          shinyjs::hide("params_setup")
        } else {
          shinyjs::hide("check_approach")
          shinyjs::show("params_setup")
        }

      })

      ## box 1: CV model ####################################################


    } ## END module function
  )
} ## END function params_server()
