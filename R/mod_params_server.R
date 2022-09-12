

mod_params_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    ##
    ## Introduction #########################################################
    ##

    ## Show/hide Introduction
    observeEvent(input$toggle_intro, {
      shinyjs::toggle(id = "params_intro", anim = T, animType = "slide")
    })



    ##
    ## checks from other modules ############################################
    ##

    output$test_approach <- renderText({

      if (is.null(rv$cv_model$cv_approach)) {
        "Approach selected: None."
      } else {
        paste0("Approach selected: ", rv$cv_model$cv_approach, ".")
      }

    })

    output$test_cv <- renderTable({ rv$cv_model$cv_mixed })

    output$test_nested <- renderText({ paste0("Nested plot input: ", rv$time$check_nested_default) })

    output$test_time <- renderText({ paste0("Unit time input: ", rv$time$check_time_default) })



    ##
    ## Show/hide inputs #####################################################
    ##

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



    ##
    ## Pass inputs to rv ####################################################
    ##



  }) ## END module server function

}
