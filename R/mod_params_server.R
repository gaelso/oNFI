

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
      } else {
        shinyjs::show("check_approach")
        shinyjs::hide("params_setup")
      }

    })

    observe({

      req(rv$cv_model$cv_approach)
      req(rv$cv_model$cv_mixed)

      if (rv$cv_model$cv_approach == "a1") {
        shinyjs::hide("check_approach")
        shinyjs::show("params_setup")
      } else {
        shinyjs::show("check_approach")
        shinyjs::hide("params_setup")
      }

    })



    ##
    ## Pass inputs to rv ####################################################
    ##


    observe({
      req(input$subplot_count, input$distance_multiplier)

      rv$params$params <- list(
        subplot_count       = seq(input$subplot_count[1], input$subplot_count[2], 2),
        distance_multiplier = input$distance_multiplier[1]:input$distance_multiplier[2]
      )

    })



    ##
    ## tmp checks ###########################################################
    ##

    output$out_subplot_count <- renderPrint(rv$params$params$subplot_count)

    output$out_distance_multiplier <- renderPrint(rv$params$params$distance_multiplier)




  }) ## END module server function

}
