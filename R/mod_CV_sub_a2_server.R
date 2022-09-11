



mod_CV_sub_a2_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ##
    ## Add inputs to rv #####################################################
    ##

    observe({

      req(input$beta0, input$beta1, input$beta2, input$beta3)

      rv$cv_model$beta0 <- input$beta0
      rv$cv_model$beta1 <- input$beta1
      rv$cv_model$beta2 <- input$beta2
      rv$cv_model$beta3 <- input$beta3

    })



    ##
    ## Calc CV for common plot design #######################################
    ##

    test_cv <- reactive({

      req(rv$cv_model$beta0, rv$cv_model$beta1, rv$cv_model$beta2, rv$cv_model$beta3)

      ## Calc CV for common plot design
      round(rv$cv_model$beta0 * 5^rv$cv_model$beta1 * 80^rv$cv_model$beta2 * (pi * (18/100)^2)^rv$cv_model$beta3 * 100)

    })



    ##
    ## Make table ###########################################################
    ##

    output$a2_check <- renderTable({

      req(rv$cv_model$beta0, rv$cv_model$beta1, rv$cv_model$beta2, rv$cv_model$beta3)

      tibble(
        beta0 = if_else(rv$cv_model$beta0 == 1.02 , "Default value", "User defined"),
        beta1 = if_else(rv$cv_model$beta1 == -0.15, "Default value", "User defined"),
        beta2 = if_else(rv$cv_model$beta2 == 0.016, "Default value", "User defined"),
        beta3 = if_else(rv$cv_model$beta3 == -0.12, "Default value", "User defined")
      )

    })


    output$a2_test <- renderText({

      req(test_cv())

     paste0("With common plot design of 5 subplots of 18 m radius, 80 m appart,
            the model gives a CV equal to ", test_cv(), "%.")

    })


  }) ## END module server function

}
