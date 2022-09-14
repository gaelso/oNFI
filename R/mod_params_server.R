

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

      req(input$subplot_count, input$distance_multiplier,
          input$nest1_radius, input$nest2_radius,
          input$plot_shape, input$allowable_error)

      rv$params$list_params <- list(
        subplot_count       = input$subplot_count[1]:input$subplot_count[2],
        distance_multiplier = input$distance_multiplier[1]:input$distance_multiplier[2],
        nest1_radius        = input$nest1_radius[1]:input$nest1_radius[2],
        nest2_radius        = input$nest2_radius[1]:input$nest2_radius[2],
        plot_shape          = input$plot_shape,
        allowable_error     = input$allowable_error
        )

    })

    ## Adapt number of subplots to plot shape
    observe({

      req(rv$params$list_params)

      if (rv$params$list_params$plot_shape == "L") {
        ## For L shape only odd numbers allowed
        rv$params$list_params$subplot_count <- rv$params$list_params$subplot_count[rv$params$list_params$subplot_count %% 2 == 1]
      }

    })



    ##
    ## disable calculations if incompatible params ##########################
    ##

    observe({

      req(rv$params$list_params)

      rv$params$check_shape_L <- rv$params$list_params$plot_shape == "L" &
        length(rv$params$list_params$subplot_count[rv$params$list_params$subplot_count %% 2 == 1]) == 0

      if (rv$params$check_shape_L) {
          shinyjs::disable(id = "calc_opti")
          shinyjs::show(id = "msg_shape_L")
      } else {
        shinyjs::enable(id = "calc_opti")
        shinyjs::hide(id = "msg_shape_L")
      }

    })



    ##
    ## Display params #######################################################
    ##

    output$out_subplot_count <- renderPrint(rv$params$list_params$subplot_count)

    output$out_distance_multiplier <- renderPrint(rv$params$list_params$distance_multiplier)

    output$out_nest1_radius <- renderPrint(rv$params$list_params$nest1_radius)

    output$out_nest2_radius <- renderPrint(rv$params$list_params$nest2_radius)

    output$out_plot_shape <- renderPrint(rv$params$list_params$plot_shape)

    output$out_allowable_error <- renderPrint(rv$params$list_params$allowable_error)



    ##
    ## Run opti function ####################################################
    ##

    observeEvent(input$calc_opti, {

      req(rv$params$list_params)



      ## + Reset outputs and results ========================================

      shinyjs::show("opti_progress")
      shinyjs::hide("box_to_results")

      updateProgressBar(session = session, id = "prog_opti", value = 0, status = "primary")

      rv$params$combi   <- NULL
      rv$params$results <- NULL



      ## + Make combinations ================================================

      rv$params$combi <- expand.grid(rv$params$list_params) %>%
        as_tibble(.) %>%
        mutate(across(where(is.double), as.integer)) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(id = 1:nrow(.)) %>%
        select(id, everything()) %>%
        mutate(
          subplot_area     = round(pi * nest1_radius^2 /100^2, 3),
          subplot_distance = distance_multiplier * nest1_radius,
          subplot_avg_distance = case_when(
            subplot_count == 1 ~ subplot_distance,
            plot_shape == "L"  ~ as.integer(subplot_distance * (subplot_count - 1) * 2 / subplot_count),
            TRUE ~ NA_integer_
          )
        )




    })



    ##
    ## Progress outputs #####################################################
    ##

    observe({

      req(rv$params$combi)

      output$nb_combi <- renderPrint({ nrow(rv$params$combi) })

    })


  }) ## END module server function

}
