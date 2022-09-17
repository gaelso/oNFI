


mod_results_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns



    # ##
    # ## checks from other modules ############################################
    # ##
    #
    # output$test_approach <- renderText({
    #
    #   if (is.null(rv$cv_model$cv_approach)) {
    #     "Approach selected: None."
    #   } else {
    #     paste0("Approach selected: ", rv$cv_model$cv_approach, ".")
    #   }
    #
    # })
    #
    # output$test_cv <- renderTable({ rv$cv_model$cv_mixed })
    #
    # output$test_nested <- renderText({ paste0("Nested plot input: ", rv$time$check_nested_default) })
    #
    # output$test_time <- renderText({ paste0("Unit time input: ", rv$time$check_time_default) })
    #
    # output$test_combi <- renderText({ paste0("Number of combinations to test: ", nrow(rv$params$combi), ".") })


    ##
    ## Make graphs ##########################################################
    ##

    ## + get average params to fix other values =============================

    observe({

      req(rv$params$results)

      rv$results$mean <- tibble(
        subplot_count       = round(mean(rv$params$results$subplot_count)),
        distance_multiplier = round(mean(rv$params$results$distance_multiplier)),
        nest1_radius        = round(mean(rv$params$results$nest1_radius)),
        nest2_radius        = round(mean(rv$params$results$nest2_radius))
      ) %>%
        mutate(
          subplot_count = if_else(
            condition = rv$params$list_params$plot_shape == "L" & subplot_count %% 2 == 0,
            true      = subplot_count + 1,
            false     = subplot_count
            )
        )

      # rv$results$cv_limit <- tibble(
      #   min = min(rv$params$results$cv),
      #   max = max(rv$params$results$cv)
      # )

    })



    ## + Update sliderInputs with average from input parameters =============

    observe({
      req(rv$params$results, rv$results$mean)

      updateSliderInput(
        session = session,
        inputId = "subplot_count",
        value   = rv$results$mean$subplot_count,
        min     = min(rv$params$results$subplot_count),
        max     = max(rv$params$results$subplot_count),
        step    = unique(rv$params$step)
        )

      updateSliderInput(
        session = session,
        inputId = "distance_multiplier",
        value   = rv$results$mean$distance_multiplier,
        min     = min(rv$params$results$distance_multiplier),
        max     = max(rv$params$results$distance_multiplier),
        step    = 1
      )

      updateSliderInput(
        session = session,
        inputId = "nest1_radius",
        value   = rv$results$mean$nest1_radius,
        min     = min(rv$params$results$nest1_radius),
        max     = max(rv$params$results$nest1_radius),
        step    = 1
      )

      updateSliderInput(
        session = session,
        inputId = "nest2_radius",
        value   = rv$results$mean$nest2_radius,
        min     = min(rv$params$results$nest2_radius),
        max     = max(rv$params$results$nest2_radius),
        step    = 1
      )

    })



    ## + Pass selected to rv ================================================

    observe({

      rv$results$selected <- tibble(
        subplot_count       = input$subplot_count,
        distance_multiplier = input$distance_multiplier,
        nest1_radius        = input$nest1_radius,
        nest2_radius        = input$nest2_radius
      )

    })



    ## + Make graphs ========================================================

    ## + + CV and time for Subplot count ------------------------------------
    output$gr_subplot_count <- renderPlot({

      req(rv$params$results, rv$results$selected)

      tt<- rv$params$results %>%
        dplyr::filter(
          distance_multiplier == rv$results$selected$distance_multiplier,
          nest1_radius        == rv$results$selected$nest1_radius,
          nest2_radius        == rv$results$selected$nest2_radius,
          allowable_error     == "10"
          )

      axis_coeff <- max(tt$cv) / max(tt$total_time)
      #axis_coeff <- 3

      ggplot(tt, aes(x = subplot_count)) +
        geom_line(aes(y = cv)) +
        geom_line(aes(y = total_time * axis_coeff), linetype = "dashed") +
        scale_y_continuous(
          name = "----- CV (%)",
          #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
          sec.axis = sec_axis(~./axis_coeff, name = "- - - Time (months)")
          ) +
        labs(x = "Number of subplots")

    })

    ## + + CV and time for subplot distance ---------------------------------
    output$gr_subplot_distance <- renderPlot({

      req(rv$params$results, rv$results$selected)

      tt <- rv$params$results %>%
        dplyr::filter(
          subplot_count   == rv$results$selected$subplot_count,
          nest1_radius    == rv$results$selected$nest1_radius,
          nest2_radius    == rv$results$selected$nest2_radius,
          allowable_error == "10"
        )

      axis_coeff <- max(tt$cv) / max(tt$total_time)

      ggplot(tt, aes(x = distance_multiplier * nest1_radius)) +
        geom_line(aes(y = cv)) +
        geom_line(aes(y = total_time * axis_coeff), linetype = "dashed") +
        scale_y_continuous(
          name = "----- CV (%)",
          #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
          sec.axis = sec_axis(~./axis_coeff, name = "- - - Time (months)")
        ) +
        labs(x = "Distance between subplot centers (m)")
        # labs(
        #   x = "Distance between subplot centers (m)",
        #   caption = paste0(
        #     "Number of subplots selected: ", rv$results$selected$subplot_count, ".\n",
        #     "Level 1 subplot radius selected: ", rv$results$selected$nest1_radius, " m.\n",
        #     "Level 2 subplot radius selected: ", rv$results$selected$nest2_radius, " m."
        #   )
        # )

    })

    ## + + CV and time for nest radius level 1 ------------------------------
    output$gr_nest1_radius <- renderPlot({

      req(rv$params$results, rv$results$selected)

      tt <- rv$params$results %>%
        dplyr::filter(
          subplot_count       == rv$results$selected$subplot_count,
          distance_multiplier == rv$results$selected$distance_multiplier,
          nest2_radius        == rv$results$selected$nest2_radius,
          allowable_error     == "10"
        )

      axis_coeff <- max(tt$cv) / max(tt$total_time)

      ggplot(tt, aes(x = nest1_radius)) +
        geom_line(aes(y = cv)) +
        geom_line(aes(y = total_time * axis_coeff), linetype = "dashed") +
        scale_y_continuous(
          name = "----- CV (%)",
          #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
          sec.axis = sec_axis(~./axis_coeff, name = "- - - Time (months)")
        ) +
        labs(x = "Radius of suplot for large trees (m)")

    })

    ## + + CV and time for nest radius level 1 ------------------------------
    output$gr_nest2_radius <- renderPlot({

      req(rv$params$results, rv$results$selected)

      tt <- rv$params$results %>%
        dplyr::filter(
          subplot_count       == rv$results$selected$subplot_count,
          distance_multiplier == rv$results$selected$distance_multiplier,
          nest1_radius        == rv$results$selected$nest1_radius,
          allowable_error     == "10"
        )

      axis_coeff <- max(tt$cv) / max(tt$total_time)

      ggplot(tt, aes(x = nest2_radius)) +
        geom_line(aes(y = cv)) +
        geom_line(aes(y = total_time * axis_coeff), linetype = "dashed") +
        scale_y_continuous(
          name = "----- CV (%)",
          #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
          sec.axis = sec_axis(~./axis_coeff, name = "- - - Time (months)")
        ) +
        labs(x = "Radius of suplot for small trees (m)")

    })




  }) ## END module server function

}
