


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


      axis_coeff1 <- (max(tt$cv) - min(tt$cv)) / (max(tt$total_time) - min(tt$total_time))
      axis_coeff2 <- max(tt$cv) - axis_coeff1 * max(tt$total_time)

      ggplot(tt, aes(x = subplot_count)) +
        geom_line(aes(y = cv)) +
        geom_line(aes(y = total_time * axis_coeff1 + axis_coeff2), linetype = "dashed") +
        scale_y_continuous(
          name = "----- CV (%)",
          #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
          sec.axis = sec_axis(~ (. - axis_coeff2) / axis_coeff1, name = "- - - Time (months)")
          ) +
        labs(x = "Number of subplots") +
        theme_bw()

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

      axis_coeff1 <- (max(tt$cv) - min(tt$cv)) / (max(tt$total_time) - min(tt$total_time))
      axis_coeff2 <- max(tt$cv) - axis_coeff1 * max(tt$total_time)

      ggplot(tt, aes(x = nest1_radius)) +
        geom_line(aes(y = cv)) +
        geom_line(aes(y = total_time * axis_coeff1 +  axis_coeff2), linetype = "dashed") +
        scale_y_continuous(
          name = "----- CV (%)",
          #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
          sec.axis = sec_axis(~ (. - axis_coeff2) / axis_coeff1, name = "- - - Time (months)")
        ) +
        labs(x = "Radius of suplot for large trees (m)") +
        theme_bw()

    })

    ## + + CV and time for subplot distance ---------------------------------
    output$gr_subplot_distance <- renderPlot({

      req(rv$cv_model$cv_approach)

      if (rv$cv_model$cv_approach == "a2") {

        req(rv$params$results, rv$results$selected)

        tt <- rv$params$results %>%
          dplyr::filter(
            subplot_count   == rv$results$selected$subplot_count,
            nest1_radius    == rv$results$selected$nest1_radius,
            nest2_radius    == rv$results$selected$nest2_radius,
            allowable_error == "10"
          )

        axis_coeff1 <- (max(tt$cv) - min(tt$cv)) / (max(tt$total_time) - min(tt$total_time))
        axis_coeff2 <- max(tt$cv) - axis_coeff1 * max(tt$total_time)

        ggplot(tt, aes(x = distance_multiplier * nest1_radius)) +
          geom_line(aes(y = cv)) +
          geom_line(aes(y = total_time * axis_coeff1 + axis_coeff2), linetype = "dashed") +
          scale_y_continuous(
            name = "----- CV (%)",
            #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
            sec.axis = sec_axis(~(. - axis_coeff2) / axis_coeff1, name = "- - - Time (months)")
          ) +
          labs(x = "Distance between subplot centers (m)") +
          theme_bw()

      }

    })

    ## + + CV and time for nest radius level 2 ------------------------------
    output$gr_nest2_radius <- renderPlot({

      req(rv$cv_model$cv_approach)

      if (rv$cv_model$cv_approach == "a2") {

        req(rv$params$results, rv$results$selected)

        tt <- rv$params$results %>%
          dplyr::filter(
            subplot_count       == rv$results$selected$subplot_count,
            distance_multiplier == rv$results$selected$distance_multiplier,
            nest1_radius        == rv$results$selected$nest1_radius,
            allowable_error     == "10"
          )

        axis_coeff1 <- (max(tt$cv) - min(tt$cv)) / (max(tt$total_time) - min(tt$total_time))
        axis_coeff2 <- max(tt$cv) - axis_coeff1 * max(tt$total_time)

        ggplot(tt, aes(x = nest2_radius)) +
          geom_line(aes(y = cv)) +
          geom_line(aes(y = total_time * axis_coeff1 + axis_coeff2), linetype = "dashed") +
          scale_y_continuous(
            name = "----- CV (%)",
            #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
            sec.axis = sec_axis(~(. - axis_coeff2) / axis_coeff1, name = "- - - Time (months)")
          ) +
          labs(x = "Radius of suplot for small trees (m)") +
          theme_bw()

      }

    })


    ##
    ##  Time for the selected params ###########################################
    ##

    output$table_time <- renderTable({

      rv$params$results %>%
        dplyr::filter(
          subplot_count       == rv$results$selected$subplot_count,
          distance_multiplier == rv$results$selected$distance_multiplier,
          nest1_radius        == rv$results$selected$nest1_radius,
          nest2_radius        == rv$results$selected$nest2_radius,
          allowable_error     == "10"
        ) %>%
        dplyr::select(id, total_time, n_plot, time_plot, time_travel, time_auth, time_measure, time_walk)

    })



    ##
    ## Best models #############################################################
    ##

    output$table_opti <- renderTable({


      best_cv <- rv$params$results %>%
        arrange(cv) %>%
        slice_head(n = 5)

      best_time <- rv$params$results %>%
        arrange(total_time) %>%
        slice_head(n = 5)

      med_cv   <- median(rv$params$results$cv)
      med_time <- median(rv$params$results$total_time)

      mid_cv <- rv$params$results %>%
        mutate(dev_cv = abs((cv - med_cv) / med_cv)) %>%
        arrange(med_cv) %>%
        slice_head(n = 5)

      mid_time <- rv$params$results %>%
        mutate(dev_time = abs((total_time - med_time) / med_time)) %>%
        arrange(med_time) %>%
        slice_head(n = 5)

      best_cv %>%
        bind_rows(best_time) %>%
        bind_rows(mid_cv) %>%
        bind_rows(mid_time)

    })




  }) ## END module server function

}
