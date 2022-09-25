#' Optimization results module server function
#'
#' @noRd
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
    # output$test_combi <- renderText({ paste0("Number of combinations to test: ", nrow(rv$opti$combi), ".") })


    ##
    ## Make graphs ##########################################################
    ##

    ## + get average params to fix other values =============================

    observe({

      req(rv$opti$results)

      rv$results$mean <- tibble(
        subplot_count       = round(mean(rv$opti$results$subplot_count)),
        distance_multiplier = round(mean(rv$opti$results$distance_multiplier)),
        nest1_radius        = round(mean(rv$opti$results$nest1_radius)),
        nest2_radius        = round(mean(rv$opti$results$nest2_radius))
      ) %>%
        mutate(
          subplot_count = if_else(
            condition = rv$opti$list_params$plot_shape == "L" & .data$subplot_count %% 2 == 0,
            true      = .data$subplot_count + 1,
            false     = .data$subplot_count
            )
        )

      # rv$results$cv_limit <- tibble(
      #   min = min(rv$opti$results$cv),
      #   max = max(rv$opti$results$cv)
      # )

    })



    ## + Update sliderInputs with average from input parameters =============

    observe({
      req(rv$opti$results, rv$results$mean)

      updateSliderInput(
        session = session,
        inputId = "subplot_count",
        value   = rv$results$mean$subplot_count,
        min     = min(rv$opti$results$subplot_count),
        max     = max(rv$opti$results$subplot_count),
        step    = unique(rv$opti$step)
        )

      updateSliderInput(
        session = session,
        inputId = "distance_multiplier",
        value   = rv$results$mean$distance_multiplier,
        min     = min(rv$opti$results$distance_multiplier),
        max     = max(rv$opti$results$distance_multiplier),
        step    = 1
      )

      updateSliderInput(
        session = session,
        inputId = "nest1_radius",
        value   = rv$results$mean$nest1_radius,
        min     = min(rv$opti$results$nest1_radius),
        max     = max(rv$opti$results$nest1_radius),
        step    = 1
      )

      updateSliderInput(
        session = session,
        inputId = "nest2_radius",
        value   = rv$results$mean$nest2_radius,
        min     = min(rv$opti$results$nest2_radius),
        max     = max(rv$opti$results$nest2_radius),
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

      req(rv$opti$results, rv$results$selected)

      if (length(unique(rv$opti$results$subplot_count)) > 1) {

        tt<- rv$opti$results %>%
          dplyr::filter(
            .data$distance_multiplier == rv$results$selected$distance_multiplier,
            .data$nest1_radius        == rv$results$selected$nest1_radius,
            .data$nest2_radius        == rv$results$selected$nest2_radius,
            .data$allowable_error     == "10"
          )


        axis_coeff1 <- (max(tt$cv) - min(tt$cv)) / (max(tt$total_time) - min(tt$total_time))
        axis_coeff2 <- max(tt$cv) - axis_coeff1 * max(tt$total_time)

        ggplot(tt, aes(x = .data$subplot_count)) +
          geom_line(aes(y = .data$cv)) +
          geom_line(aes(y = .data$total_time * axis_coeff1 + axis_coeff2), linetype = "dashed") +
          scale_y_continuous(
            name = "----- CV (%)",
            #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
            sec.axis = sec_axis(~ (. - axis_coeff2) / axis_coeff1, name = "- - - Time (months)")
          ) +
          labs(x = "Number of subplots") +
          theme_bw()

      }

    })

    ## + + CV and time for nest radius level 1 ------------------------------
    output$gr_nest1_radius <- renderPlot({

      req(rv$opti$results, rv$results$selected)

      if (length(unique(rv$opti$results$nest1_radius)) > 1) {

        tt <- rv$opti$results %>%
          dplyr::filter(
            .data$subplot_count       == rv$results$selected$subplot_count,
            .data$distance_multiplier == rv$results$selected$distance_multiplier,
            .data$nest2_radius        == rv$results$selected$nest2_radius,
            .data$allowable_error     == "10"
          )

        axis_coeff1 <- (max(tt$cv) - min(tt$cv)) / (max(tt$total_time) - min(tt$total_time))
        axis_coeff2 <- max(tt$cv) - axis_coeff1 * max(tt$total_time)

        ggplot(tt, aes(x = .data$nest1_radius)) +
          geom_line(aes(y = .data$cv)) +
          geom_line(aes(y = .data$total_time * axis_coeff1 +  axis_coeff2), linetype = "dashed") +
          scale_y_continuous(
            name = "----- CV (%)",
            sec.axis = sec_axis(~ (. - axis_coeff2) / axis_coeff1, name = "- - - Time (months)")
          ) +
          labs(x = "Radius of suplot for large trees (m)") +
          theme_bw()

      }

    })

    ## + + CV and time for subplot distance ---------------------------------
    output$gr_subplot_distance <- renderPlot({

      req(rv$cv_model$cv_approach, rv$opti$results, rv$results$selected)

      if (rv$cv_model$cv_approach == "a2" &
          length(unique(rv$opti$results$distance_multiplier)) > 1) {

        tt <- rv$opti$results %>%
          dplyr::filter(
            .data$subplot_count   == rv$results$selected$subplot_count,
            .data$nest1_radius    == rv$results$selected$nest1_radius,
            .data$nest2_radius    == rv$results$selected$nest2_radius,
            .data$allowable_error == "10"
          )

        axis_coeff1 <- (max(tt$cv) - min(tt$cv)) / (max(tt$total_time) - min(tt$total_time))
        axis_coeff2 <- max(tt$cv) - axis_coeff1 * max(tt$total_time)

        ggplot(tt, aes(x = .data$distance_multiplier * .data$nest1_radius)) +
          geom_line(aes(y = .data$cv)) +
          geom_line(aes(y = .data$total_time * axis_coeff1 + axis_coeff2), linetype = "dashed") +
          scale_y_continuous(
            name = "----- CV (%)",
            sec.axis = sec_axis(~(. - axis_coeff2) / axis_coeff1, name = "- - - Time (months)")
          ) +
          labs(x = "Distance between subplot centers (m)") +
          theme_bw()

      }

    })

    ## + + CV and time for nest radius level 2 ------------------------------
    output$gr_nest2_radius <- renderPlot({

      req(rv$cv_model$cv_approach, rv$opti$results, rv$results$selected)

      if (rv$cv_model$cv_approach == "a2" &
          length(unique(rv$opti$results$nest2_radius)) > 1 ) {

        tt <- rv$opti$results %>%
          dplyr::filter(
            .data$subplot_count       == rv$results$selected$subplot_count,
            .data$distance_multiplier == rv$results$selected$distance_multiplier,
            .data$nest1_radius        == rv$results$selected$nest1_radius,
            .data$allowable_error     == "10"
          )

        axis_coeff1 <- (max(tt$cv) - min(tt$cv)) / (max(tt$total_time) - min(tt$total_time))
        axis_coeff2 <- max(tt$cv) - axis_coeff1 * max(tt$total_time)

        ggplot(tt, aes(x = .data$nest2_radius)) +
          geom_line(aes(y = .data$cv)) +
          geom_line(aes(y = .data$total_time * axis_coeff1 + axis_coeff2), linetype = "dashed") +
          scale_y_continuous(
            name = "----- CV (%)",
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

      rv$opti$results %>%
        dplyr::filter(
          .data$subplot_count       == rv$results$selected$subplot_count,
          .data$distance_multiplier == rv$results$selected$distance_multiplier,
          .data$nest1_radius        == rv$results$selected$nest1_radius,
          .data$nest2_radius        == rv$results$selected$nest2_radius,
          .data$allowable_error     == "10"
        ) %>%
        dplyr::select(.data$id, .data$total_time, .data$n_plot, .data$time_plot,
                      .data$time_travel, .data$time_auth, .data$time_measure,
                      .data$time_walk)

    })



    ##
    ## Best models #############################################################
    ##

    output$table_opti <- renderTable({


      best_cv <- rv$opti$results %>%
        dplyr::arrange(.data$cv) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::mutate(category = "Lowest CV")

      best_time <- rv$opti$results %>%
        dplyr::arrange(.data$total_time) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::mutate(category = "Lowest time")

      med_cv   <- median(rv$opti$results$cv)
      med_time <- median(rv$opti$results$total_time)

      mid_cv <- rv$opti$results %>%
        dplyr::mutate(dev_cv = abs((.data$cv - med_cv) / med_cv)) %>%
        dplyr::arrange(.data$dev_cv) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::mutate(category = "median CV")

      mid_time <- rv$opti$results %>%
        dplyr::mutate(dev_time = abs((.data$total_time - med_time) / med_time)) %>%
        dplyr::arrange(.data$dev_time) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::mutate(category = "Median time")

      best_cv %>%
        dplyr::bind_rows(best_time) %>%
        dplyr::bind_rows(mid_cv) %>%
        dplyr::bind_rows(mid_time) %>%
        dplyr::select(-.data$dev_cv, -.data$dev_time) %>%
        dplyr::select(.data$category, tidyselect::everything())

    })

  }) ## END module server function

}
