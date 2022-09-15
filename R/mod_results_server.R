


mod_results_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns



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

    output$test_combi <- renderText({ paste0("Number of combinations to test: ", nrow(rv$params$combi), ".") })


    ##
    ## Make graphs ##########################################################
    ##

    ## + get average params to fix other values =============================

    observe({

      req(rv$params$results)

      rv$results$mean_subplot_count <- round(mean(rv$params$results$subplot_count))

      if (rv$params$list_params$plot_shape == "L" & rv$results$mean_subplot_count %% 2 == 0) {
        rv$results$mean_subplot_count <- rv$results$mean_subplot_count + 1
      }

      rv$results$mean_distance_multiplier <- round(mean(rv$params$results$distance_multiplier))

      rv$results$mean_nest1_radius <- round(mean(rv$params$results$nest1_radius))

      rv$results$mean_nest2_radius <- round(mean(rv$params$results$nest2_radius))

    })



    ## + Make graphs ========================================================

    output$gr_subplot_count <- renderPlot({

      req(rv$params$results)

      rv$params$results %>%
        dplyr::filter(
          distance_multiplier == rv$results$mean_distance_multiplier,
          nest1_radius        == rv$results$mean_nest1_radius,
          nest2_radius        == rv$results$mean_nest2_radius,
          allowable_error     == as.character("10")
          ) %>%
      ggplot(aes(x = subplot_count)) +
        geom_line(aes(y = cv)) +
        geom_line(aes(y = total_time * 5), linetype = "dashed") +
        scale_y_continuous(
          name = "----- CV (%)",
          sec.axis = sec_axis(~./5, name = "- - - Time to complete the inventory (months)")
          ) +
        labs(
          x = "Number of subplots",
          caption = paste0(
            "Subplot distance mean: ", rv$results$mean_distance_multiplier * rv$results$mean_nest1_radius, " m.\n",
            "Mean level 1 subplot radius: ", rv$results$mean_nest1_radius, " m.\n",
            "Mean level 2 subplot radius: ", rv$results$mean_nest2_radius, " m."
            )
          )

    })



  }) ## END module server function

}
