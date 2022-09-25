#' Optimization parameters module server function
#'
#' @noRd
utils::globalVariables("where")
mod_opti_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns



    ##
    ## Introduction #########################################################
    ##

    ## Show/hide Introduction
    observeEvent(input$toggle_intro, {
      shinyjs::toggle(id = "opti_intro", anim = T, animType = "slide")
    })



    ##
    ## Other modules flags ###################################################
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

      if (is.null(rv$cv_model$cv_approach)) {
        shinyjs::show("require_cv")
        shinyjs::hide("content_opti")
      } else if (rv$cv_model$cv_approach == "a1" & is.null(rv$cv_model$cv_mixed)) {
        shinyjs::show("require_cv")
        shinyjs::hide("content_opti")
      } else if (rv$cv_model$cv_approach == "a1" & !is.null(rv$cv_model$cv_mixed)) {
        shinyjs::hide("require_cv")
        shinyjs::show("content_opti")
        shinyjs::show("msg_a1")
      } else if (rv$cv_model$cv_approach == "a2") {
        shinyjs::hide("require_cv")
        shinyjs::show("content_opti")
        shinyjs::hide("msg_a1")
      }

    })



    ##
    ## Pass inputs to rv ####################################################
    ##



    ## + Add params to list =================================================

    observe({

      req(input$subplot_count, input$distance_multiplier,
          input$nest1_radius, input$nest2_radius,
          input$plot_shape, input$allowable_error)

      rv$opti$list_params <- list(
        subplot_count       = input$subplot_count[1]:input$subplot_count[2],
        distance_multiplier = input$distance_multiplier[1]:input$distance_multiplier[2],
        nest1_radius        = input$nest1_radius[1]:input$nest1_radius[2],
        nest2_radius        = input$nest2_radius[1]:input$nest2_radius[2],
        plot_shape          = input$plot_shape,
        allowable_error     = input$allowable_error
        )

    })


    ## + Adapt number of subplots to plot shape =============================

    observe({

      req(rv$opti$list_params)

      if (rv$opti$list_params$plot_shape == "L") {
        ## For L shape only odd numbers allowed
        rv$opti$list_params$subplot_count <- rv$opti$list_params$subplot_count[rv$opti$list_params$subplot_count %% 2 == 1]

        rv$opti$step <- 2

        updateSliderInput(
          session = session,
          inputId = "subplot_count",
          step = rv$optis$step
        )

      }

    })



    ## + Disable calculations if incompatible params ========================

    observe({

      req(rv$opti$list_params)

      rv$opti$check_shape_L <- rv$opti$list_params$plot_shape == "L" &
        length(rv$opti$list_params$subplot_count[rv$opti$list_params$subplot_count %% 2 == 1]) == 0

      if (rv$opti$check_shape_L) {
          shinyjs::disable(id = "calc_opti")
          shinyjs::show(id = "msg_shape_L")
      } else {
        shinyjs::enable(id = "calc_opti")
        shinyjs::hide(id = "msg_shape_L")
      }

    })



    ## + Display params =====================================================

    output$out_subplot_count       <- renderPrint(rv$opti$list_params$subplot_count)

    output$out_distance_multiplier <- renderPrint(rv$opti$list_params$distance_multiplier)

    output$out_nest1_radius        <- renderPrint(rv$opti$list_params$nest1_radius)

    output$out_nest2_radius        <- renderPrint(rv$opti$list_params$nest2_radius)

    output$out_plot_shape          <- renderPrint(rv$opti$list_params$plot_shape)

    output$out_allowable_error     <- renderPrint(rv$opti$list_params$allowable_error)



    ##
    ## Make combinations ####################################################
    ##

    ## + Make a table with all the combinations =============================

    observe({
      req(rv$opti$list_params)

      rv$opti$prepa_combi <- expand.grid(rv$opti$list_params) %>%
        dplyr::as_tibble()

      rv$opti$n_combi <- nrow(rv$opti$prepa_combi)

      rv$opti$combi <- rv$opti$prepa_combi %>%
        dplyr::mutate(dplyr::across(where(is.double), as.integer)) %>%
        dplyr::mutate(dplyr::across(where(is.factor), as.character)) %>%
        dplyr::mutate(
          id               = 1:rv$opti$n_combi,
          subplot_area     = round(pi * .data$nest1_radius^2 / 100^2, 3),
          subplot_area2    = round(pi * .data$nest2_radius^2 / 100^2, 3),
          subplot_distance = .data$distance_multiplier * .data$nest1_radius
        ) %>%
        mutate(
          subplot_avg_distance = dplyr::case_when(
            .data$subplot_count == 1 ~ as.integer(.data$subplot_distance),
            .data$plot_shape == "L"  ~ as.integer(.data$subplot_distance * (.data$subplot_count - 1) * 2 / .data$subplot_count),
            TRUE ~ NA_integer_
          )
        ) %>%
        dplyr::select(.data$id, dplyr::everything())

    })


    ## + Show total number of combinations ==================================

    output$nb_combi <- renderText({
      req(rv$opti$n_combi)
      paste0("Number of combinations to test: ", rv$opti$n_combi, ".")
    })



    ##
    ## Run opti function ####################################################
    ##

    submod_opti_calc_server("box_opti_calc", rv = rv)


    #
    # observeEvent(input$calc_opti, {
    #
    #   req(rv$params$list_params)
    #
    #
    #
    #   ## + Reset outputs and results ========================================
    #
    #   shinyjs::show("opti_progress")
    #   shinyjs::hide("box_to_results")
    #
    #   shinyWidgets::updateProgressBar(session = session, id = "prog_opti", value = 0, status = "primary")
    #
    #   rv$params$results <- NULL
    #
    #
    #
    #   ## + Calculate design indicators ======================================
    #
    #   rv$params$results <- purrr::map_dfr(1:nrow(rv$params$combi), function(x){
    #
    #     #if (x == round(x/50)*50) print(paste("calculating combination ", x, "out of ", nrow(rv$params$combi)))
    #
    #     ## + + Update progress ----------------------------------------------
    #     shinyWidgets::updateProgressBar(
    #       session = session,
    #       id      = "prog_opti",
    #       value   = x,
    #       total   = nrow(rv$params$combi)
    #       )
    #
    #
    #     ## + + Get 1 set of parameters --------------------------------------
    #     params <- rv$params$combi %>%
    #       dplyr::slice(x) %>%
    #       # dplyr::mutate(
    #       #   subplot_distance     = distance_multiplier * nest1_radius,
    #       #   subplot_area         = pi * (nest1_radius / 100)^2,
    #       #   plot_area            = subplot_area * subplot_count,
    #       #   subplot_avg_distance = dplyr::case_when(
    #       #     subplot_count == 1   ~ as.interger(subplot_distance),
    #       #     plot_shape    == "L" ~ as.integer(subplot_distance * (subplot_count - 1) * 2 / subplot_count),
    #       #     TRUE ~ subplot_distance
    #       #     )
    #       #   )
    #
    #     ## + + Calculate CV -------------------------------------------------
    #     if (rv$cv_model$cv_approach == "a1") {
    #
    #       cv <- calc_CV_a1(
    #         cv_init   = rv$cv_model$cv_mixed$cv_init,
    #         area_init = rv$cv_model$cv_mixed$area_init,
    #         area_opti = params$plot_area
    #         )
    #
    #     } else if (rv$cv_model$cv_approach == "a2") {
    #
    #       cv <- calc_CV_a2(
    #         n = params$subplot_count,
    #         d = params$subplot_distance,
    #         a1 = params$subplot_area,
    #         a2 = params$subplot_area2,
    #         cv_params = rv$cv_model$cv_params
    #       )
    #
    #     }
    #
    #     ## + + Number of plots ----------------------------------------------
    #     n_plot <- ceiling((cv * stats::qt(.95, df=Inf) / as.numeric(params$allowable_error))^2)
    #
    #     ## + + Calculate plot time ------------------------------------------
    #     plot_time <- calc_time(
    #       plot_design = params,
    #       unit_times  = rv$time$unit_times,
    #       nest_design = rv$time$nested_plot
    #       ) %>%
    #       dplyr::mutate(id = params$id)
    #
    #
    #     ## + + Calculate total time -----------------------------------------
    #     total_time <- plot_time$time_plot * n_plot / (rv$time$unit_times$working_hour * rv$time$unit_times$working_day)
    #
    #     ## + + Output the parameters with the calculation results -----------
    #     params %>%
    #       dplyr::mutate(cv = cv, n_plot = n_plot, total_time = total_time) %>%
    #       #mutate(cv = cv, n_plot = n_plot) %>%
    #       dplyr::left_join(plot_time, by = "id")
    #
    #   })
    #
    #   shinyWidgets::updateProgressBar(session = session, id = "prog_opti", value = nrow(rv$params$combi), status = "success")
    #
    # })
    #
    #
    #
    # ##
    # ## Progress outputs #####################################################
    # ##
    #
    # output$gr_cv_cost <- renderPlot({
    #   req(rv$params$results)
    #
    #   if (rv$cv_model$cv_approach == "a1") {
    #
    #     med_nest2 <- round(stats::median(rv$params$results$nest2_radius))
    #     med_dist  <- round(stats::median(rv$params$results$distance_multiplier))
    #
    #     tt <- rv$params$results %>%
    #       dplyr::filter(
    #         rv$params$results$nest2_radius == med_nest2,
    #         rv$params$results$distance_multiplier == med_dist,
    #       )
    #
    #   } else {
    #
    #     tt <- rv$params$results
    #
    #   }
    #
    #   ggplot(tt, aes(x = .data$total_time, y = .data$cv)) +
    #     geom_point(aes(
    #       color = .data$n_plot,
    #       fill  = .data$n_plot,
    #       shape = as.factor(.data$subplot_count),
    #       size  = .data$nest1_radius
    #     )) +
    #     scale_color_viridis_c(alpha = 0.8) +
    #     scale_fill_viridis_c(alpha = 0.8) +
    #     scale_shape_manual(values = c(21, 22, 23, 24, 25, 8, 9, 7)) +
    #     theme_bw() +
    #     labs(
    #       x     = "Time (months)",
    #       y     = "CV (%)",
    #       color = "Number of plots",
    #       fill  = "Number of plots",
    #       shape = "Number of subplots",
    #       size  = "subplot level 1 radius (m)"
    #       )
    #
    # })
    #
    # observe({
    #   req(rv$params$results)
    #   shinyjs::show("box_to_results")
    # })
    #
    # output$download_results <- downloadHandler(
    #   filename <- function() { "oNFI-results.csv" },
    #   content  <- function(file) { readr::write_csv(rv$params$results, file) }
    #   )
    #


    ##
    ## Change tab ###########################################################
    ##

    observe({
      req(rv$opti$results)
      shinyjs::show("box_opti_to_results")
    })

    observeEvent(input$btn_to_results, {
      rv$to_results <- input$btn_to_results
    })

  }) ## END module server function

}
