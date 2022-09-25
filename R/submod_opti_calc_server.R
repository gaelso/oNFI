#' Introduction sub-module server function to the Optimization module
#'
#' @noRd
submod_opti_calc_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ##
    ## Run opti function ####################################################
    ##

    observeEvent(input$start_calc, {

      req(rv$cv_model$cv_approach, rv$opti$n_combi, rv$opti$combi)


      ## + Reset outputs and results ========================================

      shinyjs::show("opti_progress")
      shinyjs::hide("summary_results")

      rv$opti$results <- NULL

      shinyWidgets::updateProgressBar(
        session = session,
        id = "prog_opti",
        value = 0,
        total = rv$opti$n_combi,
        status = "primary")


      ## + Calculate design indicators ======================================

      rv$opti$results <- purrr::map_dfr(1:rv$opti$n_combi, function(x){

        ## + + Update progress ----------------------------------------------
        shinyWidgets::updateProgressBar(
          session = session,
          id      = "prog_opti",
          value   = x,
          total   = rv$opti$n_combi
          )

        ## + + Get 1 set of parameters --------------------------------------
        params <- rv$opti$combi %>% dplyr::slice(x)

        ## + + Calculate CV -------------------------------------------------
        if (rv$cv_model$cv_approach == "a1") {

          cv <- calc_CV_a1(
            cv_init   = rv$cv_model$cv_mixed$cv_init,
            area_init = rv$cv_model$cv_mixed$area_init,
            area_opti = round(pi * (params$nest1_radius / 100)^2) * params$subplot_count
            )

        } else if (rv$cv_model$cv_approach == "a2") {

          cv <- calc_CV_a2(
            n = params$subplot_count,
            d = params$subplot_distance,
            a1 = params$subplot_area,
            a2 = params$subplot_area2,
            cv_params = rv$cv_model$cv_params
          )

        }

        ## + + Number of plots ----------------------------------------------
        n_plot <- ceiling(
          (cv * stats::qt(.95, df=Inf) / as.numeric(params$allowable_error))^2
          )

        ## + + Calculate plot time ------------------------------------------
        plot_time <- calc_time(
          plot_design = params,
          unit_times  = rv$time$unit_times,
          nest_design = rv$time$nested_plot
          ) %>%
          dplyr::mutate(id = params$id)


        ## + + Calculate total time -----------------------------------------
        total_time <- plot_time$time_plot * n_plot / (rv$time$unit_times$working_hour * rv$time$unit_times$working_day)

        ## + + Output the parameters with the calculation results -----------
        params %>%
          dplyr::mutate(cv = cv, n_plot = n_plot, total_time = total_time) %>%
          dplyr::left_join(plot_time, by = "id")

      })

      shinyWidgets::updateProgressBar(
        session = session,
        id = "prog_opti",
        value = rv$opti$n_combi,
        total = rv$opti$n_combi,
        status = "success"
        )

    })



    ##
    ## Optimization outputs ##################################################
    ##

    output$gr_cv_cost <- renderPlot({
      req(rv$cv_model$cv_approach, rv$opti$results)

      if (rv$cv_model$cv_approach == "a1") {

        med_nest2 <- round(stats::median(rv$opti$results$nest2_radius))
        med_dist  <- round(stats::median(rv$opti$results$distance_multiplier))

        tt <- rv$opti$results %>%
          dplyr::filter(
            rv$opti$results$nest2_radius == med_nest2,
            rv$opti$results$distance_multiplier == med_dist,
          )

      } else {

        tt <- rv$opti$results

      }

      ggplot(tt, aes(x = .data$total_time, y = .data$cv)) +
        geom_point(aes(
          color = .data$n_plot,
          fill  = .data$n_plot,
          shape = as.factor(.data$subplot_count),
          size  = .data$nest1_radius
        )) +
        scale_color_viridis_c(alpha = 0.8) +
        scale_fill_viridis_c(alpha = 0.8) +
        scale_shape_manual(values = c(21, 22, 23, 24, 25, 8, 9, 7)) +
        theme_bw() +
        labs(
          x     = "Time (months)",
          y     = "CV (%)",
          color = "Number of plots",
          fill  = "Number of plots",
          shape = "Number of subplots",
          size  = "subplot level 1 radius (m)"
          )

    })


    observe({
      req(rv$opti$results)
      shinyjs::show("summary_results")
    })


    output$download_results <- downloadHandler(
      filename <- function() { "oNFI-results.csv" },
      content  <- function(file) { readr::write_csv(rv$opti$results, file) }
      )

  })

}
