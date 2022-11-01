#' CV approach 1 submodule server function
#'
#' @noRd
submod_CV_a1_server <- function(id, rv, rv_cv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ##
    ## Initiate or Reset show/hide ##########################################
    ##

    observeEvent(rv$cv_model$start_cv, {

      ## Hide progress and results
      shinyjs::hide("panel_a1_progress")
      shinyjs::hide("box_progress_to_results")
      shinyjs::hide("panel_a1_results")

      ## Reset calculation button and associated messages
      shinyjs::disable("calc_cv")
      if (file_path() == tempdir()) shinyjs::show("msg_step_path_data") else shinyjs::hide("msg_step_path_data_ok")
      shinyjs::show("msg_step_aoi_file")
      shinyjs::hide("msg_step_aoi_file_ok")
      shinyjs::show("msg_step_agb_min")
      shinyjs::hide("msg_step_agb_min_ok")

    })



    ##
    ## Input checks #########################################################
    ##

    ## + Choosing a dir on the computer =====================================

    roots = c(wd='.')

    shinyFiles::shinyDirChoose(input, 'folder', roots = roots, filetypes = c('', 'txt'), session = session)

    file_path <- reactive({

      if (rlang::is_empty(shinyFiles::parseDirPath(roots, input$folder))) {
        tempdir()
      } else {
        as.character(shinyFiles::parseDirPath(roots, input$folder))
      }

    })


    observe({

      if (file_path() == tempdir()) {
        shinyjs::show("msg_step_path_data")
        shinyjs::hide("msg_step_path_data_ok")
      } else {
        shinyjs::hide("msg_step_path_data")
        shinyjs::show("msg_step_path_data_ok")
      }

    })


    output$show_path <- renderText({

      if(file_path() == tempdir()) "No folder selected" else file_path()

      })



    ## + Loading AOI ========================================================

    observeEvent(input$AOI, {

      req(input$AOI)

      rv_cv$sf_aoi <- sf::st_read(input$AOI$datapath)

      shinyjs::hide("msg_step_aoi_file")
      shinyjs::show("msg_step_aoi_file_ok")

      })


    output$map_aoi <- renderPlot({

      req(rv_cv$sf_aoi)

      ggplot() +
        geom_sf(data = rv_cv$sf_aoi, fill = NA, col = "darkred", size = 1) +
        theme_void()

    })



    ## + Checking AGB min value =============================================

    observeEvent(input$agb_min, {

      if (is.na(input$agb_min)) {
        shinyjs::hide("msg_step_agb_min")
        shinyjs::hide("msg_step_agb_min_ok")
        shinyjs::show("msg_step_agb_min_missing")
      } else if (input$agb_min == 0) {
        shinyjs::show("msg_step_agb_min")
        shinyjs::hide("msg_step_agb_min_ok")
        shinyjs::hide("msg_step_agb_min_missing")
      } else {
        shinyjs::hide("msg_step_agb_min")
        shinyjs::show("msg_step_agb_min_ok")
        shinyjs::hide("msg_step_agb_min_missing")
      }

      rv_cv$agb_min <- input$agb_min

    })



    ## + Enable calculations ================================================

    observe({
      req(rv_cv$sf_aoi)

      if(!is.na(input$agb_min)) {
        shinyjs::enable("calc_cv")
      } else {
        shinyjs::disable("calc_cv")
      }

    })



    ##
    ## Run calculations #####################################################
    ##

    observeEvent(input$calc_cv, {



      ## + Initiate/reset progress bars =====================================

      shinyjs::show("panel_a1_progress")
      shinyjs::hide("box_progress_to_results")
      shinyjs::hide("panel_a1_results")

      updateProgressBar(session = session, id = "prog_checks", value = 0, status = "primary")
      updateProgressBar(session = session, id = "prog_avit"  , value = 0, status = "primary")
      updateProgressBar(session = session, id = "prog_sant"  , value = 0, status = "primary")
      updateProgressBar(session = session, id = "prog_cv"    , value = 0, status = "primary")

      ## Reset reactive values
      rv$cv_model$cv_mixed <- NULL
      rv$cv_model$aoi_area <- NULL



      ## + Checks ===========================================================

      ## !!! TO BE REMOVED when checks implemented
      Sys.sleep(2)

      ## Check path works
      ## !!! TBD !!!

      ## Check file uploaded is valid
      ## !!! TBD !!!

      updateProgressBar(session = session, id = "prog_checks", value = 100, status = "success")



      ## + Get Raster data ==================================================

      ## + + Avitabile et al. 2016 map, download, load and make map ---------
      rv_cv$rs_avitabile <- get_avitabile(
        path_data   = file_path(),
        sf_aoi      = rv_cv$sf_aoi,
        progress_id = "prog_avit",
        session     = session
        )

      shinyWidgets::updateProgressBar(session = session, id = "prog_avit", value = 80)

      rv_cv$df_avitabile <- make_df(rs = rv_cv$rs_avitabile)

      shinyWidgets::updateProgressBar(session = session, id = "prog_avit", value = 100, status = "success")


      ## + + Santoro et al. 2018 map, download, load and make map ---------
      rv_cv$rs_santoro <- get_santoro(
        path_data   = file_path(),
        sf_aoi      = rv_cv$sf_aoi,
        progress_id = "prog_sant",
        session     = session,
        tile_name = NULL
        )

      shinyWidgets::updateProgressBar(session = session, id = "prog_sant", value = 80)

      rv_cv$df_santoro <- make_df(rs = rv_cv$rs_santoro)

      shinyWidgets::updateProgressBar(session = session, id = "prog_sant", value = 100, status = "success")



      ## + Calculate CV =====================================================

      rv_cv$cv_avitabile <- get_CV_AGB(rs = rv_cv$rs_avitabile, agb_min = input$agb_min) %>%
        dplyr::mutate(
          source = "Avitabile et al. 2016"
          )

      shinyWidgets::updateProgressBar(session = session, id = "prog_cv", value = 30)

      rv_cv$cv_santoro <- get_CV_AGB(rs = rv_cv$rs_santoro, agb_min = input$agb_min) %>%
        dplyr::mutate(
          source = "Santoro et al. 2018"
        )

      shinyWidgets::updateProgressBar(session = session, id = "prog_cv", value = 80)

      ## Pass cv_mixed and area_aoi to global reactive Value
      rv$cv_model$cv_mixed <- dplyr::tibble(
        cv_init   = mean(c(rv_cv$cv_santoro$cv_init, rv_cv$cv_avitabile$cv_init)),
        area_init = max(c(rv_cv$cv_santoro$area_init, rv_cv$cv_avitabile$area_init)),
        source = "Average CV"
      )

      rv$cv_model$area_aoi <- round(as.numeric(sf::st_area(rv_cv$sf_aoi)) / 1000^2)

      shinyWidgets::updateProgressBar(session = session, id = "prog_cv", value = 100, status = "success")

      }) ## END observeEvent spatial analysis



    ##
    ## Make outputs #########################################################
    ##

    output$map_agb <- renderPlot({

      req(rv_cv$sf_aoi, rv_cv$df_avitabile, rv_cv$df_santoro)

      gr1 <- ggplot() +
        geom_tile(data = rv_cv$df_avitabile, aes(x = .data$x, y = .data$y, fill = .data$agb_avitabile)) +
        scale_fill_viridis_c(direction = -1) +
        geom_sf(data = rv_cv$sf_aoi, fill = NA, col = "darkred", size = 1) +
        theme_bw() +
        theme(legend.key.height = unit(2, "cm")) +
        add_ggspatial(font = "LoraIt") +
        labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Avitabile et al. 2016 aboveground biomass") +
        coord_sf(crs = 4326)

      gr2 <- ggplot() +
        geom_tile(data = rv_cv$df_santoro, aes(x = .data$x, y = .data$y, fill = .data$agb_santoro)) +
        scale_fill_viridis_c(direction = -1) +
        geom_sf(data = rv_cv$sf_aoi, fill = NA, col = "darkred", size = 1) +
        theme_bw() +
        theme(legend.key.height = unit(2, "cm")) +
        add_ggspatial(font = "LoraIt") +
        labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Santoro et al. 2018 aboveground biomass") +
        coord_sf(crs = 4326)

      ggpubr::ggarrange(gr1, gr2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")

    })


    output$cv_table <- renderTable({

      req(rv_cv$cv_avitabile, rv_cv$cv_santoro, rv$cv_model$cv_mixed)

      rv_cv$cv_avitabile %>%
        dplyr::bind_rows(rv_cv$cv_santoro) %>%
        dplyr::bind_rows(rv$cv_model$cv_mixed)

    })


    output$area_aoi <- renderText({

      req(rv$cv_model$area_aoi)

      paste0("Based on the shapefile uploaded the size of the area of interest is: ",  rv$cv_model$area_aoi, " sq. km.")

    })


    ## Update show / hide panels
    observe({
      req(rv$cv_model$cv_mixed)
      shinyjs::show("box_progress_to_results")
    })


    observeEvent(input$btn_show_results, {

      shinyjs::hide("panel_a1_progress")
      shinyjs::hide("box_progress_to_results")
      shinyjs::show("panel_a1_results")

    })

  }) ## END module server function

}
