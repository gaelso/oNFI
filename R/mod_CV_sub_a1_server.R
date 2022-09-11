

mod_CV_sub_a1_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv_a1 <- reactiveValues()

    ##
    ## Re-initiate paramns and show/hide on click start_CV ##################
    ##

    observeEvent(rv$CV_model$start_CV, {

      shinyjs::hide("panel_a1_progress")
      shinyjs::hide("box_progress_to_results")
      shinyjs::hide("panel_a1_results")

      ## Reset calculation button and associated messages
      shinyjs::disable("calc_CV")
      shinyjs::show("msg_step_path_data")
      shinyjs::hide("msg_step_path_data_ok")
      shinyjs::show("msg_step_aoi_file")
      shinyjs::hide("msg_step_aoi_file_ok")
      shinyjs::show("msg_step_agb_min")
      shinyjs::hide("msg_step_agb_min_ok")

      rv_a1 <- reactiveValues(
        sf_aoi       = NULL,
        agb_min      = NULL,
        rs_avitabile = NULL,
        df_avitabile = NULL,
        cv_avitabile = NULL,
        rs_santoro   = NULL,
        df_santoro   = NULL,
        cv_santoro   = NULL
      )

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

    observe({
      req(input$AOI)
      rv_a1$sf_aoi <- st_read(input$AOI$datapath)
      })

    output$map_aoi <- renderPlot({

      req(rv_a1$sf_aoi)

      ggplot() +
        geom_sf(data = rv_a1$sf_aoi, fill = NA, col = "darkred", size = 1) +
        theme_void()

    })



    ## + Checking AGB min value =============================================

    observe({

      # rv$CV_model$agb_min <- input$agb_min
      rv_a1$agb_min <- input$agb_min

      if (rv_a1$agb_min == 0) {
        shinyjs::show("msg_step_agb_min")
        shinyjs::hide("msg_step_agb_min_ok")
      } else {
        shinyjs::hide("msg_step_agb_min")
        shinyjs::show("msg_step_agb_min_ok")
      }

    })



    ## + Enable calculations ================================================

    observe({
      req(rv_a1$sf_aoi)
      shinyjs::enable("calc_CV")
      shinyjs::hide("msg_step_aoi_file")
      shinyjs::show("msg_step_aoi_file_ok")
    })



    ##
    ## Run calculations #####################################################
    ##

    observeEvent(input$calc_CV, {



      ## + Initiate/reset progress bars =====================================

      shinyjs::show("panel_a1_progress")
      shinyjs::hide("box_progress_to_results")
      shinyjs::hide("panel_a1_results")

      updateProgressBar(session = session, id = "prog_checks", value = 0, status = NULL)
      updateProgressBar(session = session, id = "prog_avit"  , value = 0, status = NULL)
      updateProgressBar(session = session, id = "prog_sant"  , value = 0, status = NULL)
      updateProgressBar(session = session, id = "prog_CV"    , value = 0, status = NULL)

      ## Reset reactive values
      rv$CV_model$cv_mixed <- NULL
      rv$CV_model$aoi_area <- NULL



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
      rv_a1$rs_avitabile <- get_avitabile(
        path_data   = file_path(),
        sf_aoi      = rv_a1$sf_aoi,
        progress_id = "prog_avit",
        session     = session
        )

      rv_a1$df_avitabile <- terra::mask(rv_a1$rs_avitabile, terra::vect(rv_a1$sf_aoi)) %>%
        terra::as.data.frame(xy = TRUE) %>%
        as_tibble() %>%
        na.omit()

      updateProgressBar(session = session, id = "prog_avit", value = 100, status = "success")

      ## + + Santoro et al. 2018 map, download, load and make map ---------
      rv_a1$rs_santoro <- get_santoro(
        path_data   = file_path(),
        sf_aoi      = rv_a1$sf_aoi,
        progress_id = "prog_sant",
        session     = session
        )

      rv_a1$df_santoro <- terra::mask(rv_a1$rs_santoro, terra::vect(rv_a1$sf_aoi)) %>%
        terra::as.data.frame(rv_a1$rs_santoro, xy = TRUE) %>%
        as_tibble() %>%
        na.omit()

      updateProgressBar(session = session, id = "prog_sant", value = 100, status = "success")



      ## + Calculate CV =====================================================

      rv_a1$cv_avitabile <- get_CV_AGB(df = rv_a1$df_avitabile, agb_min = input$agb_min) %>%
        mutate(
          area_init = terra::res(rv_a1$rs_avitabile)[1]^2 / 100^2,
          source    = "Avitabile et al. 2016"
          )

      rv_a1$cv_santoro <- get_CV_AGB(df = rv_a1$df_santoro, agb_min = input$agb_min) %>%
        mutate(
          area_init = terra::res(rv_a1$rs_santoro)[1]^2 / 100^2,
          source    = "Santoro et al. 2018"
        )

      ## Pass cv_mixed and area_aoi to global reactive Value
      rv$CV_model$cv_mixed <- tibble(
        CV_init   = mean(c(rv_a1$cv_santoro$CV_init, rv_a1$cv_avitabile$CV_init)),
        area_init = max(c(rv_a1$cv_santoro$area_init, rv_a1$cv_avitabile$area_init)),
        source = "Average CV"
      )

      rv$CV_model$area_aoi <- round(as.numeric(st_area(rv_a1$sf_aoi)) / 1000^2)

      updateProgressBar(session = session, id = "prog_CV", value = 100, status = "success")

      }) ## END observeEvent spatial analysis



    ##
    ## Make outputs #########################################################
    ##

    output$map_agb <- renderPlot({

      req(rv_a1$sf_aoi, rv_a1$df_avitabile, rv_a1$df_santoro)

      gr1 <- ggplot() +
        geom_tile(data = rv_a1$df_avitabile, aes(x = x, y = y, fill = agb_avitabile)) +
        scale_fill_viridis_c(direction = -1) +
        geom_sf(data = rv_a1$sf_aoi, fill = NA, col = "darkred", size = 1) +
        theme_bw() +
        theme(legend.key.height = unit(2, "cm")) +
        add_ggspatial(font = "LoraIt") +
        labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Avitabile et al. 2016 aboveground biomass")

      gr2 <- ggplot() +
        geom_tile(data = rv_a1$df_santoro, aes(x = x, y = y, fill = agb_santoro)) +
        scale_fill_viridis_c(direction = -1) +
        geom_sf(data = rv_a1$sf_aoi, fill = NA, col = "darkred", size = 1) +
        theme_bw() +
        theme(legend.key.height = unit(2, "cm")) +
        add_ggspatial(font = "LoraIt") +
        labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Santoro et al. 2018 aboveground biomass")

      ggpubr::ggarrange(gr1, gr2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")

    })


    output$CV_table <- renderTable({

      req(rv_a1$cv_avitabile, rv_a1$cv_santoro, rv$CV_model$cv_mixed)

      rv_a1$cv_avitabile %>%
        bind_rows(rv_a1$cv_santoro) %>%
        bind_rows(rv$CV_model$cv_mixed)

    })


    output$area_aoi <- renderText({

      req(rv$CV_model$area_aoi)

      paste0("Based on the shapefile uploaded the area if interest has an area of ",  rv$CV_model$area_aoi, " sq. km.")

    })


    ## Update show / hide panels
    observe({

      req(rv$CV_model$cv_mixed)
      shinyjs::show("box_progress_to_results")

    })


    observeEvent(input$btn_show_results, {

      shinyjs::hide("panel_a1_progress")
      shinyjs::hide("box_progress_to_results")
      shinyjs::show("panel_a1_results")

    })

  }) ## END module server function

}
