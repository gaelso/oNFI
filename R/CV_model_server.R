

## !!! For testing
# rv <- list(
#   CV_model = list(
#     file_path = "data",
#     sf_aoi = st_read("data/TimorLeste.geoJSON"),
#     santoro_tile = NULL
#     )
#   )


CV_model_server <- function(id, rv) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      ## Introduction #######################################################

      ## Show/hide Introduction
      observeEvent(input$toggle_intro, {
        shinyjs::toggle(id = "intro1", anim = T, animType = "slide")
        shinyjs::toggle(id = "intro2", anim = T, animType = "fade")
        shinyjs::toggle(id = "intro3", anim = T, animType = "fade")
      })

      ## Approach selection #################################################
      CV_approach <- reactive({ input$approach })

      observeEvent(input$start_CV, {

        if(CV_approach() == "a1") {
          shinyjs::show("AGB_map")
          shinyjs::hide("CV_params")
        } else if(CV_approach() == "a2") {
          shinyjs::hide("AGB_map")
          shinyjs::show("CV_params")
        }

      })



      ## AGB map setup ######################################################

      ## + Choosing a dir on the computer ===================================
      roots = c(wd='.')

      shinyFiles::shinyDirChoose(input, 'folder', roots = roots, filetypes = c('', 'txt'), session = session)

      observe({

        if (rlang::is_empty(shinyFiles::parseDirPath(roots, input$folder))) {
          rv$CV_model$file_path <- tempdir()
        } else {
          rv$CV_model$file_path <- as.character(shinyFiles::parseDirPath(roots, input$folder))
        }

      })

      output$show_path <- renderText({

        if(rv$CV_model$file_path == tempdir()) "No folder selected" else rv$CV_model$file_path

        })

      ## !!! For testing
      # output$show_path2 <- renderText({ rv$CV_model$file_path })


      ## + + Loading AOI ----------------------------------------------------

      observeEvent(input$to_step3, {

        rv$CV_model$sf_aoi <- st_read(input$AOI$datapath)

        output$map_aoi <- renderPlot({
          ggplot() +
            geom_sf(data = rv$CV_model$sf_aoi, fill = NA, col = "darkred", size = 1) +
            theme_void()
        })

      })



      ## + AGB map spatial analysis =========================================
      observeEvent(input$calc_CV, {

        ## + + Checks -------------------------------------------------------

        updateProgressBar(
          session = session,
          id = "progress_CV",
          value = 1, total = 100,
          title = "Checking input values"
        )

        ## TO BE REMOVED when checks implemented
        Sys.sleep(2)

        ## Check path works
        ## !!! TBD !!!

        ## Check file uploaded is valid
        ## !!! TBD !!!

        ## Check at lest one biomass map selected
        ## !!! TBD !!!

        ## + + Avitabile et al. 2016 map, download, load and make map -------
        updateProgressBar(
          session = session,
          id = "progress_CV",
          value = 10, total = 100,
          title = "Downloading and preparing Avitabile et al. 2016 raster data"
        )

        rv$CV_model$rs_avitabile <- get_avitabile(path_data = rv$CV_model$file_path, sf_aoi = rv$CV_model$sf_aoi)

        rv$CV_model$df_avitabile <- mask(rv$CV_model$rs_avitabile, vect(rv$CV_model$sf_aoi)) %>%
          terra::as.data.frame(xy = TRUE) %>%
          as_tibble() %>%
          na.omit()

        ## + + Santoro et al. 2018 map, download, load and make map ---------
        updateProgressBar(
          session = session,
          id = "progress_CV",
          value = 30, total = 100,
          title = "Downloading and preparing Avitabile et al. 2016 raster data"
        )

        rv$CV_model$rs_santoro <- get_santoro(path_data = rv$CV_model$file_path, sf_aoi = rv$CV_model$sf_aoi)

        rv$CV_model$df_santoro <- mask(rv$CV_model$rs_santoro, vect(rv$CV_model$sf_aoi)) %>%
          terra::as.data.frame(rv$CV_model$rs_santoro, xy = TRUE) %>%
          as_tibble() %>%
          na.omit()

        ## + + Show maps ----------------------------------------------------
        updateProgressBar(
          session = session,
          id = "progress_CV",
          value = 50, total = 100,
          title = "Making maps"
        )

        max_agb <- reactive({

          max1 <- max(c(rv$CV_model$df_santoro$agb_santoro, rv$CV_model$df_avitabile$agb_avitabile))
          max2 <- ceiling(max1 / 100) * 100
          max2

        })

        output$map_avitabile <- renderPlot({
          ggplot() +
            geom_tile(data = rv$CV_model$df_avitabile, aes(x = x, y = y, fill = agb_avitabile)) +
            scale_fill_viridis_c(limits = c(0, max_agb()), direction = -1) +
            geom_sf(data = rv$CV_model$sf_aoi, fill = NA, col = "darkred", size = 1) +
            theme_bw() +
            theme(legend.position = "bottom", legend.key.width = unit(2, "cm")) +
            add_ggspatial(font = "LoraIt") +
            labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Avitabile et al. 2016 aboveground biomass")

        })

        output$map_santoro <- renderPlot({
          ggplot() +
            geom_tile(data = rv$CV_model$df_santoro, aes(x = x, y = y, fill = agb_santoro)) +
            scale_fill_viridis_c(limits = c(0, max_agb()), direction = -1) +
            geom_sf(data = rv$CV_model$sf_aoi, fill = NA, col = "darkred", size = 1) +
            theme_bw() +
            theme(legend.position = "none") +
            add_ggspatial(font = "LoraIt") +
            labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Santoro et al. 2018 aboveground biomass")

        })

        ## + + Get CV -------------------------------------------------------
        updateProgressBar(
          session = session,
          id = "progress_CV",
          value = 70, total = 100,
          title = "Performing CV calculations"
        )

        rv$CV_model$cv_avitabile <- get_CV_AGB(df = rv$CV_model$df_avitabile, agb_min = input$agb_min) %>%
          mutate(
            CV_init_area = terra::res(rv$CV_model$rs_avitabile)[1]^2 / 100^2,
            source       = "Avitabile et al. 2016"
            )

        rv$CV_model$cv_santoro <- get_CV_AGB(df = rv$CV_model$df_santoro, agb_min = input$agb_min) %>%
          mutate(
            CV_init_area = terra::res(rv$CV_model$rs_santoro)[1]^2 / 100^2,
            source       = "Santoro et al. 2018"
          )

        rv$CV_model$cv_mixed <- tibble(
          CV_init = mean(rv$CV_model$cv_santoro$CV_init, rv$CV_model$cv_avitabile$CV_init),
          CV_init_area = max(rv$CV_model$cv_santoro$CV_init_area, rv$CV_model$cv_avitabile$CV_init_area),
          source = "Average CV"
        )

        rv$CV_model$areas <- tibble(
          country_area = round(as.numeric(st_area(rv$CV_model$sf_aoi)) / 1000^2),
          forest_area  = round(country_area * input$forest_cover / 100)
        )


        ## + + Show tables --------------------------------------------------
        updateProgressBar(
          session = session,
          id = "progress_CV",
          value = 90, total = 100,
          title = "Making tables"
        )

        output$CV_table <- renderTable({

          rv$CV_model$cv_avitabile %>%
            bind_rows(rv$CV_model$cv_santoro) %>%
            bind_rows(rv$CV_model$cv_mixed)

        })

        output$area_table <- renderTable({

          rv$CV_model$areas

        })

        updateProgressBar(
          session = session,
          id = "progress_CV",
          value = 100, total = 100,
          title = "Tasks completed",
          status = "success"
        )


      }) ## END observeEvent spatial analysis

    }
  )
} ## END function CV_model_server()
