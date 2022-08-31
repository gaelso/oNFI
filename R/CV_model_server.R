

## !!! For testing
# rv <- list(
#   CV_model = list(
#     file_path = "data",
#     sf_aoi = st_read("data/TimorLeste.geoJSON")  %>% st_transform(crs = 4326),
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

        rv$CV_model$sf_aoi <- st_read(input$AOI$datapath) %>% st_transform(crs = 4326)

        output$map_aoi <- renderPlot({
          ggplot() +
            geom_sf(data = rv$CV_model$sf_aoi, fill = NA, col = "darkred", size = 1) +
            theme_void()
        })

      })



      ## + AGB map spatial analysis =========================================
      observeEvent(input$calc_CV, {

        ## + + Checks -------------------------------------------------------
        ## Check path works
        ## !!! TBD !!!

        ## Check file uploaded is valid
        ## !!! TBD !!!

        ## Check at lest one biomass map selected
        ## !!! TBD !!!

        ## + + Avitabile et al. 2016 map, download, load and make map -------
        rv$CV_model$rs_avitabile <- get_avitabile(path_data = rv$CV_model$file_path, sf_aoi = rv$CV_model$sf_aoi)

        rv$CV_model$df_avitabile <- mask(rv$CV_model$rs_avitabile, vect(rv$CV_model$sf_aoi)) %>%
          terra::as.data.frame(xy = TRUE) %>%
          as_tibble() %>%
          na.omit()

        output$map_avitabile <- renderPlot({
          ggplot() +
            geom_raster(data = rv$CV_model$df_avitabile, aes(x = x, y = y, fill = agb_avitabile)) +
            scale_fill_viridis_c(limits = c(0, 500), direction = -1) +
            geom_sf(data = rv$CV_model$sf_aoi, fill = NA, col = "darkred", size = 1) +
            theme_bw() +
            add_ggspatial(font = "LoraIt") +
            labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Avitabile et al. 2016 aboveground biomass")

        })

        ## + + Santoro et al. 2018 map, download, load and make map ---------
        rv$CV_model$rs_santoro <- get_santoro(path_data = rv$CV_model$file_path, sf_aoi = rv$CV_model$sf_aoi)

        rv$CV_model$df_santoro <- mask(rv$CV_model$rs_santoro, vect(rv$CV_model$sf_aoi)) %>%
          terra::as.data.frame(rv$CV_model$rs_santoro, xy = TRUE) %>%
          as_tibble() %>%
          na.omit()

        output$map_santoro <- renderPlot({
          ggplot() +
            geom_raster(data = rv$CV_model$df_santoro, aes(x = x, y = y, fill = agb_santoro)) +
            scale_fill_viridis_c(limits = c(0, 500), direction = -1) +
            geom_sf(data = rv$CV_model$sf_aoi, fill = NA, col = "darkred", size = 1) +
            theme_bw() +
            add_ggspatial(font = "LoraIt") +
            labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Santoro et al. 2018 aboveground biomass")

        })

        ## + + Get CV model table -------------------------------------------
        #rv$CV_model$cv_avitabile <- get_cv_avitabile(df = rv$CV_model$df_avitabile)


      }) ## END observeEvent spatial analysis

    }
  )
} ## END function CV_model_server()
