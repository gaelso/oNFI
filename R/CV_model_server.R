

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
      output$show_path2 <- renderText({ rv$CV_model$file_path })


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
        download_avitabile(path_data = rv$CV_model$file_path)

        rv$CV_model$rs_avitabile <- load_crop_avitabile(path_data = rv$CV_model$file_path, sf_aoi = rv$CV_model$sf_aoi)

        rv$CV_model$df_avitabile <- mask(rv$CV_model$rs_avitabile, vect(rv$CV_model$sf_aoi)) %>%
          terra::as.data.frame(xy = TRUE) %>%
          as_tibble() %>%
          na.omit()

        output$map_avitabile <- renderPlot({
          ggplot() +
            geom_raster(data = rv$CV_model$df_avitabile, aes(x = x, y = y, fill = agb_avitabile)) +
            scale_fill_viridis_c(direction = -1) +
            geom_sf(data = rv$CV_model$sf_aoi, fill = NA, col = "darkred", size = 1) +
            theme_bw() +
            add_ggspatial(font = "LoraIt") +
            labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Avitabile et al. 2016 aboveground biomass")

        })



        # ## + + Santoro et al. 2018 map, download, load and make map ----
        # santoro_tiles <- reactive({ get_santoro_tile(rv$CV_model$sf_aoi) })
        #
        # purrr::walk(santoro_tiles(), function(x){
        #   download_santoro(
        #     path_data = rv$CV_model$file_path,
        #     url = paste0("globbiomass.org/wp-content/uploads/GB_Maps/", x)
        #     )
        # })
        #
        #
        # santoro_files <- reactive({ list.files(rv$CV_model$file_path, pattern = "_agb.tif") })
        # ## !!! For testing !!!
        # # santoro_files <- list.files(rv$CV_model$file_path, pattern = "_agb.tif")
        #
        # ## Loading all santoro tiles
        # rv$CV_model$tmp1 <- map(santoro_files, function(x){
        #
        #   rs <- terra::rast(file.path(rv$CV_model$file_path, x))
        #
        #   check <- terra::intersect(ext(rs), rv$CV_model$aoi_extent)
        #
        #    if (!is.null(check)) terra::crop(rs, rv$CV_model$aoi_extent)
        #
        #   })
        #
        # ## Removing NULL elements
        # rv$CV_model$tmp2 <- rv$CV_model$tmp1[!sapply(rv$CV_model$tmp1, is.null)]
        # rv$CV_model$tmp1 <- NULL
        #
        # ## Merging elements
        # if (length(rv$CV_model$rs_santoro) == 1) {
        #   rv$CV_model$rs_santoro <- rv$CV_model$tmp2[[1]]
        # } else if (length(rv$CV_model$rs_santoro) == 2) {
        #   rv$CV_model$rs_santoro <- terra::merge(rv$CV_model$tmp2[[1]], rv$CV_model$tmp2[[2]])
        # } else if (length(rv$CV_model$rs_santoro) == 3) {
        #   rv$CV_model$tmp3 <- terra::merge(rv$CV_model$tmp2[[1]], rv$CV_model$tmp2[[2]])
        #   rv$CV_model$rs_santoro <- terra::merge(rv$CV_model$tmp3, rv$CV_model$tmp2[[3]])
        #   rv$CV_model$tmp3 <- NULL
        # } else if (length(rv$CV_model$rs_santoro) == 3) {
        #   rv$CV_model$tmp3 <- terra::merge(rv$CV_model$tmp2[[1]], rv$CV_model$tmp2[[2]])
        #   rv$CV_model$tmp4 <- terra::merge(rv$CV_model$tmp2[[3]], rv$CV_model$tmp2[[4]])
        #   rv$CV_model$rs_santoro <- terra::merge(rv$CV_model$tmp3, rv$CV_model$tmp4)
        #   rv$CV_model$tmp3 <- NULL
        #   rv$CV_model$tmp4 <- NULL
        # }
        #
        # rv$CV_model$tmp2 <- NULL
        #
        # ## ADD as dataframe then ggplot
        # df_agb <- terra::as.data.frame(rs_agb, xy = TRUE) %>%
        #   as_tibble() %>%
        #   na.omit()


      }) ## END observeEvent spatial analysis

    }
  )
} ## END function home_server()
