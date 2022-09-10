

## !!! For testing
# rv <- list(
#   CV_model = list(
#     file_path = "data",
#     sf_aoi = st_read("data/TimorLeste.geoJSON"),
#     santoro_tile = NULL
#     )
#   )


mod_CV_server <- function(id, rv) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      ns <- session$ns



      ##
      ## Introduction #######################################################
      ##

      mod_CV_sub_intro_server("cv_intro", rv = rv)

      # ## Show/hide Introduction
      # observeEvent(input$toggle_intro, {
      #   shinyjs::toggle(id = "intro1", anim = T, animType = "slide")
      #   shinyjs::toggle(id = "intro2", anim = T, animType = "slide")
      #   shinyjs::toggle(id = "intro3", anim = T, animType = "slide")
      # })



      ##
      ## Approach selection #################################################
      ##

      ## Reset/initiate approach based panels
      observeEvent(input$start_CV, {

        rv$CV_model$cv_approach <- input$approach



        ## + Re-initiate panels and steps ===================================

        if(rv$CV_model$cv_approach == "a1") {

          ## Re-initiate panels
          shinyjs::reset("layout_a1")
          shinyjs::show("layout_a1")
          shinyjs::hide("layout_a2")

          shinyjs::hide("panel_a1_progress")
          shinyjs::hide("box_progress_to_results")
          shinyjs::hide("panel_a1_results")
          shinyjs::hide("box_CV_to_params")

          ## Reset calculation button and associated messages
          shinyjs::disable("calc_CV")
          shinyjs::show("msg_step_path_data")
          shinyjs::hide("msg_step_path_data_ok")
          shinyjs::show("msg_step_aoi_file")
          shinyjs::hide("msg_step_aoi_file_ok")
          shinyjs::show("msg_step_agb_min")
          shinyjs::hide("msg_step_agb_min_ok")

        } else if (rv$CV_model$cv_approach == "a2") {

          ## Re-initiate panels
          shinyjs::reset("layout_a2")
          shinyjs::show("layout_a2")
          shinyjs::hide("layout_a1")
          shinyjs::show("box_CV_to_params")

        }



        ## + Reset CV_model values ==========================================

        rv$CV_model$file_path    <- NULL
        rv$CV_model$sf_aoi       <- NULL
        rv$CV_model$rs_avitabile <- NULL
        rv$CV_model$df_avitabile <- NULL
        rv$CV_model$cv_avitabile <- NULL
        rv$CV_model$rs_santoro   <- NULL
        rv$CV_model$df_santoro   <- NULL
        rv$CV_model$cv_santoro   <- NULL
        rv$CV_model$cv_mixed     <- NULL

      })



      ##
      ## Panel A1 server ####################################################
      ##

      # ## + Choosing a dir on the computer ===================================
      # roots = c(wd='.')
      #
      # shinyFiles::shinyDirChoose(input, 'folder', roots = roots, filetypes = c('', 'txt'), session = session)
      #
      # observe({
      #
      #   if (rlang::is_empty(shinyFiles::parseDirPath(roots, input$folder))) {
      #     rv$CV_model$file_path <- tempdir()
      #   } else {
      #     rv$CV_model$file_path <- as.character(shinyFiles::parseDirPath(roots, input$folder))
      #   }
      #
      #   if (rv$CV_model$file_path == tempdir()) {
      #     shinyjs::show("msg_step_path_data")
      #     shinyjs::hide("msg_step_path_data_ok")
      #   } else {
      #     shinyjs::hide("msg_step_path_data")
      #     shinyjs::show("msg_step_path_data_ok")
      #   }
      #
      # })
      #
      # output$show_path <- renderText({
      #
      #   if(rv$CV_model$file_path == tempdir()) "No folder selected" else rv$CV_model$file_path
      #
      #   })
      #
      #
      #
      # ## + Loading AOI ======================================================
      #
      # observe({
      #   req(input$AOI)
      #   rv$CV_model$sf_aoi <- st_read(input$AOI$datapath)
      #   })
      #
      # output$map_aoi <- renderPlot({
      #
      #   req(rv$CV_model$sf_aoi)
      #
      #   ggplot() +
      #     geom_sf(data = rv$CV_model$sf_aoi, fill = NA, col = "darkred", size = 1) +
      #     theme_void()
      #
      # })
      #
      #
      #
      # ## + Checking AGB min value ===========================================
      #
      # observe({
      #
      #   rv$CV_model$agb_min <- input$agb_min
      #
      #   if (rv$CV_model$agb_min == 0) {
      #     shinyjs::show("msg_step_agb_min")
      #     shinyjs::hide("msg_step_agb_min_ok")
      #   } else {
      #     shinyjs::hide("msg_step_agb_min")
      #     shinyjs::show("msg_step_agb_min_ok")
      #   }
      #
      # })
      #
      #
      #
      # ## + Enable calculations ==============================================
      #
      # observe({
      #   req(rv$CV_model$sf_aoi)
      #   shinyjs::enable("calc_CV")
      #   shinyjs::hide("msg_step_aoi_file")
      #   shinyjs::show("msg_step_aoi_file_ok")
      # })
      #
      #
      #
      # ## + A1 spatial analysis ==============================================
      #
      # observeEvent(input$calc_CV, {
      #
      #   ## Initiate/reset progress bars
      #   shinyjs::show("panel_a1_progress")
      #   shinyjs::hide("box_progress_to_results")
      #   shinyjs::hide("panel_a1_results")
      #
      #   updateProgressBar(session = session, id = "prog_checks", value = 0, status = NULL)
      #   updateProgressBar(session = session, id = "prog_avit"  , value = 0, status = NULL)
      #   updateProgressBar(session = session, id = "prog_sant"  , value = 0, status = NULL)
      #   updateProgressBar(session = session, id = "prog_CV"    , value = 0, status = NULL)
      #
      #   ## Reset reactive values
      #   rv$CV_model$cv_mixed <- NULL
      #
      #   ## + + Checks -------------------------------------------------------
      #   ## !!! TO BE REMOVED when checks implemented
      #   Sys.sleep(2)
      #
      #   ## Check path works
      #   ## !!! TBD !!!
      #
      #   ## Check file uploaded is valid
      #   ## !!! TBD !!!
      #
      #   ## Check at lest one biomass map selected
      #   ## !!! TBD !!!
      #
      #   updateProgressBar(session = session, id = "prog_checks", value = 100, status = "success")
      #
      #   ## + + Avitabile et al. 2016 map, download, load and make map -------
      #   rv$CV_model$rs_avitabile <- get_avitabile(path_data   = rv$CV_model$file_path,
      #                                             sf_aoi      = rv$CV_model$sf_aoi,
      #                                             progress_id = "prog_avit",
      #                                             session     = session)
      #
      #   rv$CV_model$df_avitabile <- terra::mask(rv$CV_model$rs_avitabile, terra::vect(rv$CV_model$sf_aoi)) %>%
      #     terra::as.data.frame(xy = TRUE) %>%
      #     as_tibble() %>%
      #     na.omit()
      #
      #   updateProgressBar(session = session, id = "prog_avit", value = 100, status = "success")
      #
      #   ## + + Santoro et al. 2018 map, download, load and make map ---------
      #   rv$CV_model$rs_santoro <- get_santoro(path_data   = rv$CV_model$file_path,
      #                                         sf_aoi      = rv$CV_model$sf_aoi,
      #                                         progress_id = "prog_sant",
      #                                         session     = session)
      #
      #   rv$CV_model$df_santoro <- terra::mask(rv$CV_model$rs_santoro, terra::vect(rv$CV_model$sf_aoi)) %>%
      #     terra::as.data.frame(rv$CV_model$rs_santoro, xy = TRUE) %>%
      #     as_tibble() %>%
      #     na.omit()
      #
      #   updateProgressBar(session = session, id = "prog_sant", value = 100, status = "success")
      #
      #   ## + + Get CV -------------------------------------------------------
      #   rv$CV_model$cv_avitabile <- get_CV_AGB(df = rv$CV_model$df_avitabile, agb_min = input$agb_min) %>%
      #     mutate(
      #       CV_init_area = terra::res(rv$CV_model$rs_avitabile)[1]^2 / 100^2,
      #       source       = "Avitabile et al. 2016"
      #       )
      #
      #   rv$CV_model$cv_santoro <- get_CV_AGB(df = rv$CV_model$df_santoro, agb_min = input$agb_min) %>%
      #     mutate(
      #       CV_init_area = terra::res(rv$CV_model$rs_santoro)[1]^2 / 100^2,
      #       source       = "Santoro et al. 2018"
      #     )
      #
      #   rv$CV_model$cv_mixed <- tibble(
      #     CV_init = mean(c(rv$CV_model$cv_santoro$CV_init, rv$CV_model$cv_avitabile$CV_init)),
      #     CV_init_area = max(rv$CV_model$cv_santoro$CV_init_area, rv$CV_model$cv_avitabile$CV_init_area),
      #     source = "Average CV"
      #   )
      #
      #   rv$CV_model$area_aoi <- round(as.numeric(st_area(rv$CV_model$sf_aoi)) / 1000^2)
      #
      #   updateProgressBar(session = session, id = "prog_CV", value = 100, status = "success")
      #
      #   }) ## END observeEvent spatial analysis
      #
      #
      #
      # ## + A1 make outputs ==================================================
      #
      # ## + + Show maps ------------------------------------------------------
      # output$map_agb <- renderPlot({
      #
      #   req(rv$CV_model$sf_aoi, rv$CV_model$df_avitabile, rv$CV_model$df_santoro)
      #
      #   gr1 <- ggplot() +
      #     geom_tile(data = rv$CV_model$df_avitabile, aes(x = x, y = y, fill = agb_avitabile)) +
      #     scale_fill_viridis_c(direction = -1) +
      #     geom_sf(data = rv$CV_model$sf_aoi, fill = NA, col = "darkred", size = 1) +
      #     theme_bw() +
      #     theme(legend.key.height = unit(2, "cm")) +
      #     add_ggspatial(font = "LoraIt") +
      #     labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Avitabile et al. 2016 aboveground biomass")
      #
      #   gr2 <- ggplot() +
      #     geom_tile(data = rv$CV_model$df_santoro, aes(x = x, y = y, fill = agb_santoro)) +
      #     scale_fill_viridis_c(direction = -1) +
      #     geom_sf(data = rv$CV_model$sf_aoi, fill = NA, col = "darkred", size = 1) +
      #     theme_bw() +
      #     theme(legend.key.height = unit(2, "cm")) +
      #     add_ggspatial(font = "LoraIt") +
      #     labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Santoro et al. 2018 aboveground biomass")
      #
      #   ggpubr::ggarrange(gr1, gr2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")
      #
      # })
      #
      # ## + + Show tables ----------------------------------------------------
      # output$CV_table <- renderTable({
      #
      #   req(rv$CV_model$cv_avitabile)
      #
      #   rv$CV_model$cv_avitabile %>%
      #     bind_rows(rv$CV_model$cv_santoro) %>%
      #     bind_rows(rv$CV_model$cv_mixed)
      #
      # })
      #
      # output$area_aoi <- renderText({
      #
      #   req(rv$CV_model$area_aoi)
      #
      #   paste0("Based on the shapefile uploaded the area if interest has an area of ",  rv$CV_model$area_aoi, " sq. km.")
      #
      # })
      #
      # ## + Show results =====================================================
      #
      # # !!! Not working, using req() instead
      # # observeEvent(!is.null(rv$CV_model$cv_mixed), {
      # #
      # #   shinyjs::show("CV_to_results")
      # #
      # # })
      #
      # observe({
      #   req(rv$CV_model$cv_mixed)
      #   shinyjs::show("box_progress_to_results")
      # })
      #
      # observeEvent(input$btn_show_results, {
      #
      #   shinyjs::hide("panel_a1_progress")
      #   shinyjs::hide("box_progress_to_results")
      #   shinyjs::show("panel_a1_results")
      #   shinyjs::show("box_CV_to_params")
      #
      # })



      ##
      ## Panel A2 server ####################################################
      ##




      ##
      ## Change tab #########################################################
      ##

      observeEvent(input$btn_to_params, {
        rv$to_params <- input$btn_to_params
      })

    } ## END module server function
  )

} ## END function CV_model_server()
