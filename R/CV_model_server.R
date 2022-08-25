

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

      ## AGB map ############################################################

      ## + Choosing a dir on the computer ----
      roots = c(wd='.')

      shinyFiles::shinyDirChoose(input, 'folder', roots = roots, filetypes = c('', 'txt'), session = session)

      folder_path <- reactive({ shinyFiles::parseDirPath(roots, input$folder) })

      output$show_path <- renderText({

        if(length(folder_path()) == 0) "No folder selected" else folder_path()

        })

      ## + AGB map spatial analysis -----------------------------------------
      observeEvent(input$calc_CV, {

        ## + + Checks ----
        ## Check path works
        ## !!! TBD !!!

        ## Check file uploaded is valid
        ## !!! TBD !!!

        ## Check at lest one biomass map selected
        ## !!! TBD !!!

        ## + + Read AOI data ----

        rv$CV_model$aoi <- st_read(input$AOI)

        rv$CV_model$santoro_tile <- get_santoro_tile(rv$CV_model$aoi)

        download_agb(
          path_data = rv$file_path,
          data_name = "Avitabile_AGB_Map.zip",
          url = "http://lucid.wur.nl/storage/downloads/high-carbon-ecosystems/Avitabile_AGB_Map.zip",
          zipfile = TRUE)

        purrr::walk(rv$CV_model$santoro_tile, function(x){

          download_agb(
            path_data = rv$file_path,
            data_name = x,
            url = paste0("globbiomass.org/wp-content/uploads/GB_Maps/", x),
            zipfile = TRUE)

        })

        tt %>%




        rv$file_path <- ifelse(length(folder_path()) == 0, tempdir(), folder_path())

        download_agb(
          path_data = rv$file_path,
          data_name = "Avitabile_AGB_Map.zip",
          url = "http://lucid.wur.nl/storage/downloads/high-carbon-ecosystems/Avitabile_AGB_Map.zip",
          zipfile = TRUE)

      })





    }
  )
} ## END function home_server()
