

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

      ## Choosing a dir on the computer
      roots = c(wd='.')

      shinyFiles::shinyDirChoose(input, 'folder', roots = roots, filetypes = c('', 'txt'), session = session)

      folder_path <- reactive({ shinyFiles::parseDirPath(roots, input$folder) })

      output$show_path <- renderText({

        if(length(folder_path()) == 0) "No folder selected" else folder_path()

        })


    }
  )
} ## END function home_server()
