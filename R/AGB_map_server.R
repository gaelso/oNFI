

AGB_map_server <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      ## Choosing a dir on the computer
      roots = roots=c(wd='.')

      shinyDirChoose(input, 'folder', roots=roots, filetypes=c('', 'txt'), session = session)
      output$rawInputValue <- renderPrint({str(input$folder)})
      output$folder_path <- renderPrint({ parseDirPath(roots, input$folder) })

    }
  )
} ## END function home_server()
