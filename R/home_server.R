

home_server <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      observeEvent(input$to_AGB_map,
                   updateTabsetPanel(session, "navbar", "AGB_map")
      )

    }
  )
} ## END function home_server()
