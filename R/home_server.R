

home_server <- function(id, rv) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      ns <- session$ns

      observeEvent(input$to_AGB_map, {
        #updateTabsetPanel(session, "navbar", "AGB_map")
        rv$to_AGB_map <- input$to_AGB_map
      })

    }
  )
} ## END function home_server()
