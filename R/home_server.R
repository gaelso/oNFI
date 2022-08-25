

home_server <- function(id, rv) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      ns <- session$ns

      observeEvent(input$to_CV_model, {
        rv$to_CV_model <- input$to_CV_model
      })

    }
  )
} ## END function home_server()
