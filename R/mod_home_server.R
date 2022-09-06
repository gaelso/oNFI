

mod_home_server <- function(id, rv) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      ns <- session$ns

      observeEvent(input$btn_to_CV, {
        rv$to_CV <- input$btn_to_CV
      })

    }
  )
} ## END function home_server()
