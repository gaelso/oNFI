

mod_home_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$btn_to_CV, {
      rv$to_CV <- input$btn_to_CV
    })

  }) ## END module server function

}
