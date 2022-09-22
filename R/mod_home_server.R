#' Home module server function
#'
#' @noRd
mod_home_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$btn_to_cv, {
      rv$to_cv <- input$btn_to_cv
    })

  }) ## END module server function

}
