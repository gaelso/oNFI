#' CV introduction submodule server function
#'
#' @noRd
submod_CV_intro_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## Show/hide Introduction
    observeEvent(input$toggle_intro, {
      shinyjs::toggle(id = "intro1", anim = T, animType = "slide")
      shinyjs::toggle(id = "intro2", anim = T, animType = "slide")
      shinyjs::toggle(id = "intro3", anim = T, animType = "slide")
    })

  }) ## END module server function

}
