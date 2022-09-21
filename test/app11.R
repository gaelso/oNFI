
library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  numericInput("obs", "Observations:", 10, min = 1, max = 100),
  actionButton("action", "Click me"),
  verbatimTextOutput("value")
)
server <- function(input, output) {

  observe({

    if (is.na(input$obs)) {
      shinyjs::disable("action")
    } else {
      shinyjs::enable("action")
    }

  })

  output$value <- renderText({ input$obs })
}

shinyApp(ui, server)
