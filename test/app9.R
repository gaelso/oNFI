library(shiny)

server <- shinyServer(function(input, output) {
})

ui <- shinyUI(fluidPage(
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$']]}
                });
                </script>
                ")),

  titlePanel("Minimal application"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        h4("(Hello!)"),
        p("Inline maths: $\\alpha = \\beta$"),
        )),
    mainPanel(
      fluidRow(
        h4("Hello!"),
        p("$$\\alpha = \\beta$$")
        ))
  )
))

shinyApp(ui=ui, server=server)
