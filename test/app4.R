#
# library(shiny)
# library(shinyFiles)
#
# ui <- shinyUI(bootstrapPage(
#   shinyDirButton('folder', 'Folder select', 'Please select a folder', FALSE),
#   textOutput("file_path")
# ))
# server <- shinyServer(function(input, output) {
#   observe({
#     shinyDirChoose(input, 'folder', roots=c(wd='.'), filetypes=c('', 'txt'))
#
#     print(input$folder)
#
#     output$file_path <- renderText({ input$folder$path })
#
#
#   })
#
# })
#
# runApp(list(
#   ui=ui,
#   server=server
# ))


ui <- shinyUI(bootstrapPage(
  shinyDirButton('files', 'File select', 'Please select a file', FALSE),
  verbatimTextOutput('rawInputValue'),
  verbatimTextOutput('filepaths')
))
server <- shinyServer(function(input, output) {
  roots = roots=c(wd='.')
  shinyDirChoose(input, 'files', roots=roots, filetypes=c('', 'txt'))
  output$rawInputValue <- renderPrint({str(input$files)})
  output$filepaths <- renderPrint({ rlang::is_empty(parseDirPath(roots, input$files)) })
})
runApp(list(

  ui=ui,
  server=server
))

