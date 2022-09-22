

## Libs
library(devtools)
library(usethis)
library(roxygen2)

## Make package
usethis::create_package("D:/github-repos/oNFI")

##Load all functions
devtools::load_all()
shiny_optimize_NFI()

## Add dependencies
## + Shiny
usethis::use_package("shiny")
usethis::use_package("shinyjs")
usethis::use_package("shinyWidgets")
usethis::use_package("shinyFiles")

## + Tidyverse
usethis::use_package("magrittr")
usethis::use_package("readr")
usethis::use_package("tidyselect")
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
#usethis::use_package("tibble")
#usethis::use_package("forcats")
usethis::use_package("purrr")
usethis::use_package("stringr")
usethis::use_package("rlang")
# usethis::use_package("tidyverse", type = "depends")

## --- Geospatial
usethis::use_package("sf")
usethis::use_package("terra")
usethis::use_package("units")

## --- Misc
usethis::use_package("ggspatial")
usethis::use_package("ggpubr")
usethis::use_package("showtext")
usethis::use_package("sysfonts")

## Convert Roxygen data and function description to .Rd files in man/
devtools::document()

## Check
devtools::check()

## Add license
usethis::use_mit_license()

## Install
devtools::install()

## Launch lesson
learnr::run_tutorial("eNFI-lesson2.1", "eNFI")


## Add github actions
usethis::use_github_action_check_standard()

## Install from github
remotes::install_github("gaelso/oNFI")

## Shiny app as package, example:
# monthFeedbackUI <- function(id) {
#   textOutput(NS(id, "feedback"))
# }
# monthFeedbackServer <- function(id, month) {
#   stopifnot(is.reactive(month))
#
#   moduleServer(id, function(input, output, session) {
#     output$feedback <- renderText({
#       if (month() == "October") {
#         "You picked a great month!"
#       } else {
#         "Eh, you could do better."
#       }
#     })
#   })
# }
#
# birthstoneUI <- function(id) {
#   p(
#     "The birthstone for ", textOutput(NS(id, "month"), inline = TRUE),
#     " is ", textOutput(NS(id, "stone"), inline = TRUE)
#   )
# }
# birthstoneServer <- function(id, month) {
#   stopifnot(is.reactive(month))
#
#   moduleServer(id, function(input, output, session) {
#     stone <- reactive(stones$stone[stones$month == month()])
#     output$month <- renderText(month())
#     output$stone <- renderText(stone())
#   })
# }
#
#
# shiny_NFI_optimization <- function(...) {
#
#   #stones <- vroom::vroom("data/birthstones.csv")
#   load(file = "data/stones.rda")
#   months <- c(
#     "January", "February", "March", "April", "May", "June",
#     "July", "August", "September", "October", "November", "December"
#   )
#
#   ui <- navbarPage(
#     "Sample app",
#     tabPanel("Pick a month",
#              selectInput("month", "What's your favourite month?", choices = months)
#     ),
#     tabPanel("Feedback", monthFeedbackUI("tab1")),
#     tabPanel("Birthstone", birthstoneUI("tab2"))
#   )
#   server <- function(input, output, session) {
#     monthFeedbackServer("tab1", reactive(input$month))
#     birthstoneServer("tab2", reactive(input$month))
#   }
#   shinyApp(ui, server, ...)
# }


