

## Libs
library(devtools)
library(usethis)
library(roxygen2)

## Make package
usethis::create_package("D:/github-repos/oNFI")

##Load all functions
devtools::load_all()
shiny_optimize_NFI()

##
## Add dependencies
## + Tidyverse
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("tibble")
usethis::use_package("forcats")
usethis::use_package("purrr")
usethis::use_package("tidyverse", type = "depends")

## --- Geospatial
usethis::use_package("sf")
usethis::use_package("tmap")
usethis::use_package("units")

## --- Tutos
usethis::use_package("learnr")
usethis::use_dev_package("gradethis")
usethis::use_dev_package("eNFIrawdata")
usethis::use_package("ggspatial")
usethis::use_package("sysfonts")
usethis::use_package("showtext")
usethis::use_package("knitr")
usethis::use_package("kableExtra")

## RAW DATA
## --- Created by R scripts in data-raw/
## --- Data stored in inst/extdata/
## --- called with system.file()
## --- See: https://r-pkgs.org/data.html#data
## PACKAGE DATA
## --- Created by R-script in data-raw/
## --- Documentation created in R/
## --- Data stored in data/

## Create raw data template
usethis::use_data_raw()

## Create inst subfolders
dir.create("inst", showWarnings = F)
dir.create("inst/extdata", showWarnings = F)
dir.create("inst/tuto-helpers", showWarnings = F)
dir.create("inst/tuto-helpers/images", showWarnings = F)


## --- Create Louland spatial data

## Create inst/extdata/louland_lc90m.tif
## Create data/louland_param.Rda and R/louland_param.R
source("data-raw/create-louland.R", local = TRUE)

## Create inst/extdata/louland_lc.Rda
## Create inst/extdata/louland_admin.Rda
## Create inst/tuto-helpers/images/louland-map.png
## Create inst/tuto-helpers/images/louland-map-nolegend.png
source("data-raw/create-louland-shp.R", local = TRUE)


## --- Create exploratory crew forest inventory data

## Create data/exfi_plot.Rda and data/exfi_plot.R
## Create data/exfi_tree.Rda and data/exfi_tree.R
## Create inst/extdata/sf_exfi.Rda
source("data-raw/create-exfi.R", local = TRUE)


## Create solutions for Lesson 1
source("data-raw/solutions-lesson1.R", local = TRUE)

## Create solutions for Lesson 2.1
source("data-raw/solutions-lesson21.R", local = TRUE)


## Copy images from tuto-helpers to tutos
images <- list.files("inst/tuto-helpers/images", full.names = T)
images

## -> L1
file.copy("inst/tuto-helpers/images/louland.gif", "inst/tutorials/eNFI-lesson1/images")
file.copy("inst/tuto-helpers/images/louland-map.png", "inst/tutorials/eNFI-lesson1/images")

## -> L2
file.copy("inst/tuto-helpers/images/louland-grid10.png", "inst/tutorials/eNFI-lesson2.1/images")
file.copy("inst/tuto-helpers/images/louland-grid5.png", "inst/tutorials/eNFI-lesson2.1/images")
file.copy("inst/tuto-helpers/images/louland-grid4.png", "inst/tutorials/eNFI-lesson2.1/images")
file.copy("inst/tuto-helpers/images/louland-srs.png", "inst/tutorials/eNFI-lesson2.1/images")
file.copy("inst/tuto-helpers/images/louland-srs2.png", "inst/tutorials/eNFI-lesson2.1/images")

unlink(images)

## Check
devtools::check()

## Convert Roxygen data and function description to .Rd files in man/
devtools::document()

## Install
devtools::install()

## Launch lesson
learnr::run_tutorial("eNFI-lesson2.1", "eNFI")


## Add github actions
usethis::use_github_action_check_standard()


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


