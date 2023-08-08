<!-- badges: start -->
[![R-CMD-check](https://github.com/gaelso/oNFI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gaelso/oNFI/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# oNFI

Optimization functions for NFI sampling design for aboveground biomass.

This package mostly contains the function `shiny_optimize_NFI()` that launches a shinyapp where users are guided through several steps to calculate an optimal sampling size for forest carbon for a desired area of interest.

Optimization in this context means that the inventory plot characteristics and the measurement costs are factored in the identification of the sampling size, in addition the the well-know Cochran sampling size formula based on the variable of interest $CV$ (coefficient of variation) and allowable error (E).

The app targets National Forest Inventory but will work on any area of interest.

## Installation

    remotes::install_github("gaelso/oNFI")

## Launch the shiny app

    oNFI::shiny_optimize_NFI()

## Cloud-based version

Alternatively the app can be launched here but may not work for large areas: [](https://gaelsola.shinyapps.io/onfi/)

 
