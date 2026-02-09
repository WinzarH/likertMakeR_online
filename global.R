
###
##### globals
###

library(shiny) # what this is
library(htmltools) # render tags
library(bslib) # easier cards panels and other layout
library(bsicons) # even more icons to not use
library(shinycssloaders) # interactive and pretty
library(matrixcalc) # to test positive definite status
library(ragg) # to save output chart
library(markdown) # display text files
library(DT) # data table rendering
library(GGally) # pairs plots
library(ggplot2) # plots core functions
library(Matrix) # near PD
library(LikertMakeR)

link_github <- tags$a(shiny::icon("github"),
                      "LikertMakeR development version",
                      href = "https://winzarh.github.io/LikertMakeR/",
                      target = "_blank"
)
link_cran <- tags$a(shiny::icon("r-project"),
                    "LikertMakeR on CRAN",
                    href = "https://CRAN.R-project.org/package=LikertMakeR",
                    target = "_blank"
)
