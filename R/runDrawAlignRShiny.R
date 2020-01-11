#' Launches the shiny app for DrawAlignR - User facing function.
#'
#' @return None. Runs the Shiny webapp found in inst/available-shiny-apps/DrawAlignR/app.R
#'
#' @import shiny
#' @import ggplot2
#' @import data.table
#' @import plyr
#' @import tibble
#' @import ggplot2
#' @import gridExtra
#' @import ggrepel
#' @import signal
#' @import tidyverse
#' @import crayon
#' @import pbmcapply
#' @import plotly
#' @import mstools
#' @import zoo
#' @import dbplyr
#' @import tidyr
#' @import mzR
#' @import Rcpp
#' @import DIAlignR
#' @import latticeExtra
#'
#' @source "GetXIC.r"
#' @source "getPepLibData.R"
#' @source "getChromatogramDatapoints.R"
#' @source "plot_aligned.R"
#' @source "plot_chrom_reference.R"
#' 
#' @export
runDrawAlignR <- function() {
  appDir <- system.file("shiny-script",
                      package = "DrawAlignR")

  shiny::runApp(appDir = appDir, display.mode = "normal")
  return()
}
