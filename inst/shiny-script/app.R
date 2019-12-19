#' Builds the shiny webapp for DrawAlignR
#'
#' @return None. Calling this script creates the Shiny webapp
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

library(plotly)
library(shiny)
library(shinyjs)
library(ggplot2)
library(data.table)
library(plyr)
library(tibble)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(signal)
library(tidyverse)
library(crayon)
library(pbmcapply)
library(mstools)
library(zoo)
library(dbplyr)
library(tidyr)
library(mzR)
library(Rcpp)
library(DIAlignR)
library(latticeExtra)

#Setting max file size
options(shiny.maxRequestSize=10000*1024^2)

## This should be done only if the user does not have the package, otherwise it will install everytime the app is run, and will be very slow for installing large packages.
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("mzR") # Requires netcdf

## Some usefull functionality here
# install.packages("shinyjs")


### TODO in the future: Link zooming
### https://stackoverflow.com/questions/47933524/how-do-i-synchronize-the-zoom-level-of-multiple-charts-with-plotly-js


ui <- fluidPage(
  
  useShinyjs(),  # Include shinyjs
  
  titlePanel("DrawAlignR"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Chromatogram Input
      splitLayout(cellWidths = c("80%", "20%"),
                  #Select 1 or set of mzML files,
                  fileInput(inputId = "ChromatogramFile", "Choose a Chromatogram File", multiple = TRUE, accept = c(".mzML", ".sqMass")),
                  ## Reset
                  actionButton(inputId = "resetChromatogramFile", label = 'X')
      ),
      tags$style(type='text/css', "#ChromatogramFile { width:100%; margin-top: 25px;}"),
      tags$style(type='text/css', "#resetChromatogramFile { width:100%; margin-top: 25px;}"),
      
      # Library File Input
      splitLayout(cellWidths = c("80%", "20%"),
                  #Select 1 or set of mzML files,
                  fileInput(inputId = "LibraryFile", "Choose a Library File", multiple = FALSE, accept = c(".pqp")),
                  ## Reset
                  actionButton(inputId = "resetLibraryFile", label = 'X')
      ),
      tags$style(type='text/css', "#LibraryFile { width:100%; margin-top: 25px;}"),
      tags$style(type='text/css', "#resetLibraryFile { width:100%; margin-top: 25px;}"),

      # OSW File Input
      splitLayout(cellWidths = c("80%", "20%"),
                  #Select 1 or set of mzML files,
                  fileInput(inputId = "OSWFile", "Choose a OSW File", multiple = FALSE, accept = c(".osw")),
                  ## Reset
                  actionButton(inputId = "resetOSWFile", label = 'X')
      ),
      tags$style(type='text/css', "#OSWFile { width:100%; margin-top: 25px;}"),
      tags$style(type='text/css', "#resetOSWFile { width:100%; margin-top: 25px;}"),
      
      #Path to directory where mzml folder and osw folder are located. By default is set to the working directory.
      textInput(inputId = "WorkingDirectory", "Set Working Directory (Location of mzML and osw folders)", 
                value = paste((gsub('............$', '', getwd())), 'extdata', sep = '')),
      
      #Full peptide name including modifications
      selectizeInput('Mod', 'Peptide Name', choices = '', options = list(
        valueField = 'Unique Peptide string',
        labelField = 'name',
        searchField = 'name',
        options = list( ),
        create = FALSE, 
        multiple = FALSE,
        selected = NULL
        
      )),
       
      #Charge of desired peptide (Specific charge must be in data set)
      selectizeInput('Charge', 'Peptide Charge', choices = '', options = list(
        valueField = 'Unique Charge',
        labelField = 'name',
        searchField = 'name',
        options = list( ),
        create = FALSE, 
        multiple = FALSE,
        selected = NULL
        
      )),
      
      #Number of plots to display
      sliderInput("n", "Number of Plots", value=2, min=1, max=10),
      
      #Off by default. Enabled if DIAlignR should be run and aligned chromatograms should be plotted.
      checkboxInput(inputId = "Align", "Plot Aligned", value = FALSE, width = NULL),
      
      #Name of the reference run if performing multiple pairwise alignments. Not required.
      textInput(inputId = "Reference", "Select Reference Run for Alignment", value = "chludwig_K150309_013_SW_0"),
      
    ),
    
    mainPanel(
      uiOutput("plots")
    )
  )
)

server <- function(input, output, session) {
  
  ## Clear Chromatogram File input
  observeEvent( input$resetChromatogramFile, {
    shinyjs::reset('ChromatogramFile')
  } )
  
  ## Clear Library File input
  observeEvent( input$resetLibraryFile, {
    shinyjs::reset('LibraryFile')
  } )
  
  ## Clear OSW File input
  observeEvent( input$resetOSWFile, {
    shinyjs::reset('OSWFile')
  } )
  
  ## reactive values object to store some re-usable stuff
  values <- reactiveValues()

  observeEvent( input$LibraryFile, {
    ## Load Librady file into data frame
    lib_df <- mstools::getPepLibData_( input$LibraryFile$datapath ) 
    ## Store library data.frame into a re-usable  object
    values$lib_df <- lib_df
    ## Get list of unique modified peptides
    uni_peptide_list <- as.list(unique( lib_df$MODIFIED_SEQUENCE )) 
    ## Update slection list with unique peptides
    updateSelectizeInput( session, inputId = 'Mod', choices = uni_peptide_list  )
  } )
 
  ## Load OSW file
  observeEvent( input$OSWFile, {
    ## Load OSW file
    osw_df <- mstools::getOSWData_( oswfile=input$OSWFile, decoy_filter = TRUE, ms2_score = TRUE, ipf_score = TRUE )
    print(dim(osw_df))
    print(unique(osw_df$run_id))
  })
  
  ## Observe Peptide Selection
  observeEvent( input$Mod, {
    ## Check if there is no lib df returned from intial library load
    if ( !is.null( values$lib_df ) ){
      ## get unique charge state for current peptide selection
     values$lib_df %>%
      dplyr::filter( MODIFIED_SEQUENCE==input$Mod ) %>%
      dplyr::select( PRECURSOR_CHARGE ) %>%
      unique() %>%
      as.list() -> unique_charges
    ## Update charge selection to charges available for currently selected peptide sequence.  
    updateSelectizeInput( session, inputId = 'Charge', choices = unique_charges )
    }
  } )
  
  #Generate set of variable plots
  
  output$plots <- renderUI({
    plot_output_list <- lapply(1:input$n, function(i) {
      plotname <- paste("plot", i, sep="")
      plotlyOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })
  
  #Generate all plots. Max plots set to 10
  
  for (i in 1:10) {
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      
      
      output[[plotname]] <- renderPlotly({
        
        
        #If alignment is disabled, generate standard chromatogram plot.
        
        if (is.null(input$ChromatogramFile)){
          stop()
        }
        else if (is.null(input$LibraryFile)){
          stop()
        }
        else if (is.null(input$Mod)){
          stop()
        }
        
        if (!(input$Align)){
          chrom_input <- input$ChromatogramFile[[my_i, 'datapath']]
          lib_input <- input$LibraryFile
          peptide <- input$Mod
          lib <- getPepLibData_(lib_input$datapath, peptide_id = '')
          g.out <- getXIC(graphic_obj = ggplot(), chromatogram_file = chrom_input,
                          df_lib = lib, mod = peptide, Isoform_Target_Charge = input$Charge)
          plotly::ggplotly(g.out$graphic_obj, dynamicTicks = TRUE)
        }
        
        #If alignment is enabled.
        
        else {
          
          #Ensuring at least two runs selected, not conducting alignment against same run
          
          if (!(input$Reference == gsub('...........$', '', input$ChromatogramFile[[my_i, 'name']]))){
            
            if ( F ){
              my_i <- 1
              dataPath <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata"
              analytes <- "ANSS(UniMod:21)PTTNIDHLK(UniMod:259)_2"
              input <- list()
              input$ChromatogramFile <- data.frame(name=c("chludwig_K150309_012_SW_1_1.chrom.mzML", "chludwig_K150309_013_SW_0_.chrom.mzML", "chludwig_K150309_010_SW_1_2.chrom.mzML"))
              input$Reference <- "chludwig_K150309_013_SW_0"
            }
            tictoc::tic("DIAlignR Elapsed Time:...")
            dataPath <- input$WorkingDirectory
            analytes <- paste(input$Mod, "_", toString(input$Charge), sep="")
            runs <- c(input$Reference, gsub('...........$', '', input$ChromatogramFile[my_i, 'name']))
            print(runs)
            AlignObjOutput <- getAlignObjs(analytes, runs, dataPath)
            k <- plotAlignedAnalytes(AlignObjOutput, DrawAlignR = T, annotatePeak = T)
            plotly::ggplotly(k$prefU, dynamicTicks = TRUE)
            tictoc::toc()
          }
          
          #If all possible pairwise alignments conducted, plotting the reference run
          
          else {
            chrom_input <- input$ChromatogramFile[[my_i, 'datapath']]
            lib_input <- input$LibraryFile
            peptide <- input$Mod
            lib <- getPepLibData_(lib_input$datapath, peptide_id = '')
            g.out <- getXIC(graphic_obj = ggplot(), chromatogram_file = chrom_input,
                            df_lib = lib, mod = peptide, Isoform_Target_Charge = input$Charge)
            plotly::ggplotly(g.out$graphic_obj, dynamicTicks = TRUE)
          }
        }
      })
    })
  }
}


shinyApp(ui = ui, server = server)

