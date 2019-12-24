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
      uiOutput("plots"),
      
      absolutePanel( id='log-pannel', draggable = TRUE,
      ## Log
        fluidRow(
          
          # log output
          textOutput( 'log' )
        )
      ),
      
    )
 
    
  )
)

server <- function(input, output, session) {
  
  ## reactive values object to store some re-usable stuff
  values <- reactiveValues()
  
  ## Clear Chromatogram File input
  observeEvent( input$resetChromatogramFile, {
    shinyjs::reset('ChromatogramFile')
    
    ##*******************************
    ## Pre-Load mzML Files
    ##*******************************
    
    ## Get filenames from osw files and check if names are consistent between osw and mzML files. ######
    filenames <- DIAlignR::getRunNames( input$WorkingDirectory, oswMerged=TRUE)
    runs <- c(input$Reference, gsub('...........$', '', input$ChromatogramFile[,'name']))
    filenames <- filenames[filenames$runs %in% runs,]
    tictoc::tic('Pre-Loading mzML Chromatogram Files onto disk')
    masterMzExperiment <- list()
    for ( chromatogram_input_index_num in seq(1, length(filenames$runs)) ){
      run <- rownames(filenames)[ chromatogram_input_index_num ]
      message(sprintf("Working on run %s", run))
      ## Get path for current chromatogram file
      chromatogram_file_i <- input$ChromatogramFile$datapath[[chromatogram_input_index_num]]
      # Create an mzR object that stores all header information, and use ProteoWizard api to access data from MzML file
      mz <- mzR::openMSfile(chromatogram_file_i, backend = "pwiz", verbose = T)
      ## Get table of chromatogram incidces and respective transtion ids
      chromHead <- mzR::chromatogramHeader(mz)
      ## Store run id and mz object into master list
      masterMzExperiment[[run]]$run <- run
      mzExperiment <- list()
      mzExperiment$mz <- mz
      mzExperiment$chromHead <- chromHead
      masterMzExperiment[[run]]$mzExperiment <- mzExperiment
    }
    tictoc::toc()
    
    values$masterMzExperiment <- masterMzExperiment
    
  } )
  
  ## Clear Library File input
  observeEvent( input$resetLibraryFile, {
    shinyjs::reset('LibraryFile')
  } )
  
  ## Clear OSW File input
  observeEvent( input$resetOSWFile, {
    shinyjs::reset('OSWFile')
  } )
  
  

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
          chrom_input <- input$ChromatogramFile$datapath[[my_i]]
          lib_input <- input$LibraryFile
          osw_input <- input$OSWFile
          peptide <- input$Mod
          modification_labels <- regmatches(peptide, gregexpr("\\(.*?\\)", peptide))[[1]]
          naked_peptide <-  gsub( paste(gsub('\\)','\\\\)',gsub('\\(','\\\\(',modification_labels)), collapse = '|'), '', peptide )
          # lib <- getPepLibData_(lib_input$datapath, peptide_id = '')
          # log <- capture.output(
          # g.out <- getXIC(graphic_obj = ggplot(), chromatogram_file = chrom_input,
                          # df_lib = lib, mod = peptide, Isoform_Target_Charge = input$Charge)
          
          
          out.plot.h <- mstools::XICMasterPlotFcn_( naked_peptide, 
                                           peptide,
                                           chrom_input,  lib_input, osw_input, 
                                           plotPrecursor=T,
                                           plotIntersectingDetecting=T,
                                           plotUniqueDetecting=F,
                                           plotIdentifying=T,
                                           plotIdentifying.Unique=T,
                                           plotIdentifying.Shared=F,
                                           plotIdentifying.Against=F,
                                           doFacetZoom=F,
                                           # FacetFcnCall = NULL,
                                           doPlot=T,
                                           # RT_padding=90000,
                                           Charge_State=input$Charge,
                                           N_sample = 1,
                                           # idx_draw_these = c(8),
                                           # store_plots_subdir = paste('/XIC_plots/TP/U', pool, '/', sep=''),
                                           # store_plots_subdir = '/Presentation/Figures/IPF_Scoring_With_no_MS 1MS2_precursor_Scoring/6_highest_Concentrations/', 
                                           # store_plots_subdir = '/Presentation/Figures/Default_IPF_with_5_pkgrprank_transition_scoring/',
                                           # store_plots_subdir = '/Presentation/Figures/IPF_Scoring_With_no_MS1MS2_precursor_Scoring/', 
                                           printPlot=T,
                                           use_top_trans_pep=F,
                                           show_n_transitions=6,
                                           show_all_pkgrprnk=F,
                                           # show_manual_annotation = manual_annotation_coordinates,
                                           show_legend=T,
                                           verbosity = 0 )
          
          
          # )
          # output$log <- renderText( paste(log, collapse = '\n') )
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
              input$WorkingDirectory <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata"
              input$Mod <- "ANSS(UniMod:21)PTTNIDHLK(UniMod:259)"
              input$Charge <- 2
              input$ChromatogramFile$datapath <- '/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/mzml/chludwig_K150309_013_SW_0.chrom.mzML' 
              input$ChromatogramFile <- data.frame(name=c("chludwig_K150309_013_SW_0.chrom.mzML", "chludwig_K150309_010_SW_1_2.chrom.mzML"),
                                                   datapath=c('/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/mzml/chludwig_K150309_013_SW_0.chrom.mzML' ,
                                                              '/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/mzml/chludwig_K150309_010_SW_1_2.chrom.mzML'),
                                                              stringsAsFactors=F)
              input$Reference <- "chludwig_K150309_013_SW_0"
              input$LibraryFile$datapath <- '/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/pqp/psgs_phospho_optimized_decoys.pqp'
            }
            tictoc::tic("DIAlignR Elapsed Time:...")
            dataPath <- input$WorkingDirectory
            analytes <- paste(input$Mod, "_", toString(input$Charge), sep="")
            runs <- c(input$Reference, gsub('...........$', '', input$ChromatogramFile[my_i, 'name']))
            masterMzExperiment <- values$masterMzExperiment
            print( str(values) )
            AlignObjOutput <- DIAlignR::getAlignObjs(analytes = analytes, runs = runs, dataPath = dataPath, masterMzExperiment=masterMzExperiment)
            tictoc::toc()
            k <- plotAlignedAnalytes(AlignObjOutput, DrawAlignR = T, annotatePeak = T)
            plotly::ggplotly(k$prefU, dynamicTicks = TRUE)
            
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

