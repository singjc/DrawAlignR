#' Builds the shiny webapp for DrawAlignR
#'
#' @return None. Calling this script creates the Shiny webapp
#'
#' @import shiny
#' @import shinyjs
#' @import shinyFiles 
#' @import data.table
#' @import plyr
#' @import dplyr
#' @import dbplyr
#' @import tidyr
#' @import tibble
#' @import ggplot2
#' @import gridExtra
#' @import ggrepel
#' @import latticeExtra
#' @import plotly
#' @import signal
#' @import tidyverse
#' @import crayon
#' @import pbmcapply
#' @import zoo
#' @import mzR
#' @import Rcpp
#' @import DIAlignR
#' @import mstools
#'
#' @source "GetXIC.r"
#' @source "getPepLibData.R"
#' @source "getChromatogramDatapoints.R"
#' @source "plot_aligned.R"
#' @source "plot_chrom_reference.R"

library(plotly)
library(shiny)
library(shinyjs)
library(shinyFiles)
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
options(shiny.maxRequestSize=1000000*1024^2)

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
  
titlePanel( title=div( img(src="DIAlignR-logo.jpg", width = 80, height = 80, align="top" ), ( HTML(sprintf("DrawAlignR <h6 style='display:inline'>Ver: %s</h6>", packageVersion("DrawAlignR"))) ) ) ),
  sidebarLayout(
    
    sidebarPanel(
      
      tabsetPanel(
        tabPanel( title = "General Settings",
                  
                  # Chromatogram Input
                  splitLayout(cellWidths = c("80%", "20%"),
                              #Select 1 or set of mzML files,
                              fileInput(inputId = "ChromatogramFile", "Choose a Chromatogram File(s)", multiple = TRUE, accept = c(".mzML", ".sqMass"), buttonLabel = icon("folder")  ),
                              ## Reset
                              actionButton(inputId = "resetChromatogramFile", label = 'X')
                  ),
                  tags$style(type='text/css', "#ChromatogramFile { width:100%; margin-top: 25px;}"),
                  tags$style(type='text/css', "#resetChromatogramFile { width:100%; margin-top: 25px;}"),
                  
                  # Library File Input
                  splitLayout(cellWidths = c("80%", "20%"),
                              #Select 1 or set of mzML files,
                              fileInput(inputId = "LibraryFile", "Choose a Library File", multiple = FALSE, accept = c(".pqp"), buttonLabel = icon("folder") ),
                              ## Reset
                              actionButton(inputId = "resetLibraryFile", label = 'X')
                  ),
                  tags$style(type='text/css', "#LibraryFile { width:100%; margin-top: 25px;}"),
                  tags$style(type='text/css', "#resetLibraryFile { width:100%; margin-top: 25px;}"),
                  
                  # OSW File Input
                  splitLayout(cellWidths = c("80%", "20%"),
                              #Select 1 or set of mzML files,
                              fileInput(inputId = "OSWFile", "Choose a OSW File", multiple = FALSE, accept = c(".osw"), buttonLabel = icon("folder") ),
                              ## Reset
                              actionButton(inputId = "resetOSWFile", label = 'X')
                  ),
                  tags$style(type='text/css', "#OSWFile { width:100%; margin-top: 25px;}"),
                  tags$style(type='text/css', "#resetOSWFile { width:100%; margin-top: 25px;}"),
                  
                  # Path to directory where mzml folder and osw folder are located. By default is set to the working directory.
                  splitLayout(cellWidths = c("17%", "83%"),
                              ## GUI directroy selector
                              shinyFiles::shinyDirButton( id = "interactiveWorkingDirectory", label = "",  title = "Set Working Directory (Location of mzML and osw folders)", icon = icon("folder") ),
                              ## Text box for user to manually input working data path
                              textInput(inputId = "WorkingDirectory", "Set Working Directory (Location of mzML and osw folders)",
                                        value = paste((gsub('............$', '', getwd())), 'extdata', sep = ''))
                  ),
                  tags$style(type='text/css', "#interactiveWorkingDirectory { width:100%; margin-top: 50px;}"),
                  tags$style(type='text/css', "#WorkingDirectory { width:100%; margin-top: 25px;}"),
                  
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
                  sliderInput("n", "Number of Plots", value=1, min=1, max=10),
                  
                  #Off by default. Enabled if DIAlignR should be run and aligned chromatograms should be plotted.
                  checkboxInput(inputId = "Align", "Plot Aligned", value = FALSE, width = NULL),
                  
                  #Name of the reference run if performing multiple pairwise alignments. Not required.
                  # textInput(inputId = "Reference", "Select Reference Run for Alignment", value = "chludwig_K150309_013_SW_0"),
                  selectizeInput('Reference', 'Select Reference Run for Alignment', choices = '', options = list(
                    valueField = 'Run Name',
                    labelField = 'name',
                    searchField = 'name',
                    options = list( ),
                    create = FALSE, 
                    multiple = FALSE,
                    selected = NULL
                  )),
                  
                  #Charge of desired peptide (Specific charge must be in data set)
                  selectizeInput('Experiment', 'Experiment to Align', choices = '', options = list(
                    valueField = 'Run Name',
                    labelField = 'name',
                    searchField = 'name',
                    options = list( ),
                    create = FALSE, 
                    multiple = FALSE,
                    selected = NULL
                    
                  )),
                  
                  #Plots to show
                  checkboxInput("ref", "Reference Plot", value = T),
                  checkboxInput("exp", "Experiment Plot", value = F),
                  checkboxInput("expAligned", "Experiment Aligned Plot", value = F)
                  
                  
                  
                  ), # End of tabPanel 1
        tabPanel( title = "Alignment Settings",
                  
                  ##***********************************************
                  ##    Alignment Parameters
                  ##***********************************************
                  
                  ## identifying
                  checkboxInput('analyteInGroupLabel', 'Use Analyte Group Label', value = FALSE),
                  
                  ## identifying
                  checkboxInput('identifying', 'Include Identiyfying Transitions', value = FALSE),
                  
                  ## oswMerged
                  checkboxInput('oswMerged', 'Merged OSW File', value = TRUE),
                  
                  ## nameCutPattern
                  textInput("nameCutPattern", "REGEX string for mzML filename", value = "(.*)(/)(.*)"),
                  
                  ## SgolayFiltOrd
                  numericInput("maxFdrQuery", "OSW Extraction m-score threshold", value=0.05, min = NA, max = NA, step = NA),
                  
                  ## maxFdrLoess
                  numericInput("maxFdrLoess", "Feature m-score threshold for LOESS fit", value=0.01, min = NA, max = NA, step = NA),
                  
                  ## analyteFDR
                  numericInput("analyteFDR", "Analyte m-score threshold", value=1, min = 0, max = 1, step = NA),
                  
                  ## spanvalue
                  numericInput("spanvalue", "Span Value for LOESS fit", value=0.1, min = NA, max = NA, step = NA),
                  
                  ## normalization
                  selectizeInput('normalization', 'Normalization', selected = 'mean', choices = c('mean', 'l2'), 
                                 options = list(
                                   valueField = 'normalization',
                                   labelField = 'name',
                                   searchField = 'name',
                                   options = list( ),
                                   create = FALSE, 
                                   multiple = FALSE,
                                   selected = NULL
                                 )
                  ),
                  
                  ## simMeasure
                  selectizeInput('simMeasure', 'Similarity Measure', selected = 'dotProductMasked', choices = c('dotProduct', 'cosineAngle', 'cosine2Angle', 'dotProductMasked', 'euclideanDist', 'covariance', 'correlation'), 
                                 options = list(
                    valueField = 'simMeasure',
                    labelField = 'name',
                    searchField = 'name',
                    options = list( ),
                    create = FALSE, 
                    multiple = FALSE,
                    selected = NULL
                    )
                  ),
                  
                  ## cosAngleThresh
                  numericInput("cosAngleThresh", "Cosine Angle Threshold", value=0.3, min = NA, max = NA, step = NA),
                  
                  ## dotProdThresh
                  numericInput("dotProdThresh", "dot-product Threshold", value=0.96, min = NA, max = NA, step = NA),
                  
                  ## XICfilter
                  selectizeInput('XICfilter', 'XIC smoothing', selected = 'sgolay', choices = c('sgolay', 'none'), 
                                 options = list(
                                   valueField = 'XICfilter',
                                   labelField = 'name',
                                   searchField = 'name',
                                   options = list( ),
                                   create = FALSE, 
                                   multiple = FALSE,
                                   selected = NULL
                                 )
                  ),
                  
                  ## SgolayFiltOrd
                  numericInput("SgolayFiltOrd", "Sgolay Poly Order", value=4, min = NA, max = NA, step = NA),
                  
                  ## SgolayFiltLen
                  numericInput("SgolayFiltLen", "Sgolay Frame Length", value=9, min = NA, max = NA, step = NA),
                  
                  ## goFactor
                  numericInput("goFactor", "Initial Gap Penalty", value=0.125, min = NA, max = NA, step = NA),
                  
                  ## geFactor
                  numericInput("geFactor", "Subsequent Gap Penalty", value=40, min = NA, max = NA, step = NA),
                  
                  ## OverlapAlignment
                  checkboxInput('OverlapAlignment', 'Overlap Alignment', value = TRUE),
                  
                  ## gapQuantile
                  numericInput("gapQuantile", "Gap Quantile", value=0.5, min = 0, max = 1, step = NA),
                  
                  ## hardConstrain
                  checkboxInput('hardConstrain', 'Hard Constrain', value = FALSE),
                  
                  ## samples4gradient
                  numericInput("samples4gradient", "Mask Penalty", value=100, min = NA, max = NA, step = NA),
                  
                  ## samplingTime
                  numericInput("samplingTime", "Chromatogram Datapoints Sampling Time", value=3.4, min = NA, max = NA, step = NA),
                  
                  ## RSEdistFactor
                  numericInput("RSEdistFactor", "RSE Distance Factor", value=3.5, min = NA, max = NA, step = NA)
                  
                  
                  
                  ) # End of tabPanel 2
      ) # End of tabsetPanel
      
      
      
     
      
      
    ), # End of sidebarPanel
    
    
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
    
  } )
  
  ## Clear Library File input
  observeEvent( input$resetLibraryFile, {
    shinyjs::reset('LibraryFile')
  } )
  
  ## Clear OSW File input
  observeEvent( input$resetOSWFile, {
    shinyjs::reset('OSWFile')
  } )
  
  ## Observe input chromatogramfile  
  observeEvent( input$ChromatogramFile, {
    ## Get File Extension Type
    fileType <- gsub( '.*\\.', '', input$ChromatogramFile$name)
    print(input$ChromatogramFile$name)
    print(fileType)
    if ( fileType=='mzML' | fileType=='mzML.gz'){
      ##*******************************
      ## Pre-Load mzML Files
      ##*******************************
      
      ## Get filenames from osw files and check if names are consistent between osw and mzML files. ######
      filenames <- DIAlignR::getRunNames( input$WorkingDirectory, oswMerged=TRUE)
      runs <- c(input$Reference, gsub('...........$', '', input$ChromatogramFile[,'name']))
      filenames <- filenames[filenames$runs %in% runs,]
      tictoc::tic('Pre-Loading mzML Chromatogram Files onto disk')
      mzPntrs <- list()
      for ( chromatogram_input_index_num in seq(1, length(filenames$runs)) ){
        run <- rownames(filenames)[ chromatogram_input_index_num ]
        message(sprintf("Cacheing mzML for %s of %s runs", run, length(filenames$runs)))
        ## Get path for current chromatogram file
        chromatogram_file_i <- input$ChromatogramFile$datapath[[chromatogram_input_index_num]]
        # Create an mzR object that stores all header information, and use ProteoWizard api to access data from MzML file
        mz <- mzR::openMSfile(chromatogram_file_i, backend = "pwiz", verbose = T)
        ## Get table of chromatogram incidces and respective transtion ids
        chromHead <- mzR::chromatogramHeader(mz)
        ## Store run id and mz object into master list
        mzPntrs[[run]] <- list()
        mzPntrs[[run]]$mz <- mz
        mzPntrs[[run]]$chromHead <- chromHead
      }
      tictoc::toc()
      
      ## Store mzPntrs container
      values$mzPntrs <- mzPntrs
      ## Store chromatogram file run names
      values$chromnames <- gsub("\\.chrom\\.mzML$|\\.chrom\\.sqMass$", "", input$ChromatogramFile$name)
      ## Update Reference list
      updateSelectizeInput( session, inputId = "Reference", choices = as.list(values$chromnames))
      ## Update Experiment list with reverse order
      updateSelectizeInput( session, inputId = "Experiment", choices = as.list(rev(values$chromnames)))
    }
  })
  
  ## Working Directory
  shinyDirChoose(input, 'interactiveWorkingDirectory', roots = c( `Working Directory` =  "../", home = "~", root = "/" ), defaultRoot = 'Working Directory', defaultPath = '/' )
  ### Create a reactive object to store working directory
  dir <- reactive(input$interactiveWorkingDirectory)
  
  global <- reactiveValues(datapath = getwd())
  
  values$WorkingDirectory <- renderText({  
    global$datapath
  })  
  ## Observe interactive set working directory button
  observeEvent( input$interactiveWorkingDirectory, {
    if ( class(dir())[1]=='list' ){
      ## Get root directory based on used choice, working directory, home or root
      if ( dir()$root=='Working Directory' ){
        root_node <- dirname(getwd())
      } else if ( dir()$root == 'home' ) {
        root_node <- "~"
      } else {
        root_node <- .Platform$file.sep
      }
      ## Get full working directroy of user selected directory
      global$datapath <- paste( root_node, file.path( paste( unlist(dir()$path[-1]), collapse = .Platform$file.sep ) ), sep = .Platform$file.sep )
      ## Update Working Directory Text Box
      updateTextInput( session = session, inputId = 'WorkingDirectory', value = global$datapath  )
    }
  })
  
  observeEvent( {
    input$LibraryFile
    input$OSWFile
    input$WorkingDirectory
    input$interactiveWorkingDirectory
  }
  , {
    ## If Library File is supplied, file modification list with peptide values
    if( !is.null(input$LibraryFile) ) {
      ## Load Librady file into data frame
      lib_df <- mstools::getPepLibData_( input$LibraryFile$datapath ) 
      ## Store library data.frame into a re-usable  object
      values$lib_df <- lib_df
      ## Get list of unique modified peptides
      uni_peptide_list <- as.list(unique( lib_df$MODIFIED_SEQUENCE )) 
      ## Update slection list with unique peptides
      updateSelectizeInput( session, inputId = 'Mod', choices = uni_peptide_list  )
    } else {
      # If an explicit path to an oswfile is supplied
      if( !is.null(input$OSWFile) ){
        osw_df <- mstools::getOSWData_( input$OSWFile$datapath, decoy_filter = TRUE, ms2_score = TRUE, ipf_score = FALSE ) 
        values$osw_df <- osw_df
        ## Get list of unique modified peptides
        uni_peptide_list <- as.list(unique( osw_df$FullPeptideName ) )
        ## Update selection list with unique peptides
        updateSelectizeInput( session, inputId = 'Mod', choices = uni_peptide_list  )
      }  else if ( !is.null(input$WorkingDirectory) ) {
        items_in_wd <- list.files( input$WorkingDirectory )
        if( "osw" %in% items_in_wd ){
          files_in_osw_dir <- list.files( paste(input$WorkingDirectory,'osw/',sep='/'), pattern = "*osw$" ,full.names = T )
          if ( length(files_in_osw_dir) > 1 ){
            warning( sprintf("There were %s osw files found, taking first file!!")) # TODO: If user uses non merged osw file?
            files_in_osw_dir <- files_in_osw_dir[1]
          }
          osw_df <- mstools::getOSWData_( files_in_osw_dir, decoy_filter = TRUE, ms2_score = TRUE, ipf_score = FALSE )
          values$osw_df <- osw_df
          uni_peptide_list <- as.list( unique( osw_df$FullPeptideName ) )
          ## Update slection list with unique peptides
          updateSelectizeInput( session, inputId = 'Mod', choices = uni_peptide_list  )
        }
      } else {
        warning("There was no library file, osw file or working directory supplied!.")
      }
      
    }
    
    
  } )
  
  ## Load OSW file
  observeEvent( input$OSWFile, {
    ## Load OSW file
    osw_df <- mstools::getOSWData_( oswfile=input$OSWFile$datapath, decoy_filter = TRUE, ms2_score = TRUE, ipf_score =  FALSE)
    values$osw_df <- osw_df
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
      names(unique_charges) <- unique_charges
      ## Update charge selection to charges available for currently selected peptide sequence.  
      updateSelectizeInput( session, inputId = 'Charge', choices = unique_charges )
    } else if ( !is.null(values$osw_df) ) {
      values$osw_df %>%
        dplyr::filter( FullPeptideName==input$Mod ) %>%
        dplyr::select( Charge ) %>%
        unique() %>%
        as.list() -> unique_charges
      names(unique_charges) <- unique_charges
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
  
  observeEvent( 
    {
      input$Mod
      input$Align 
    }, {
      if ( !(input$Align) ){
        cat("Alignment option was not selected\n")
        #Generate all plots. Max plots set to 10
        
        for (i in 1:10) {
          local({
            my_i <- i
            plotname <- paste("plot", my_i, sep="")
            
            output[[plotname]] <- renderPlotly({
              
              
              #If alignment is disabled, generate standard chromatogram plot.
              
              if (is.null(input$ChromatogramFile)){
                stop('A Chromgatogram file was not supplied')
              }
              # else if (is.null(input$LibraryFile)){
              #   stop("A Library File was not supplied")
              # }
              else if (is.null(input$Mod)){
                stop("There was no peptide found")
              }
              
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
              
            }) # End renderPlotly
          }) # End local
        } # End For
        
        
      } else {
        cat('Alignment Option was selected\n')
        #Ensuring at least two runs selected, not conducting alignment against same run
        
        if (!(input$Reference == gsub('...........$', '', input$Experiment ))){
          
          if ( F ){
            my_i <- 2
            values <- list()
            dataPath <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Synthetic_Dilution_Phosphoproteomics/"
            analytes <- "ANSS(UniMod:21)PTTNIDHLK(UniMod:259)_2"
            input <- list()
            input$WorkingDirectory <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Synthetic_Dilution_Phosphoproteomics/"
            input$Mod <- "ANSS(UniMod:21)PTTNIDHLK(UniMod:259)"
            input$Charge <- 2
            # input$ChromatogramFile$datapath <- '/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/mzml/chludwig_K150309_013_SW_0.chrom.mzML' 
            input$ChromatogramFile <- data.frame(name=c("chludwig_K150309_013_SW_0.chrom.mzML", "chludwig_K150309_012_SW_1_1.chrom.mzML"),
                                                 datapath=c('/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Synthetic_Dilution_Phosphoproteomics/mzml/chludwig_K150309_013_SW_0.chrom.mzML' ,
                                                            '/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Synthetic_Dilution_Phosphoproteomics/mzml/chludwig_K150309_012_SW_1_1.chrom.mzML'),
                                                 stringsAsFactors=F)
            input$Reference <- "chludwig_K150309_013_SW_0"
            input$Experiment <- "chludwig_K150309_012_SW_1_1"
            input$LibraryFile$datapath <- '/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Synthetic_Dilution_Phosphoproteomics/pqp/psgs_phospho_optimized_decoys.pqp'
            input$OSWFile$datapath <- '/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Synthetic_Dilution_Phosphoproteomics/osw/merged.merged.osw'     
            input$analyteInGroupLabel=FALSE; input$identifying=FALSE; 
            input$oswMerged=TRUE; input$nameCutPattern="(.*)(/)(.*)";
            input$maxFdrQuery=0.05; input$maxFdrLoess=0.01; input$analyteFDR=1; 
            input$spanvalue=0.1;  input$normalization="mean"; input$simMeasure="dotProductMasked";
            input$XICfilter="sgolay"; input$SgolayFiltOrd=4; input$SgolayFiltLen=9;
            input$goFactor=0.125; input$geFactor=40; input$cosAngleThresh=0.3; input$OverlapAlignment=TRUE;
            input$dotProdThresh=0.96; input$gapQuantile=0.5; input$hardConstrain=FALSE; 
            input$samples4gradient=100;  input$samplingTime=3.4;  input$RSEdistFactor=3.5
            
            ## Spyogenes
            values <- list()
            dataPAth <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Spyogenes/"
            analytes <- "GEANVELTPELAFK_2"
            input <- list()
            input$WorkingDirectory <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Spyogenes/"
            input$Mod <- "GEANVELTPELAFK"
            input$Charge <- 2
            # input$ChromatogramFile$datapath <- '/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/mzml/chludwig_K150309_013_SW_0.chrom.mzML' 
            input$ChromatogramFile <- data.frame(name=c("hroest_K120808_Strep10%PlasmaBiolRepl1_R03_SW_filt.chrom.mzML", "hroest_K120809_Strep10%PlasmaBiolRepl2_R04_SW_filt.chrom.mzML"),
                                                 datapath=c('/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Spyogenes/mzml/hroest_K120808_Strep10%PlasmaBiolRepl1_R03_SW_filt.chrom.mzML' ,
                                                            '/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Spyogenes/mzml/hroest_K120809_Strep10%PlasmaBiolRepl2_R04_SW_filt.chrom.mzML'),
                                                 stringsAsFactors=F)
            input$Experiment <- "hroest_K120808_Strep10%PlasmaBiolRepl1_R03_SW_filt"
            input$Reference <- "hroest_K120809_Strep10%PlasmaBiolRepl2_R04_SW_filt"
            input$LibraryFile$datapath <- NULL
            input$OSWFile$datapath <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Spyogenes/osw/merged.osw"
            
            
          }
          tryCatch({  
          tictoc::tic("DIAlignR Elapsed Time")
          dataPath <- input$WorkingDirectory
          analytes <- paste(input$Mod, "_", toString(input$Charge), sep="")
          runs <- c(input$Reference, input$Experiment)
          cat( sprintf( "Reference: %s\nExperiment: %s\n", input$Reference, input$Experiment))
          cat( sprintf( "Runs: %s\n", runs ) )
          mzPntrs <- values$mzPntrs
          print(input$spanvalue)
          AlignObjOutput <- DIAlignR::getAlignObjs(analytes = analytes, runs = runs, dataPath = dataPath, refRun = input$Reference, 
                                                   analyteInGroupLabel = input$analyteInGroupLabel, identifying = input$identifying, 
                                                   oswMerged = input$oswMerged, nameCutPattern = input$nameCutPattern,
                                                   maxFdrQuery = input$maxFdrQuery, maxFdrLoess = input$maxFdrLoess, analyteFDR = input$analyteFDR, 
                                                   spanvalue = input$spanvalue,  normalization = input$normalization, simMeasure = input$simMeasure,
                                                   XICfilter = input$XICfilter, SgolayFiltOrd = input$SgolayFiltOrd, SgolayFiltLen = input$SgolayFiltLen,
                                                   goFactor = input$goFactor, geFactor = input$geFactor, cosAngleThresh = input$cosAngleThresh, OverlapAlignment = input$OverlapAlignment,
                                                   dotProdThresh = input$dotProdThresh, gapQuantile = input$gapQuantile, hardConstrain = input$hardConstrain, 
                                                   samples4gradient = input$samples4gradient,  samplingTime = input$samplingTime,  RSEdistFactor = input$RSEdistFactor, 
                                                   objType = "light", mzPntrs = mzPntrs)
          tictoc::toc()
          
          ## Generate Plot
          k <- DIAlignR::plotAlignedAnalytes(AlignObjOutput, DrawAlignR = T, annotatePeak = T)
          values$plot_i <- 1
          observeEvent( input$ref, {

            if (input$ref) { 
              if ( values$plot_i != input$n ){
                updateSliderInput(session, 'n', value=values$plot_i)
              }
              output[[ paste0('plot', values$plot_i) ]] <- renderPlotly({
               x <- 1+2 
                pt1 <- plotly::ggplotly(k$prefU, dynamicTicks = TRUE)
                
              }) # End renderPlotly
              values$plot_i <- values$plot_i + 1
            } else {
              pt1 <- NULL
            }
          })
          
          observeEvent( input$exp, {
            if (input$exp){
              if ( values$plot_i != input$n ){
                updateSliderInput(session, 'n', value=values$plot_i)
              }
              output[[ paste0('plot', values$plot_i) ]] <- renderPlotly({
                
                pt2 <- plotly::ggplotly(k$peXpU, dynamicTicks = TRUE)
                
              }) # End renderPlotly
              values$plot_i <- values$plot_i + 1
            } else {
              pt2 <- NULL
            }
          })
          
          observeEvent( input$expAligned, {
            if (input$expAligned) {
              if ( values$plot_i != input$n ){
                updateSliderInput(session, 'n', value=values$plot_i)
              }
              output[[ paste0('plot', values$plot_i) ]] <- renderPlotly({
                
                pt3 <- plotly::ggplotly(k$peXpA, dynamicTicks = TRUE)
                
              }) # End renderPlotly
              values$plot_i <- values$plot_i + 1
            } else {
              pt3 <- NULL
            }
          })
          # # Store plots in a list
          # ptlist <- list(pt1, pt2, pt3)
          # # remove the null plots from ptlist and wtlist
          # to_delete <- !sapply(ptlist,is.null)
          # ptlist <- ptlist[to_delete] 
          # 
          # if (length(ptlist)==0) return(NULL)
          # 
        }, error= function(e) {
          cat("There was some sort of error that occured")
        })  
          
          
          
        } # End if !input$Reference != input$Experiment
        
        
      } # End else
      
      
    }) # End Observe Event
  
  
  
  
  
} ## End Server


shinyApp(ui = ui, server = server)

