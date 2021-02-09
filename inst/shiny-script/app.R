#' Builds the shiny webapp for DrawAlignR
#'
#' @return None. Calling this script creates the Shiny webapp
#'
#' @import shiny
#' @import shinyjs
#' @import shinyFiles 
#' @import shinyWidgets
#' @import shinyBS
#' @import plotly
#' @importFrom  DIAlignR getGlobalAlignment getAlignObj 
#'

## This should be done only if the user does not have the package, otherwise it will install everytime the app is run, and will be very slow for installing large packages.
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("mzR") # Requires netcdf

## Some usefull functionality here
# install.packages("shinyjs")

## Cool Widgets
# install.packages("shinyWidgets")

## Extra Bootstrap controls
# install.packages("shinyBS")


### TODO in the future: Link zooming
### https://stackoverflow.com/questions/47933524/how-do-i-synchronize-the-zoom-level-of-multiple-charts-with-plotly-js


library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyWidgets)
library(shinyBS)
library(shinyalert)
library(plotly)
library(DrawAlignR)
library(DBI)
# library(mstools)
# library(DIAlignR)

# soetwd("./inst/shiny-script/")

source( "external/uiTabs.R", local = TRUE )
source( "external/server_help_description_text.R", local = TRUE )
source( "external/chromFile_Input_Button.R", local = TRUE )
source( "external/libFile_Input_Button.R", local = TRUE )
source( "external/oswFile_Input_Button.R", local = TRUE )
source( "external/workingDirectory_Input.R", local = TRUE )
source( "external/linkZoomEvent.R", local = TRUE )
source( "external/cacheChromatogramData.R", local = TRUE )
source( "external/cacheAlignmentPlots.R", local = TRUE )
source( "external/drawAlignedPlots.R", local = TRUE )
source( "external/clearPlots.R", local = TRUE )
# source( "external/withConsoleRedirect.R", local = TRUE )
# source( "external/customswitchButton.R", local = TRUE )

# setwd("../../")

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  ## Set theme
  theme = "button.css",
  
  useShinyjs(),  # Include shinyjs
  useShinyalert(),  # Set up shinyalert
  
  titlePanel( title=div( img(src="DIAlignR-logo.png", width = 80, height = 80, align="top" ), ( HTML(sprintf("DrawAlignR <h6 style='display:inline'>Ver: %s</h6>", tryCatch(expr={ver<-packageVersion("DrawAlignR")}, error = function(e){ ver<-'0' }) )) ) ),
              windowTitle = HTML("<title>DrawAlignR</title> <link rel='icon' type='image/gif/png' href='DIAlignR-logo.png'>")),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        GeneralSettingsTab, 
        AlignmentSettingsTab,
        PlottingSettingsTab
      ) # End of tabsetPanel
    ), # End of sidebarPanel
    
    
    mainPanel(
      verbatimTextOutput("brushing"),
      
      # Output: Tabset with Chromatogram Plots, Path Plots and Tables
      tabsetPanel(type = "tabs", id="output_tabs",
                  tabPanel( "chromPlot", uiOutput("plots") ),
                  tabPanel( "pathPlot", uiOutput("pathplots") ),
                  tabPanel( "oswTable", uiOutput("oswtables" ) ),
                  tabPanel( "logs", 
                            fluidPage(
                              tags$style(HTML("
                  #console {
                    height:800px;
                    overflow-y:scroll
                  }
                  ")),
                              pre(id = "console")
                            )
                  )
      ),
      ## Caching Progress Bar
      plotOutput("bar") 
      
      # absolutePanel( id='log-pannel', draggable = TRUE,
      #                ## Log
      #                fluidRow(
      #                  
      #                  # log output
      #                  textOutput( 'log' )
      #                )
      # )
      
    ), # End of mainPanel
    
  ) # End of sidebarLayout
) # End of ui


# SERVER ------------------------------------------------------------------

## TODO: REMOVE THE LINE BELOW WHEN DEPLOYING STABLE VERSION
# print(list.files( getwd(), full.names = T, recursive = T) )
# lapply(list.files("./R/", full.names = T), function( source_file ) { message(sprintf("Loading Source File: %s\n", source_file)); source(source_file, local = TRUE) } )
# unzoom_double_click <<- NULLi
# TODO Use tools::file_ext(app.obj$chromFile, exts=c("mzML", "mzML.gz", "chrom.mzML")) for getting files, this is more precise.
server <- function(input, output, session) {
  ## Copy logs for debugging
  observeEvent(input$copy,{
    destDir <- '/data/'
    file_to_cp <- '/srv/shiny-server/DrawAlignR/inst/shiny-script/mstools-trace.log'
    cat("Copying file to:", destDir,"\n")
    result <- file.copy( file_to_cp,
                         file.path(destDir, basename(file_to_cp)) )
    cat("Done copying file to:", destDir,"/", basename(file_to_cp), "\n")
  })
  
  # Server Help Annotations -------------------------------------------------
  
  server_help_description_text(input, output, session)
  
  
  # Server Reactive Values --------------------------------------------------
  
  ## reactive app.obj object to store some re-usable stuff
  app.obj <- reactiveValues(
    datapath = '', 
    previous_datapath = '',
    chromFile = '', 
    libFile = '', 
    oswFile = '', 
    mostRecentDir = getwd(), 
    foundChromFiles = list(mzml=list(), sqmass=list()), 
    cacheChromData = T,
    ChromDatabase = F,
    mzPntrsdb = '',
    chromTypes_available = "",
    chrom_ext = "",
    plotly.autorange.x = T,
    plotly.autorange.y = T,
    link_zoom_range_current = list(),
    unzoom_double_click = NULL,
    run_mapping_table = list(run_id=numeric(), 
                             runs=character(), 
                             plot_id=character(), 
                             reference=logical(), 
                             experiment=logical(),
                             display=logical(),
                             chromfile_path=character()
    ),
    Reference = '',
    Previous_Reference = '',
    Experiments_to_Align = '',
    # run_mapping_table = "",
    transition_selection_list = list(),
    lib_df = NULL,
    osw_df = NULL,
    transition_dt =NULL,
    reference_plotted = FALSE,
    drives = shinyFiles::getVolumes(),
    plots = list(),
    alignedChromsPlot = list(),
    alignmentPathPlot = list(),
    mzPntrs = NULL,
    start_plotting = FALSE,
    app_notification_ids = character(0)
  )
  ## Append class DrawAlignR
  # class(app.obj) <- "DrawAlignR"
  class(app.obj) <- append(class(app.obj), "DrawAlignR")
  # global <- reactiveValues(
  #   datapath = '', 
  #   previous_datapath = '',
  #   chromFile = '', 
  #   libFile = '', 
  #   oswFile = '', 
  #   mostRecentDir = getwd(), 
  #   foundChromFiles = list(mzml=list(), sqmass=list()), 
  #   chromTypes_available = "",
  #   chrom_ext = "",
  #   plotly.autorange.x = T,
  #   plotly.autorange.y = T,
  #   link_zoom_range_current = list(),
  #   unzoom_double_click = NULL
  # )
  output$chromTypes_available <- renderText({ '' })
  # link_zoom_ranges  <- reactiveValues(x = NULL, y = NULL)
  brush <- NULL
  makeReactiveBinding("brush")
  n_runs_index <- NULL
  out.plot.h <- NULL
  # link_zoom_ranges <- reactiveValues(`xaxis.range[0]`=NULL, `xaxis.range[1]`=NULL, `yaxis.range[0]`=NULL, `yaxis.range[1]`=NULL)
  # link_zoom_range_current <- reactiveValues(`xaxis.range[0]`=NULL, `xaxis.range[1]`=NULL, `yaxis.range[0]`=NULL, `yaxis.range[1]`=NULL)
  # link_zoom_range_current <- reactiveValues()
  # link_zoom_ranges <- list(`xaxis.range[0]`=1650, `xaxis.range[1]`=1900, `yaxis.range[0]`=0, `yaxis.range[1]`=15000)
  
  
  # Show/Hide Output Tabs ---------------------------------------------------
  observeEvent( {
                  app.obj$chromTypes_available
                  input$Align
    }, {
    ## General
    if ( length(app.obj$chromTypes_available) >= 1 && app.obj$chromTypes_available != '' ){
      MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app - Show/Hide Output Tabs] Showing chromPlot and oswTable tabs..."))
      showTab(inputId = "output_tabs", target = "chromPlot")
      showTab(inputId = "output_tabs", target = "oswTable")
    } else {
      MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app - Show/Hide Output Tabs] Hiding chromPlot and oswTable tabs..."))
      hideTab(inputId = "output_tabs", target = "chromPlot")
      hideTab(inputId = "output_tabs", target = "oswTable")
    }
    ## Alignment Specific
    if ( input$Align==TRUE ){
      MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app - Show/Hide Output Tabs] Showing pathPlot tabs..."))
      showTab(inputId = "output_tabs", target = "pathPlot")
    } else {
      MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app - Show/Hide Output Tabs] Hiding pathPlot tabs..."))
      hideTab(inputId = "output_tabs", target = "pathPlot")
    }
  })
  
  # File Input Events -------------------------------------------------------
  
  ## Observe Working Directory Input material switch.
  #   User may switch between using a working directory or
  #   supply each individual file
  observeEvent( input$WorkingDirectoryInput, {
    withConsoleRedirect("console", {
    message("Welcome! Have fun Aligning! =)")
    if ( input$WorkingDirectoryInput ){
      ## Observe interactive set working directory button
      workingDirectory_Input( input, output, app.obj, session )
      # if ( app.obj$datapath!='' & app.obj$chromFile!=''  ){
      # app.obj$start_plotting <- TRUE
      # }
    } else {
      ## Observe input chromatogramfile 
      chromFile_Input_Button( input, output, app.obj, session ) 
      
      ## Observe LibraryFile button
      libFile_Input_Button( input, output, app.obj, session )
      
      ## Observe OSWFile button
      oswFile_Input_Button(  input, output, app.obj, session  )
      
    }
    })
  })
  
  ## Get mapping of runs to filename
  observeEvent( {
    app.obj$oswFile
    app.obj$chromFile
    input$chromType_Choice
  },{
    message("INFO: Generating Mapping table between OSW-PQP-ChromFiles...")
    if ( input$chromType_Choice!='' | length(input$chromType_Choice) > 0){
      ## Get mapping of runs to filename
      if ( grepl(".*mzml", input$chromType_Choice) ){
        use_chrom_ext <- "*.mzML|.chrom.mzML"
      } else if ( grepl(".*sqmass", input$chromType_Choice) ){
        use_chrom_ext <- "*.sqMass|.chrom.sqMass"
      } else {
        use_chrom_ext <- NULL
      }
      if ( !is.null(use_chrom_ext) & app.obj$oswFile!='' & all(app.obj$chromFile!='') ){
        app.obj$runs_filename_mapping <<- getRunNames(oswFiles = app.obj$oswFile, chromFiles = app.obj$chromFile, oswMerged = TRUE, chrom_ext = use_chrom_ext)
      }
    }
  })
  
  # Chromatogram File Caching Events ---------------------------------------
  
  ## If multiple chromatogram format types are found, check to see which fortmat user wants to use  
  observeEvent( {
    input$chromType_Choice
    app.obj$datapath
    app.obj$runs_filename_mapping
  } , {
    ## TODO put the caching function here
    cacheChromatogramData( input, output, app.obj, session )
  }) # End observer
  
  # Reference and Experiment Input Events -----------------------------------
  
  
  ## Observe Reference input
  observeEvent( input$Reference, {
    withConsoleRedirect("console", {
    ## Get Reference Run
    app.obj$Reference <- input$Reference
    if ( app.obj$Reference!=app.obj$Previous_Reference & app.obj$Previous_Reference!='' ){
      message(sprintf("Previous Reference: %s --> New Reference: %s\n", app.obj$Previous_Reference, app.obj$Reference ))
      # app.obj$reference_plotted <- FALSE
      # ref_plotname <- paste("plot_run_", app.obj$run_index_map[[ app.obj$Previous_Reference ]], sep="")
      # clearPlots( input, output, app.obj, session )
    }
    app.obj$Previous_Reference <- input$Reference
    ## Get Experiments minuc Reference
    app.obj$Experiments_to_Align <- app.obj$chromnames[ !(app.obj$chromnames %in% input$Reference) ]
    ## Update Experiment list with first entry removed
    updateSelectizeInput( session, inputId = "Experiment", choices = as.list(app.obj$Experiments_to_Align), selected = as.list(app.obj$Experiments_to_Align) )
    
    ##TODO HERE
    # shiny::updateCheckboxGroupInput( session, inputId = "n_runs", choices = n_runs_index, selected = seq(1, length((app.obj$chromnames))), inline = TRUE  )
    }) # End withConsoleRedirect
  }) # End observer

  # Runs to Display ---------------------------------------------------------

## Observe runs to display to show which runs to display
  observeEvent( input$runs_to_display, {
    withConsoleRedirect( "console", {
      if ( !is.null(input$runs_to_display) ){
        ## These are the runs that will be dispalyed
        message( sprintf("These are the current runs to display:\n%s", paste( paste( "Run_", seq(1, length(input$runs_to_display)), input$runs_to_display, sep = " "), collapse = "\n") ) )
        app.obj$n_runs <- seq(1, length(input$runs_to_display))
        app.obj$runs_to_display <- input$runs_to_display
        
      } else {
        app.obj$runs_to_display <- ""
      warning("There are no runs selected to display!")
    }
    
  })
}, ignoreNULL = FALSE)
  
  # Observe changes in Reference, Experiment and  runs to display -----------
  
  observeEvent( {
    app.obj$runs_to_display
    app.obj$Reference
    app.obj$Experiments_to_Align
  }, {
    ## Update mapping table
    if ( length(app.obj$run_mapping_table$chromfile_path) < nrow(app.obj$runs_filename_mapping) ){
      print("INFO: Reseting run_mapping_table, detected a change in number of chromatogram files.")
      app.obj$run_mapping_table <- list(run_id=numeric(), 
           runs=character(), 
           plot_id=character(), 
           reference=logical(), 
           experiment=logical(),
           display=logical(),
           chromfile_path=character()
      )
    }

    app.obj$run_mapping_table$runs <- c(app.obj$chromnames[app.obj$chromnames %in% app.obj$Reference], app.obj$chromnames[!(app.obj$chromnames %in% app.obj$Reference)])
    app.obj$run_mapping_table$run_id <- seq(1, length(app.obj$run_mapping_table$runs))
    app.obj$run_mapping_table$plot_id <- paste("plot_run_", app.obj$run_mapping_table$run_id, sep="")
    app.obj$run_mapping_table$reference <- app.obj$run_mapping_table$runs %in% app.obj$Reference
    app.obj$run_mapping_table$experiment <- app.obj$run_mapping_table$runs %in% app.obj$Experiments_to_Align
    app.obj$run_mapping_table$display <- app.obj$run_mapping_table$runs %in% input$runs_to_display
    app.obj$run_mapping_table$chromfile_path <- unlist(lapply( app.obj$run_mapping_table$runs, function(x){  unlist(app.obj$chromFile)[  grepl(x, (app.obj$chromFile)) ] } ))
    app.obj$run_mapping_table$chromfile_path <- as.character(app.obj$run_mapping_table$chromfile_path)
    app.obj$run_mapping_table <- as.data.frame( app.obj$run_mapping_table )
   
    app.obj$runs_filename_mapping %>% 
      tibble::rownames_to_column(var="run_number") %>%
      merge(., app.obj$run_mapping_table, by="runs", all.y=T ) %>%
      dplyr::arrange( run_id ) -> app.obj$run_mapping_table
    
  })
  # Peptide Selection Event -------------------------------------------------
  
  ## Observe Peptide Selection
  observeEvent( input$Mod, {
    withConsoleRedirect("console", {
    tryCatch(
      expr = {
        DrawAlignR:::update_charge_list( input, output, app.obj, session )
      },
      error = function(e){
        message(sprintf("[Updating Charge Drop Down List] There was the following error that occured during Charge Drop Down List: %s\n", e$message))
      }
    ) # End tryCatch
  }) # End withConsoleRedirect
  }) # End observer
  
  
  # Plot Settings Tab Events ------------------------------------------------
  
  ## transition_selection_list
  observeEvent( {input$yIdent
    input$bIdent }, {
      withConsoleRedirect("console", {
      tryCatch(
        expr = {
          
          if ( input$yIdent!="" ){
            app.obj$transition_selection_list$y <- c(text2numericInput(input$yIdent))
          } else {
            app.obj$transition_selection_list$y <- NULL
          }
          
          if ( input$bIdent!="" ){
            app.obj$transition_selection_list$b <- c(text2numericInput(input$bIdent))
          } else {
            app.obj$transition_selection_list$b <- NULL
          }
        }, 
        error = function(e){
          message(sprintf("[Observe Identifying ions selection] There was the following error that occured during Identifying Ions Input Observation: %s\n", e$message))
        }
      ) # End tryCatch
    }) # End withConsoleRedirect
    })# End observer
  
  
  # Plot Control Events -----------------------------------------------------
  
  
  observeEvent(app.obj$run_mapping_table,{
    withConsoleRedirect("console", {
      
    if ( !all(unlist(lapply(app.obj$run_mapping_table, length))==0) ){
    #Generate set of variable plots
    output$plots <- renderUI({
      
      plot_output_list <- lapply(1:length(app.obj$run_mapping_table$run_id), function(i) {
        run_index <- app.obj$run_mapping_table$run_id[[i]]
        
        plotname <- paste("plot_run_", run_index, sep="")
        message(sprintf("Creating Plot: %s\n", plotname))
        # plotOutput(plotname,
        #            dblclick = "link_zoom_dblclick",
        #            brush = brushOpts(
        #              id = "link_zoom_brush",
        #              resetOnNew = TRUE
        #            ),
        #            hover = hoverOpts(
        #              id = paste0(plotname, "_hover")
        #            )
        # ) # End of plotlyOutput
        plotlyOutput(plotname)
        
      })
      do.call(tagList, plot_output_list)
      
    })
    
    #Generate set of variable plots for path plots
    output$pathplots <- renderUI({
      
      path_plot_output_list <- lapply(1:length(app.obj$run_mapping_table$run_id), function(i) {
        run_index <- app.obj$run_mapping_table$run_id[[i]]
        
        path_plotname <- paste("pathplot_run_", run_index, sep="")
        message(sprintf("Creating Path Plot: %s\n", path_plotname))
        plotlyOutput(path_plotname, width = "120%", height = "1000px")
        
      })
      do.call(tagList, path_plot_output_list)
      
    })
    
    } ### End if
    
    
    }) # End withConsoleRedirect
  }) # End observer
  
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$link_zoom_brush, {
    brush <- input$link_zoom_brush
    if (!is.null(brush)) {
      link_zoom_ranges$x <- c(brush$xmin, brush$xmax)
      link_zoom_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      link_zoom_ranges$x <- NULL
      link_zoom_ranges$y <- NULL
    }
  })
  
  observeEvent(input$link_zoom_dblclick, {
    print("Resetting brush")
    session$resetBrush("link_zoom_brush")
    brush <<- NULL
    link_zoom_ranges$x <- NULL
    link_zoom_ranges$y <- NULL
  })
  
  
  # oswTable Output ---------------------------------------------------------
  
  observeEvent(app.obj$run_mapping_table,{
    withConsoleRedirect("console", {
    #Generate set of variable oswtables
    output$oswtables <- renderUI({
      
      datatable_output_list <- lapply(1:length(app.obj$run_mapping_table$run_id), function(i) {
        run_index <- app.obj$run_mapping_table$run_id[[i]]
        
        tablename <- paste("oswtable_run_", run_index, sep="")
        message(sprintf("Creating osw table: %s\n", tablename))
        
        DT::dataTableOutput(tablename)
        
      })
      do.call(tagList, datatable_output_list)
      
    })
  }) # End withConsoleRedirect
  }) # End observer
  

  # Notifications -----------------------------------------------------------
  observeEvent(input$close_notifications, {
    if (length(app.obj$app_notification_ids)>0){
      for ( i in app.obj$app_notification_ids){
        removeNotification(app.obj$app_notification_ids[1])
        app.obj$app_notification_ids <- app.obj$app_notification_ids[-1]
      }
      
    }
  })
  
  # Main Observation Event --------------------------------------------------
  
  observeEvent( 
    {
      input$Mod
      input$Align 
      input$refreshAlign
      input$Charge
      input$Reference
      app.obj$start_plotting
      app.obj$run_mapping_table
    }, {
      
      withConsoleRedirect("console", { 
        if ( !(input$Align)  ){
          if ( sum(app.obj$run_mapping_table$display)>0 ) {
            withProgress( message = sprintf('Generating Chromatogram Plots for  %s runs...', sum(app.obj$run_mapping_table$display)),
                         detail = 'Extracting Ion Chromatogram Traces...', value = 0, expr = {
                           #Generate all plots.
                           cat("wd:", getwd(), "\n")
                           # NOTE: Should we loop over each chrom file input, or loop over each selected n runs input
                           for ( i in seq(1,dim(app.obj$run_mapping_table)[1]) ) {
                             current_run_table <- app.obj$run_mapping_table[i, ]
                             ## If current run is set to not to be dispalyed, skip this run
                             if( isFALSE(current_run_table$display) ) { next }
                             local({
                               current_run_table <- app.obj$run_mapping_table[i, ]
                               
                               # run_index <- app.obj$n_runs[[i]]
                               run_index <- current_run_table$run_id
                               plotname <- paste("plot_run_", run_index, sep="")
                               
                               
                               #If alignment is disabled, generate standard chromatogram plot.
                               ## Warning Handles
                               if (  all(unlist(lapply( unique(basename(unlist(app.obj$chromFile))), function(x){!grepl(".*mzML$|.*sqMass$", x)}))) ) {
                                 warning('A Chromgatogram file(s) was not supplied or not found')
                               } else if ( !grepl(".*pqp$", app.obj$libFile) ){
                                 warning("A Library File was not supplied or not found")
                               } else if ( !grepl(".*osw$", app.obj$oswFile) ){
                                 warning("A Merged OSW Results File was not supplied or not found")
                               } else if (is.null(input$Mod)){
                                 warning("There was no peptide(s) found")
                               }
                               
                               ##****************************************************
                               ## Chromatogram Plot Generation
                               ##****************************************************
                               tryCatch(
                                 expr = {
                                   # chrom_input <- app.obj$chromFile[[i]]
                                   app.obj$current_chrom_input <- as.character( current_run_table$chromfile_path )
                                   app.obj$current_osw_input <- app.obj$oswFile[[1]]
                                   app.obj$current_peptide <- input$Mod
                                   app.obj$current_modification_labels <- regmatches(app.obj$current_peptide, gregexpr("\\(.*?\\)", app.obj$current_peptide))[[1]]
                                   app.obj$current_naked_peptide <-  gsub( paste(gsub('\\)', '\\\\)',gsub('\\(', '\\\\(', app.obj$current_modification_labels)), collapse = '|'), '', app.obj$current_peptide )
                                   app.obj$current_run_id <- current_run_table$run_number
                                   app.obj$current_manual_annotation_coordinates <- NULL
                                   app.obj$Precursor=input$Precursor
                                   app.obj$Detecting=input$Detecting
                                   app.obj$Identifying_Unique=input$Identifying_Unique
                                   app.obj$Charge = input$Charge
                                   app.obj$nIdentifyingTransitions = input$nIdentifyingTransitions
                                   app.obj$ShowTransitionScores = show_transition_scores=input$ShowTransitionScores
                                   app.obj$ShowAllPkGrps = input$ShowAllPkGrps
                                   
                                   MazamaCoreUtils::logger.trace('[DrawAlignR::app]  Call drawChromatogram function...')
                                   start_plot_time <- tictoc::tic(quiet = TRUE)
                                   out.plot.h <- DrawAlignR:::drawChromatogram( app.obj, type="default" )
                                   end_plot_time <- tictoc::toc(quiet = TRUE)
                                   MazamaCoreUtils::logger.trace(paste0('[DrawAlignR::app]  Plotting time: ', end_plot_time$toc - end_plot_time$tic))
                                   
                                 }, 
                                 error = function(e){
                                   MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app::drawChromatogram] There was the following error that occured during drawChromatogram function call: %s\n", e$message))
                                   current_noti_id <- showNotification( sprintf("There was the following error that occured during chromatogram generation for %s:\n%s\n", plotname, e$message), duration = NULL, type="error")
                                   app.obj$app_notification_ids <- c(app.obj$app_notification_ids, current_noti_id)
                                 }
                               ) # End tryCatch
                               
                               ##****************************************************
                               ## Render and Caching Chromatogram Plots
                               ##****************************************************
                               tryCatch(
                                 expr = {    
                                   MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app - rendering XIC] Preparing plot for rendering..."))
                                   ## Old method using plotly
                                   xic_plot_title <- paste0( out.plot.h$labels$title,
                                                         '<br>',
                                                         '<sup>',
                                                         gsub( ' \\| Precursor: \\d+ \\| Peptide: \\d+ \\| Charge: \\d+ | \\| ms2_m-score: .*' , ' ', gsub('\\\n', ' | ', out.plot.h$labels$subtitle)),
                                                         '</sup>')
                                   # MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app - rendering XIC] xic_plot_title: %s", xic_plot_title))
                                   plotly::ggplotly( p = (out.plot.h), source = plotname, tooltip = c("x", "y", "text"), dynamicTicks = T ) %>%
                                     plotly::layout(title = list( text = unique(xic_plot_title) ) ) %>%
                                     event_register(event="plotly_relayout") -> p.out
                                   MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app - rendering XIC] Rendering plot: %s", plotname))
                                   app.obj$plots[[plotname]] <- p.out
                                   app.obj$plots_org[[plotname]] <- p.out 
                                   MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app - rendering XIC] Get current zoom link range..."))
                                   app.obj$link_zoom_range_current[[plotname]] <<- reactiveValues(`xaxis.range[0]`=app.obj$plots_org[[plotname]]$x$layout$xaxis$range[1], `xaxis.range[1]`=app.obj$plots_org[[plotname]]$x$layout$xaxis$range[2],
                                                                                                 `yaxis.range[0]`=app.obj$plots_org[[plotname]]$x$layout$yaxis$range[1], `yaxis.range[1]`=app.obj$plots_org[[plotname]]$x$layout$yaxis$range[2])
                                   MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app - rendering XIC] Set unzoom double click to NULL..."))
                                   app.obj$unzoom_double_click[[plotname]] <- NULL
                                 }, 
                                 error = function(e){
                                   MazamaCoreUtils::logger.trace(sprintf("[DrawAlignR::app - rendering XIC] There was the following error that occured during XIC rendering: %s\n", e$message))
                                   # message(sprintf("[rendering XIC] There was the following error that occured during XIC rendering: %s\n", e$message))
                                 }
                               ) # End tryCatch
                               
                               
                               
                             }) # End local
                             ## Track progress
                             incProgress(1/dim(app.obj$run_mapping_table)[1])
                           } # End For
                           
                         }) # End of Progress Tracker
            
            ## Observe zoom
            withProgress(message = sprintf('Drawing Chromatogram Plots for  %s runs...', dim(app.obj$run_mapping_table)[1]),
                         detail = 'Draw Extracted Ion Chromatograms...', value = 0, expr = {
                           # output[['plot_run_']] <<- NULL
                           for ( i in seq(1,dim(app.obj$run_mapping_table)[1]) ) {
                             current_run_table <- app.obj$run_mapping_table[i, ]
                             ## If current run is set to not to be dispalyed, skip this run
                             if( isFALSE(current_run_table$display) ) { next }
                             local({
                               
                               ##********************************************************
                               ##  Chromatogram Plot Output
                               ##********************************************************
                               current_run_table <- app.obj$run_mapping_table[i, ]
                               
                               # run_index <- app.obj$n_runs[[i]]
                               run_index <- current_run_table$run_id
                               plotname <- paste("plot_run_", run_index, sep="")
                               output[[plotname]] <- renderPlotly({
                                 output$log <- renderText( paste(log, collapse = '\n') )
                                 
                                 if ( (input$plotly.linkedzooming.x==FALSE & input$plotly.linkedzooming.y==FALSE) || (app.obj$plotly.autorange.x==TRUE & app.obj$plotly.autorange.y==TRUE)  ){
                                   cat("\nNo Link Zoom/reset\n")
                                   # app.obj$unzoom_double_click <- NULL
                                   # js$resetDoubleClick()
                                   app.obj$plots[[plotname]] %>% 
                                     plotly::config( displayModeBar = input$plotly.displayModeBar )
                                   
                                 } else {
                                   cat( sprintf("\nLink Zoom:\nautorange.x=%s\nautorange.y=%s\n%s\n", app.obj$plotly.autorange.x[1], app.obj$plotly.autorange.y[1], listTostring(reactiveValuesToList( app.obj$link_zoom_range_current[[plotname]]))) )
                                   app.obj$plots[[plotname]] %>% 
                                     plotly::config( displayModeBar = input$plotly.displayModeBar ) %>%
                                     plotly::layout(
                                       xaxis = list( autorange=app.obj$plotly.autorange.x[1], range=as.double(c(reactiveValuesToList( app.obj$link_zoom_range_current[[plotname]])$`xaxis.range[0]`[1], reactiveValuesToList( app.obj$link_zoom_range_current[[plotname]])$`xaxis.range[1]`[1])) ), 
                                       yaxis = list( autorange=app.obj$plotly.autorange.y[1], range=as.double(c(reactiveValuesToList( app.obj$link_zoom_range_current[[plotname]])$`yaxis.range[0]`[1], reactiveValuesToList( app.obj$link_zoom_range_current[[plotname]])$`yaxis.range[1]`[1])) )
                                     ) 
                                 }
                                 
                               }) # End renderPlotly
                               
                               ## Get zoom events and unzoom events for linked zooming
                               linkZoomEvent( input, output, app.obj, session, plotname )
                               
                               
                               
                               ##********************************************************
                               ##  oswTable Output
                               ##********************************************************
                               ## Get Table tag id            
                               tablename <- paste("oswtable_run_", run_index, sep="")
                               ## Get current filename to filter table
                               ms_file_runname <- app.obj$runs_filename_mapping$filename[ app.obj$runs_filename_mapping$runs %in% gsub("\\.\\w+", "", basename(as.character(current_run_table$chromfile_path))) ]
                               ## Render Table            
                               output[[tablename]] <- DT::renderDataTable(
                                 app.obj$osw_df %>%
                                   dplyr::filter( FullPeptideName==input$Mod & Charge==input$Charge ) %>%
                                   dplyr::mutate( filename = basename(filename) ) %>%
                                   dplyr::filter( filename== basename(ms_file_runname) ) %>%
                                   dplyr::select( "filename", "Charge", "mz", "Intensity", "RT", "assay_rt", "leftWidth", "rightWidth", dplyr::contains("ms2_pep"), dplyr::contains("peak_group_rank"), dplyr::contains("d_score"), dplyr::contains("ms2_m_score"), dplyr::contains("m_score"), dplyr::contains("original_assay") ) 
                               )
                               
                             }) # End Local
                             incProgress(1/dim(app.obj$run_mapping_table)[1])
                           } # End for
                         }) # End of Progress Tracker
            
            
          } # End !is.null(app.obj$n_runs)
        } else {
          cat('Alignment Option was selected\n')
          
          withProgress(message = sprintf('Performing alignment for  %s runs...', length(input$Experiment)),
                       detail = 'Dynamic Programming Alignment...', value = 0, expr = {
                         AlignObj_List <- list()
                         app.obj$AlignObj_List <- AlignObj_List
                         for ( i in input$Experiment ) {
                           local({
                             
                             # print("Start Experiment Alignment")
                             # print(paste("Current Exp: ", i, sep=""))
                             
                             # Define Experiment_i
                             current_experiment <- i
                             # Get run Indec
                             run_index <- app.obj$run_index_map[[ current_experiment ]]
                             cat( sprintf("Working on Experiment %s with Run Index: %s\n", current_experiment, run_index) )
                             
                             #Ensuring at least two runs selected, not conducting alignment against same run
                             if ( !(input$Reference == gsub('...........$', '', current_experiment )) ) {
                               
                               
                               tryCatch(
                                 expr = {  
                                   
                                   tictoc::tic("DIAlignR Elapsed Time")
                                   dataPath <- input$WorkingDirectory
                                   analytes <- paste( input$Mod, "_", toString(input$Charge), sep="") 
                                   runs <- c(input$Reference, current_experiment)
                                   # cat( sprintf( "analytes: %s\nReference: %s\nExperiment: %s\n", analytes, input$Reference, current_experiment))
                                   
                                   # suppressWarnings(
                                   AlignObjOutput <- DrawAlignR::getAlignObjs(analytes = analytes, runs = runs, dataPath = dataPath, refRun = input$Reference,
                                                                              analyteInGroupLabel = input$analyteInGroupLabel, identifying = input$identifying, 
                                                                              oswMerged = input$oswMerged, nameCutPattern = input$nameCutPattern, chrom_ext=app.obj$chrom_ext,
                                                                              maxFdrQuery = input$maxFdrQuery, maxFdrLoess = input$maxFdrLoess, analyteFDR = input$analyteFDR, 
                                                                              spanvalue = input$spanvalue,  normalization = input$normalization, simMeasure = input$simMeasure,
                                                                              XICfilter = input$XICfilter, SgolayFiltOrd = input$SgolayFiltOrd, SgolayFiltLen = input$SgolayFiltLen,
                                                                              goFactor = input$goFactor, geFactor = input$geFactor, cosAngleThresh = input$cosAngleThresh, OverlapAlignment = input$OverlapAlignment,
                                                                              dotProdThresh = input$dotProdThresh, gapQuantile = input$gapQuantile, hardConstrain = input$hardConstrain, 
                                                                              samples4gradient = input$samples4gradient,  samplingTime = input$samplingTime,  RSEdistFactor = input$RSEdistFactor, 
                                                                              objType = "medium ", mzPntrs = app.obj$mzPntrs, runType = input$runType)
                                   # )
                                   tictoc::toc()
                                   cat("\n")
                                   
                                   app.obj$AlignObj_List[[current_experiment]] <- AlignObjOutput
                                   cat( sprintf("Added %s to list of aligned objects.\n Total in list now: %s\n", current_experiment, paste(names( app.obj$AlignObj_List), collapse=", ") ) )
                                 }, 
                                 error = function(e){
                                   message(sprintf("[DrawAlignR::app::doAlignment] There was the following error that occured during Alignment: %s\n", e$message))
                                   app.obj$AlignObj_List[[current_experiment]] <<- e$message
                                 }
                               ) # End tryCatch
                               
                             }
                           }) # End of local
                           ## Track progress
                           incProgress(1/length(input$Experiment))
                         } # End for loop
                         
                       }) # End of Progress Tracker
          
          
          
          ##***********************************************
          ##    Alignment Plotting Events
          ##***********************************************
          ## Clear Plots
          clearPlots( input, output, app.obj, session )
          ## Cache Algined Plots
          cacheAlignmentPlots( input, output, app.obj, session ) 
          ## Draw Aligned Results
          drawAlignedPlots( input, output, app.obj, session ) 
          
          
          
          # }) # End ObserveEvent for showing original annotaiton
          
          
          
          
          
        } # End else plot alignment checkbox
        
      }) ## With Consolde redirect
      
    }) # End Observe Event
  
  
  ## Return gloval varialbles for UI to use
  outputOptions(output, "chromTypes_available", suspendWhenHidden = FALSE)
  
} ## End Server


shinyApp(ui = ui, server = server)

