chromFile_Input_Button <- function( input, output, app.obj, session ){
  
  observeEvent( input$ChromatogramFile, {
    tryCatch(
      expr = {
        ## Define Roots
        roots <- c( getwd(), path.expand("~"), .Platform$file.sep, app.obj$mostRecentDir )
        names(roots) <- c("Working Directory", "home", "root", "Recent Directory")
        roots <- c(roots, app.obj$drives()) 
        ## ChromatogramFile
        shinyFileChoose(input, 'ChromatogramFile', roots = roots, defaultRoot = 'root', defaultPath = .Platform$file.sep )
        ### Create a reactive object to store ChromatogramFile
        chromFile <- reactive(input$ChromatogramFile)
        app.obj$ChromatogramFile <- renderText({  
          app.obj$chromFile
        }) 
        if ( class(chromFile())[1]=='list' ){
          ## Get root directory based on used choice, working directory, home or root
          root_node <- roots[ which( names(roots) %in% chromFile()$root ) ]
          ## Get chromFile working directroy of user selected directory
          app.obj$chromFile <- lapply( chromFile()$files, function(x){ normalizePath( paste( root_node, file.path( paste( unlist(x), collapse = .Platform$file.sep ) ), sep = .Platform$file.sep ) ) }) 
          
          names(app.obj$chromFile) <- lapply(app.obj$chromFile, function(file_path){gsub("\\..*", "", basename(file_path))} )
          
          ## Update app.obj most recent directroy
          app.obj$mostRecentDir <- dirname( dirname(app.obj$chromFile[[1]]) )
          
          ## Store chromatogram file run names
          # app.obj$chromnames <- gsub("\\.chrom\\.mzML$|\\.chrom\\.sqMass$", "", input$ChromatogramFile$name)
          app.obj$chromnames <- gsub("\\.chrom\\.mzML$|\\.chrom\\.sqMass$", "", names(app.obj$chromFile))
          if ( !is.null(app.obj$osw_df) | dim(app.obj$osw_df)[1]>0 ){
            app.obj$osw_df %>%
              dplyr::select( filename ) %>%
              dplyr::group_by( filename ) %>%
              dplyr::add_count() %>%
              unique() %>%
              dplyr::ungroup() %>%
              dplyr::filter( n == max(n) ) %>%
              dplyr::select( filename ) %>%
              as.character() %>%
              basename() %>%
              strsplit("\\.") %>% unlist() %>% dplyr::nth(1) -> run_with_most_features
          } else {
            run_with_most_features <- app.obj$chromnames[1]
          }
          ## Update Reference list
          updateSelectizeInput( session, inputId = "Reference", choices = as.list(run_with_most_features) )
          ## Update Experiment list with first entry removed
          updateSelectizeInput( session, inputId = "Experiment", choices = as.list(app.obj$chromnames[-(match(run_with_most_features, app.obj$chromname))]), selected = as.list(app.obj$chromnames[-(match(run_with_most_features, app.obj$chromname))]) )
          ## Update n chroms input
          n_runs_index <- c(seq(1, length(app.obj$chromnames)))
          names(n_runs_index) <-  paste( "Run ", seq(1, length((app.obj$chromnames))), sep='')
          run_index_map <- c(seq(1, length(app.obj$chromnames)))
          names(run_index_map) <- app.obj$chromnames
          app.obj$run_index_map <- run_index_map
          shiny::updateCheckboxGroupInput( session, inputId = "n_runs", choices = n_runs_index, selected = seq(1, length((app.obj$chromnames))), inline = TRUE  )
        }
        
        # ## Get File Extension Type
        # # fileType <- gsub( '.*\\.', '', input$ChromatogramFile$name)
        # fileType <- unique(gsub( ".*\\.", "", app.obj$chromFile))
        # if ( tolower(fileType)=='mzml' | tolower(fileType)=='mzml.gz' ){
        #   ##*******************************
        #   ## Pre-Load mzML Files
        #   ##*******************************
        #   output$bar <- renderPlot({
        #     withProgress(message = sprintf('Cacheing %s mzML Chromatogram File(s)...', length(n_runs_index)),
        #                  detail = 'This might take a while for large chromatogram files...', value = 0, {
        #                    mzPntrs <- DrawAlignR::getmzPntrs( input, app.obj, progress=TRUE  )
        #                  })
        #   })
        #   ## Store mzPntrs container
        #   app.obj$mzPntrs <- mzPntrs
        # } else if ( tolower(fileType)=='sqmass' ){
        #   ##*******************************
        #   ## Pre-Load sqMass Files
        #   ##*******************************
        #   ## Get filenames from osw files and check if names are consistent between osw and mzML files. 
        #   filenames <- getRunNames( input$WorkingDirectory, oswMerged=TRUE, chrom_ext=".chrom.sqMass")
        #   runs <- filenames$runs
        #   names(runs) <- rownames(filenames)
        #   output$bar <- renderPlot({
        #     withProgress(message = sprintf('Cacheing %s mzML Chromatogram File(s)...', length(n_runs_index)),
        #                  detail = 'This might take a while for large chromatogram files...', value = 0, {
        #                    mzPntrs <- DrawAlignR::getsqMassPntrs(dataPath=input$WorkingDirectory, runs)
        #                  })
        #   })
        #   app.obj$mzPntrs <- mzPntrs
        # } else {
        #   warning( sprintf("There was an unknown chromtagoram file format: %s. Could not cache data of this type. Email or submit any issue to %s.", fileType, getMaintainer() ))
        # }
        
        
      },
      error = function(e){
        message(sprintf("[Observe Chromatogram Input Button] There was the following error that occured during Chromatogram Input Button observation: %s\n", e$message))
      }
    ) # End tryCatch
  })
  
  return(list( app.obj=app.obj ))
  
}