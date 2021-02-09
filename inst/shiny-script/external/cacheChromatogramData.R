cacheChromatogramData <- function( input, output, app.obj, session) {
  withConsoleRedirect("console", {
    if ( input$chromType_Choice!='' ){
      message( sprintf("Using chromtype: %s", input$chromType_Choice) )
      tryCatch(
        expr = {
          if ( grepl(".*mzml", input$chromType_Choice) ){
            if ( input$WorkingDirectoryInput  ) {
              app.obj$chromFile <- app.obj$foundChromFiles$mzml
              ## Store chromatogram file run names
              # app.obj$chromnames <- gsub("\\.chrom\\.mzML$|\\.chrom\\.sqMass$", "", input$ChromatogramFile$name)
              app.obj$chromnames <- gsub("\\.chrom?\\.mzML$|\\.chrom?\\.sqMass$", "", names(app.obj$chromFile))
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
              ## Update runs_to_display list
              # updateSelectizeInput( session, inputId = "runs_to_display", choices = as.list(app.obj$chromnames), selected = as.list(app.obj$chromnames) )
              shinyWidgets::updatePickerInput( session = session, inputId = "runs_to_display", choices = as.list(app.obj$chromnames), selected = as.list(app.obj$chromnames) ) 
              app.obj$n_runs <- seq(1, length(input$runs_to_display))
              ## Update Reference list
              updateSelectizeInput( session, inputId = "Reference", choices = as.list(app.obj$chromnames), selected = as.list(run_with_most_features) )
              ## Update Experiment list with first entry removed
              updateSelectizeInput( session, inputId = "Experiment", choices = as.list(app.obj$chromnames[-(match(run_with_most_features, app.obj$chromname))]), selected = as.list(app.obj$chromnames[-(match(run_with_most_features, app.obj$chromname))]) )
              ## Update n chroms input
              n_runs_index <- c(seq(1, length(app.obj$chromnames)))
              names(n_runs_index) <-  paste( "Run ", seq(1, length((app.obj$chromnames))), sep='')
              run_index_map <- c(seq(1, length(app.obj$chromnames)))
              names(run_index_map) <- app.obj$chromnames
              app.obj$run_index_map <- run_index_map
              
              # app.obj$runs_filename_mapping %>% dplyr::select( runs ) %>% dplyr::filter( runs %in% app.obj$chromnames ) ### TODO 
              
              # shiny::updateCheckboxGroupInput( session, inputId = "n_runs", choices = n_runs_index, selected = seq(1, length((app.obj$chromnames))), inline = TRUE  )
              ## Get File Extension Type
              # fileType <- gsub( '.*\\.', '', input$ChromatogramFile$name)
              fileType <- unique(gsub( ".*\\.", "", app.obj$chromFile))
              ## Store chromatogram extension
              app.obj$chrom_ext <- paste0(".", gsub("^.*?\\.","", app.obj$chromFile ) )
              if ( app.obj$cacheChromData ){
                if ( tolower(fileType)=='mzml' | tolower(fileType)=='mzml.gz' ){
                  ##*******************************
                  ## Pre-Load mzML Files
                  ##*******************************
                  output$bar <- renderPlot({
                    withProgress(message = sprintf('Caching %s mzML Chromatogram File(s)...', length(n_runs_index)),
                                 detail = 'This might take a while for large chromatogram files...', value = 0, expr = {
                                   app.obj$mzPntrs <- getmzPntrs( input, app.obj, progress=T  )
                                 })
                  })
                  ## Write mzPntrs obj to a database
                  ChromDatabase <- paste0(unique( dirname(app.obj$chromFile) )[[1]], .Platform$file.sep, "cached_chromatogram_data.mzPntrs")
                  write_mzPntrsdb( app.obj$mzPntrs, ChromDatabase )
                  app.obj$cacheChromData <- FALSE
                  app.obj$ChromDatabase <- TRUE
                  app.obj$mzPntrsdb <- ChromDatabase
                  
                } 
              }
            }
            
          } else if ( grepl(".*sqmass", input$chromType_Choice) ){
            if ( input$WorkingDirectoryInput  ) {
              app.obj$chromFile <- app.obj$foundChromFiles$sqmass
              
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
              ## Update runs_to_display list
              # updateSelectizeInput( session, inputId = "runs_to_display", choices = as.list(app.obj$chromnames), selected = as.list(app.obj$chromnames) )
              shinyWidgets::updatePickerInput( session = session, inputId = "runs_to_display", choices = as.list(app.obj$chromnames), selected = as.list(app.obj$chromnames) ) 
              app.obj$n_runs <- seq(1, length(input$runs_to_display))
              ## Update Reference list
              updateSelectizeInput( session, inputId = "Reference", choices = as.list(app.obj$chromnames), selected = as.list(run_with_most_features) )
              ## Update Experiment list with first entry removed
              updateSelectizeInput( session, inputId = "Experiment", choices = as.list(app.obj$chromnames[-(match(run_with_most_features, app.obj$chromname))]), selected = as.list(app.obj$chromnames[-(match(run_with_most_features, app.obj$chromname))]) )
              ## Update n chroms input
              n_runs_index <- c(seq(1, length(app.obj$chromnames)))
              names(n_runs_index) <-  paste( "Run ", seq(1, length((app.obj$chromnames))), sep='')
              run_index_map <- c(seq(1, length(app.obj$chromnames)))
              names(run_index_map) <- app.obj$chromnames
              app.obj$run_index_map <- run_index_map
              shiny::updateCheckboxGroupInput( session, inputId = "n_runs", choices = n_runs_index, selected = seq(1, length((app.obj$chromnames))), inline = TRUE  )
              
              ## Get File Extension Type
              # fileType <- gsub( '.*\\.', '', input$ChromatogramFile$name)
              fileType <- unique(gsub( ".*\\.", "", app.obj$chromFile))
              ## Store chromatogram extension
              app.obj$chrom_ext <- paste0(".", gsub("^.*?\\.","", app.obj$chromFile ) )
              if ( app.obj$cacheChromData ){
                if ( tolower(fileType)=='sqmass' ){
                  ##*******************************
                  ## Pre-Load sqMass Files
                  ##*******************************
                  ## Get filenames from osw files and check if names are consistent between osw and mzML files. 
                  filenames <- getRunNames( input$WorkingDirectory, oswMerged=TRUE, chrom_ext=".chrom.sqMass")
                  runs <- filenames$runs
                  names(runs) <- rownames(filenames)
                  output$bar <- renderPlot({
                    withProgress(message = sprintf('Caching %s sqMass Chromatogram File(s)...', length(n_runs_index)),
                                 detail = 'This might take a while for large chromatogram files...', value = 0, {
                                   app.obj$mzPntrs <- getsqMassPntrs(dataPath=input$WorkingDirectory, runs) # TODO Check filenames inconsistency, use app.obj$runs_filename_mapping
                                 })
                  })
                  ## Write mzPntrs obj to a database
                  ChromDatabase <- paste0(unique( dirname(app.obj$chromFile) )[[1]], .Platform$file.sep, "cached_chromatogram_data.mzPntrs")
                  write_mzPntrsdb( app.obj$mzPntrs, ChromDatabase )
                  app.obj$cacheChromData <- FALSE
                  app.obj$ChromDatabase <- TRUE
                  app.obj$mzPntrsdb <- ChromDatabase
                }
              }
            }
          } else {
            warning(sprintf("There was an issue with the chromType, selection is not a currently supported format: %s", input$chromType_Choice))
          }
        },
        error = function(e){
          message(sprintf("[DrawAlignR::main::chromType_Choice::caching %s] There was the following error that occured during chromatogram caching: %s\n", input$chromType_Choice, e$message))
          shinyalert::shinyalert("Oops!", sprintf("There was the following error that occured during chromatogram caching of type %s:\n%s\n", input$chromType_Choice, e$message), type = "error")
        }
      ) # End tryCatch
    }
  }) # end withConsoleRedirect
  
}