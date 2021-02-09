workingDirectory_Input <- function( input, output, app.obj, session ) {
  observeEvent( 
    {
      input$interactiveWorkingDirectory
      input$WorkingDirectory
    }, {
      if ( input$WorkingDirectoryInput  ) {
        
        ##*********************************
        ##    Working Directory
        ##*********************************
        tryCatch(
          expr = {
            ## Define Roots
            roots <- c( getwd(), path.expand("~"), .Platform$file.sep, app.obj$mostRecentDir )
            names(roots) <- c("Working Directory", "home", "root", "Recent Directory")
            roots <- c(roots, app.obj$drives()) 
            ## Get working directory contain data files
            shinyDirChoose(input=input, id='interactiveWorkingDirectory', roots = roots, defaultRoot = 'root', defaultPath = .Platform$file.sep, session=session  )
            ## Resetting Textbox input for working directory if interactive working directory button is selected.
            if( is.numeric(input$interactiveWorkingDirectory) ){
              message("[DrawAlignR::WorkingDirectoryInput] Working Directory button pressed. Resetting textbox\n")
              reset("WorkingDirectory")
            }
            if ( input$WorkingDirectory!="" | input$WorkingDirectory!=app.obj$datapath ) {
              message("Using test working directory input")
              app.obj$datapath <- normalizePath( input$WorkingDirectory )
              ## Get mapping of runs to filename
              app.obj$runs_filename_mapping <- getRunNames(dataPath = app.obj$datapath, oswMerged = TRUE, chrom_ext = ".chrom.mzML|.chrom.sqMass")
              ## Search working directory for osw file, mzml files, pqpfiles
              subDirs <- normalizePath( list.dirs( path = app.obj$datapath, full.names = T, recursive = F ) )
            } else {
              # print("Using interactive button working directory input")
              ### Create a reactive object to store working directory
              dir <- reactive( input$interactiveWorkingDirectory )
              app.obj$WorkingDirectory <- renderText({  
                app.obj$datapath
              })  
              
              if ( class(dir())[1]=='list' ){
                ## Get root directory based on used choice, working directory, home or root
                root_node <- roots[ which( names(roots) %in% dir()$root ) ]
                ## Get full working directroy of user selected directory
                app.obj$datapath <- normalizePath( paste( normalizePath(root_node), file.path( paste( unlist(dir()$path[-1]), collapse = .Platform$file.sep ) ), sep = .Platform$file.sep ) )
                if ( app.obj$datapath!=app.obj$previous_datapath & app.obj$previous_datapath!='' ){
                  message(sprintf("Previous working directory: %s --> New working directory: %s\n", app.obj$previous_datapath, app.obj$datapath ))
                }
                app.obj$previous_datapath <- app.obj$datapath
                ## Get mapping of runs to filename
                app.obj$runs_filename_mapping <- getRunNames(app.obj$datapath, oswMerged = TRUE, chrom_ext = ".chrom.mzML|.chrom.sqMass")
                ## Update app.obj most recent directroy
                app.obj$mostRecentDir <- app.obj$datapath
                ## Update Working Directory Text Box
                updateTextInput( session = session, inputId = 'WorkingDirectory', value = app.obj$datapath  )
              }
            }
            
            ## Search working directory for osw file, mzml files, pqpfiles
            subDirs <- normalizePath( list.dirs( path = app.obj$datapath, full.names = T, recursive = F ) )
          },
          error = function(e){
            message(sprintf("[DrawAlignR::WorkingDirectoryInput] There was the following error that occured while obtaining working directory: %s\n", e$message))
            shinyalert::shinyalert("Oops!", sprintf("There was the following error that occured while obtaining working directory: %s\n", e$message), type = "error")
          }
        ) # End tryCatch
        ## Search working directory for osw file, mzml files, pqpfiles
        subDirs <- normalizePath( list.dirs( path = app.obj$datapath, full.names = T, recursive = F ) )
        
        ##*********************************
        ##    OSW Path Search
        ##*********************************
        ##*********************************
        message("INFO: Searching for OSW file...")
        tryCatch(
          expr = {
            if ( "osw" %in% basename(subDirs) ) {
              target_subdir <- normalizePath( paste(app.obj$datapath ,'osw/',sep = .Platform$file.sep) )
              if ( any(grepl("\\.osw", list.files(subDirs))) ) {
                ## Search for OSW folder
                files_in_osw_dir <- normalizePath( list.files( target_subdir, pattern = "*osw$" ,full.names = T ) )
                if ( length(files_in_osw_dir) > 1 ){
                  warning( sprintf("There were %s osw files found, taking first file!!", length(files_in_osw_dir)) ) # TODO: If user uses non merged osw file?
                  showNotification( sprintf("There were %s osw files found, taking first file!!", length(files_in_osw_dir)), duration = 5, type="warning")
                  files_in_osw_dir <- normalizePath( files_in_osw_dir[1] )
                }
                app.obj$oswFile <- files_in_osw_dir
                message( sprintf("INFO: OSW File found: %s", app.obj$oswFile[[1]]) )
                message( "INFO: Loading data from file..." )
                ## Load OSW file
                use_ipf_score <- Score_IPF_Present( app.obj$oswFile[[1]] )
                tictoc::tic()
                ### TODO : Need to make sure that this is extracting the correct information from the osw file when using the ipf scores
                osw_df <- getOSWData_( oswfile=app.obj$oswFile[[1]], decoy_filter = TRUE, ms2_score = TRUE, ipf_score =  use_ipf_score)
                m_score_filter_var <- ifelse( length(grep( "m_score|ms2_m_score", colnames(osw_df), value = T))==2, "m_score", "ms2_m_score" )
                osw_df %>%
                  dplyr::filter( !is.na(m_score_filter_var)) -> osw_df
                app.obj$osw_df <- osw_df
                exec_time <- tictoc::toc(quiet = TRUE)
                message( sprintf("[DrawAlignR::workingDirectory_Input] Caching OSW Feature Scoring Data took %s seconds", round(exec_time$toc - exec_time$tic, 3) ))
                
                if ( input$ShowTransitionScores ){
                  tictoc::tic()
                  transition_dt <- getTransitionScores_( oswfile = app.obj$oswFile[[1]], run_name = "", precursor_id = "", peptide_id = "")
                  app.obj$transition_dt <- transition_dt
                  exec_time <- tictoc::toc(quiet = TRUE)
                  message( sprintf("[DrawAlignR::workingDirectory_Input] Caching Transition Feature Scoring Data took %s seconds", round(exec_time$toc - exec_time$tic, 3) ))
                }
                
                # if (  is.null( app.obj$lib_df ) ){
                #   ## Get list of unique modified peptides
                #   message( "INFO: Updating Peptide list..." )
                #   uni_peptide_list <- as.list(unique( osw_df$FullPeptideName ) )
                #   ## Update selection list with unique peptides
                #   updateSelectizeInput( session, inputId = 'Mod', choices = uni_peptide_list, selected = uni_peptide_list[1]  )
                #   # input$Mod <- uni_peptide_list[1]
                # }
              } else {
                warning( sprintf("There was no osw file found in osw directory:\n%s\n", target_subdir) )
                current_noti_id <- showNotification( sprintf("There was no osw file found in osw directory:\n%s\n", target_subdir), duration = NULL, type="warning")
                app.obj$app_notification_ids <- c(app.obj$app_notification_ids, current_noti_id)
              }
            } else {
              warning( sprintf("There was no osw folder found in working directory:\n%s\n", app.obj$datapath) )
              current_noti_id <- showNotification( sprintf("There was no osw folder found in working directory:\n%s\n", app.obj$datapath), duration = NULL, type="warning")
              app.obj$app_notification_ids <- c(app.obj$app_notification_ids, current_noti_id)
            }
          },
          error = function(e){
            message(sprintf("[WorkingDirectoryInput:findOSW] There was the following error that occured during OSW Path Searching: %s\n", e$message))
            shinyalert::shinyalert("Oops!", sprintf("There was the following error that occured during OSW Path Searching: %s\n", e$message), type = "error")
          }
        ) # End tryCatch
        
        ##*********************************
        ##    PQP Path Search
        ##*********************************
        message("INFO: Searching for PQP file...")
        tryCatch(
          expr = {
            if ( "pqp" %in% basename(subDirs)  ) {
              target_subdir <- normalizePath( paste(app.obj$datapath ,'pqp/',sep = .Platform$file.sep) )
              if ( any(grepl("\\.pqp", list.files(subDirs))) ){
                ## Search for PQP folder
                find_file <- normalizePath( list.files( target_subdir, pattern = "*pqp$" ,full.names = T ) )
                if ( length(find_file) > 1 ){
                  warning( sprintf("There were %s pqp files found, taking first file!!",  length(find_file))) # TODO: If user uses non merged osw file?
                  showNotification( sprintf("There were %s pqp files found, taking first file!!", length(find_file)), duration = 5, type="warning")
                  find_file <- find_file[1]
                }
                app.obj$libFile <- find_file
                message( sprintf("INFO: PQP File found: %s", app.obj$libFile[[1]]) )
                message( "INFO: Loading data from file..." )
                ## Read in library and Cache Library onto disk
                tictoc::tic()
                # lib_df <- getPepLibData_( app.obj$libFile[[1]] )
                lib_df <- DrawAlignR:::pqp_data_access( filename = app.obj$libFile[[1]] )
                app.obj$lib_df <- lib_df
                exec_time <- tictoc::toc(quiet = TRUE)
                message( sprintf("[DrawAlignR::workingDirectory_Input] Caching PQP Library Data took %s seconds", round(exec_time$toc - exec_time$tic, 3) ))
                # ## Get list of unique modified peptides
                # message( "INFO: Updating Peptide list..." )
                # uni_peptide_list <- as.list(unique( lib_df$MODIFIED_SEQUENCE )) 
                # ## Update slection list with unique peptides
                # updateSelectizeInput( session, inputId = 'Mod', choices = uni_peptide_list, selected = uni_peptide_list[1]  )
                # # input$Mod <- uni_peptide_list[1]
              } else {
                warning( sprintf("There was no pqp file found in pqp directory:\n%s\n", target_subdir) )
                current_noti_id <- showNotification( sprintf("There was no pqp file found in pqp directory:\n%s\n", target_subdir), duration = NULL, type="warning")
                app.obj$app_notification_ids <- c(app.obj$app_notification_ids, current_noti_id)
              }
            } else {
              warning( sprintf("There was no pqp folder found in working directory:\n%s\n", app.obj$datapath) )
              current_noti_id <- showNotification( sprintf("There was no pqp folder found in working directory:\n%s\n", app.obj$datapath), duration = NULL, type="warning")
              app.obj$app_notification_ids <- c(app.obj$app_notification_ids, current_noti_id)
            }
          },
          error = function(e){
            message(sprintf("[WorkingDirectoryInput:findPQP] There was the following error that occured during PQP Path Searching: %s\n", e$message))
            shinyalert::shinyalert("Oops!", sprintf("There was the following error that occured during PQP Path Searching: %s\n", e$message), type = "error")
          }
        ) # End tryCatch
        DrawAlignR:::update_peptide_list( input, output, app.obj, session )
        DrawAlignR:::update_charge_list( input, output, app.obj, session )
        ##*********************************
        ##    Chromatogram Files
        ##*********************************
        message("INFO: Searching for Chromatogram files...")
        tryCatch(
          expr = {
            ## Check to see if a chromatogram database file exists
            ChromDatabase <- list.files( path = subDirs, full.names = T, pattern = "*.mzPntrs" )
            if ( length(ChromDatabase)>0 ){
              app.obj$cacheChromData <- FALSE
              app.obj$ChromDatabase <- TRUE
              app.obj$mzPntrsdb <- ChromDatabase
            } 
            if ( "mzml" %in% basename(subDirs) | "sqmass" %in% basename(subDirs) ) {
              
              
              
              chromTypes_available <- grep("mzml|sqmass", basename(subDirs), value=TRUE)
              output$chromTypes_available <- renderText({ chromTypes_available })
              app.obj$chromTypes_available <- chromTypes_available
              chromTypes_available_list <- list()
              chromTypes_available_list <- (paste("Use ", chromTypes_available, sep=""))
              names(chromTypes_available_list) <- (paste("Use ", chromTypes_available, sep=""))
              shinyWidgets::updateAwesomeRadio(session = session, 
                                               inputId = "chromType_Choice",
                                               choices = chromTypes_available_list,
                                               inline = TRUE, 
                                               status = "primary",
                                               checkbox = TRUE
              )
              if ( "mzml" %in% basename(subDirs) ) {
                message("INFO: Searching mzml folder")
                target_subdir <- normalizePath( paste(app.obj$datapath ,'mzml/',sep = .Platform$file.sep) )
                if ( any(grepl("\\.mzML", list.files(subDirs))) ){
                  ## Search for OSW folder
                  find_file <- normalizePath( list.files( target_subdir, pattern = "*mzML$" ,full.names = T ) )
                  
                  app.obj$foundChromFiles$mzml <- find_file
                  names(app.obj$foundChromFiles$mzml) <- lapply(app.obj$foundChromFiles$mzml, function(file_path){gsub("\\..*", "", basename(file_path))} )
                } else {
                  warning( sprintf("There was no mzml file found in mzml directory:\n%s\n", target_subdir) )
                  current_noti_id <- showNotification( sprintf("There was no mzml file found in mzml directory:\n%s\n", target_subdir), duration = NULL, type="warning")
                  app.obj$app_notification_ids <- c(app.obj$app_notification_ids, current_noti_id)
                }
              }
              
              if ( "sqmass" %in% basename(subDirs) ) {
                message("INFO: Searching sqmass folder")
                target_subdir <- normalizePath( paste(app.obj$datapath ,'sqmass/',sep = .Platform$file.sep) )
                if ( any(grepl("\\.sqMass", list.files(subDirs))) ){
                  ## Search for OSW folder
                  find_file <- normalizePath( list.files( target_subdir, pattern = "*sqMass$" ,full.names = T ) )
                  
                  app.obj$foundChromFiles$sqmass <- find_file
                  names(app.obj$foundChromFiles$sqmass) <- lapply(app.obj$foundChromFiles$sqmass, function(file_path){gsub("\\..*", "", basename(file_path))} )
                  
                } else {
                  warning( sprintf("There was no sqmass file found in sqmass directory:\n%s\n", target_subdir) )
                  current_noti_id <- showNotification( sprintf("There was no sqmass file found in sqmass directory:\n%s\n", target_subdir), duration = NULL, type="warning")
                  app.obj$app_notification_ids <- c(app.obj$app_notification_ids, current_noti_id)
                }
              }
              
            } else {
              warning( sprintf("There was no mzml or sqmass folder found in working directory:\n%s\n", app.obj$datapath) )
              current_noti_id <- showNotification( sprintf("There was no mzml or sqmass folder found in working directory:\n%s\n", app.obj$datapath), duration = NULL, type="warning")
              app.obj$app_notification_ids <- c(app.obj$app_notification_ids, current_noti_id)
            }
            
            
            
          },
          error = function(e){
            message(sprintf("[WorkingDirectoryInput:findChroms] There was the following error that occured during Chromatogram Path Searching: %s\n", e$message))
            shinyalert::shinyalert("Oops!", sprintf("There was the following error that occured during Chromatogram Path Searching: %s\n", e$message), type = "error")
          }
        ) # End tryCatch
        message("[WorkingDirectoryInput:findChroms] Finished workingDirectory Input parsing\n")
        showNotification( sprintf("Finished parsing working directory:\n%s\n", app.obj$datapath), duration = 5, type="message")
      }
    })
  return(list( app.obj=app.obj ))
}
