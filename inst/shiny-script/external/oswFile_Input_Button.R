oswFile_Input_Button <- function(  input, output, app.obj, session  ) {
  observeEvent( input$OSWFile, {
    
    tryCatch(
      expr = {
        ## Define Roots
        roots <- c( getwd(), path.expand("~"), .Platform$file.sep, app.obj$mostRecentDir )
        names(roots) <- c("Working Directory", "home", "root", "Recent Directory")
        roots <- c(roots, app.obj$drives()) 
        ## OSWFile
        shinyFileChoose(input, 'OSWFile', roots = roots, defaultRoot = 'root', defaultPath = .Platform$file.sep  )
        ### Create a reactive object to store OSWFile
        oswFile <- reactive(input$OSWFile)
        
        app.obj$OSWFile <- renderText({  
          app.obj$oswFile
        }) 
        
        if ( class(oswFile())[1]=='list' ){
          ## Get root directory based on used choice, working directory, home or root
          root_node <- roots[ which( names(roots) %in% oswFile()$root ) ]
          ## Get oswFile working directroy of user selected directory
          app.obj$oswFile <- lapply( oswFile()$files, function(x){ paste( root_node, file.path( paste( unlist(x), collapse = .Platform$file.sep ) ), sep = .Platform$file.sep ) }) 
          names(app.obj$oswFile) <- lapply(app.obj$oswFile, basename)
          ## Update app.obj most recent directroy
          app.obj$mostRecentDir <- dirname( dirname( app.obj$oswFile[[1]] ) )
          ## Load OSW file
          use_ipf_score <- Score_IPF_Present( app.obj$oswFile[[1]] )
          tictoc::tic()
          osw_df <- getOSWData_( oswfile=app.obj$oswFile[[1]], decoy_filter = TRUE, ms2_score = TRUE, ipf_score =  use_ipf_score)
          m_score_filter_var <- ifelse( length(grep( "m_score|mss_m_score", colnames(osw_df), value = T))==2, "m_score", "ms2_m_score" )
          osw_df %>%
            dplyr::filter( !is.na(m_score_filter_var)) -> osw_df
          app.obj$osw_df <- osw_df
          exec_time <- tictoc::toc(quiet = TRUE)
          message( sprintf("[DrawAlignR::oswFile_Input_Button] Caching OSW Feature Scoring Data took %s seconds", round(exec_time$toc - exec_time$tic, 3) ))
          
          ## Cache Transition Feature Scoring Information
          if ( input$ShowTransitionScores ){
            tictoc::tic()
            transition_dt <- getTransitionScores_( oswfile = in_osw, run_name = "", precursor_id = "", peptide_id = "")
            app.obj$transition_dt <- transition_dt
            exec_time <- tictoc::toc(quiet = TRUE)
            message( sprintf("[DrawAlignR::oswFile_Input_Button] Caching Transition Feature Scoring Data took %s seconds", round(exec_time$toc - exec_time$tic, 3) ))
          }
          
          if ( dim(app.obj$lib_df)[1]==0){
            ## Get list of unique modified peptides
            uni_peptide_list <- as.list(unique( osw_df$FullPeptideName ) )
            ## Update selection list with unique peptides
            updateSelectizeInput( session, inputId = 'Mod', choices = uni_peptide_list, selected = uni_peptide_list[1]  )
            input$Mod <- uni_peptide_list[1]
          }
        }
        
      },
      error = function(e){
        message(sprintf("[Observe OSW Input Button] There was the following error that occured during OSW Input Button observation: %s\n", e$message))
      }
    ) # End tryCatch
    
  })
  return(list( app.obj=app.obj ))
}
