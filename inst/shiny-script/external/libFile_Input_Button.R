libFile_Input_Button <- function( input, output, app.obj, session ) {
  observeEvent( input$LibraryFile, {
    tryCatch(
      expr = {
        ## Define Roots
        roots <- c( getwd(), path.expand("~"), .Platform$file.sep, app.obj$mostRecentDir )
        names(roots) <- c("Working Directory", "home", "root", "Recent Directory")
        roots <- c(roots, app.obj$drives()) 
        ## LibraryFile
        shinyFileChoose(input, 'LibraryFile', roots = roots, defaultRoot = 'root', defaultPath = .Platform$file.sep  )
        ### Create a reactive object to store LibraryFile
        libFile <- reactive(input$LibraryFile)
        
        app.obj$LibraryFile <- renderText({  
          app.obj$libFile
        }) 
        if ( class(libFile())[1]=='list' ){
          ## Get root directory based on used choice, working directory, home or root
          root_node <- roots[ which( names(roots) %in% libFile()$root ) ]
          ## Get libFile working directroy of user selected directory
          app.obj$libFile <- lapply( libFile()$files, function(x){ paste( root_node, file.path( paste( unlist(x), collapse = .Platform$file.sep ) ), sep = .Platform$file.sep ) }) 
          names(app.obj$libFile) <- lapply(app.obj$libFile, basename)
          ## Update app.obj most recent directroy
          app.obj$mostRecentDir <- dirname( dirname( app.obj$libFile[[1]] ) )
          ## Read in library and Cache Library onto disk
          tictoc::tic("Reading and Caching Library File")
          #lib_df <- getPepLibData_( app.obj$libFile[[1]] )
          lib_df <- DrawAlignR:::pqp_data_access( filename = app.obj$libFile[[1]] )
          app.obj$lib_df <- lib_df
          tictoc::toc()
          ## Get list of unique modified peptides
          uni_peptide_list <- as.list(unique( lib_df$MODIFIED_SEQUENCE )) 
          ## Update slection list with unique peptides
          updateSelectizeInput( session, inputId = 'Mod', choices = uni_peptide_list, selected = uni_peptide_list[1]  )
          input$Mod <- uni_peptide_list[1]
        }
        
      },
      error = function(e){
        message(sprintf("[Observe Library Input Button] There was the following error that occured during Library Input Button observation: %s\n", e$message))
      }
    ) # End tryCatch
    
  })
  return(list( app.obj=app.obj ))
}