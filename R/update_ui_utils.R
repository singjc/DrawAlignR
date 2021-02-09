#' @export
update_peptide_list <- function( input, output, app.obj, session ) {
  if (  !is.null( app.obj$lib_df ) ){
    ## Get list of unique modified peptides
    message( "INFO: Updating Peptide list from PQP..." )
    uni_peptide_list <- as.list(unique( app.obj$lib_df$MODIFIED_SEQUENCE )) 
    ## Update slection list with unique peptides
    updateSelectizeInput( session, inputId = 'Mod', choices = uni_peptide_list, selected = uni_peptide_list[1]  )
    # input$Mod <- uni_peptide_list[1]
  } else if ( !is.null(app.obj$osw_df) ) {
    ## Get list of unique modified peptides
    message( "INFO: Updating Peptide list from OSW..." )
    uni_peptide_list <- as.list(unique( app.obj$osw_df$FullPeptideName ) )
    ## Update selection list with unique peptides
    updateSelectizeInput( session, inputId = 'Mod', choices = uni_peptide_list, selected = uni_peptide_list[1]  )
    # input$Mod <- uni_peptide_list[1]
  } 
  
}

#' @export
update_charge_list <- function( input, output, app.obj, session ){
  ## Check if there is no lib df returned from intial library load
  if ( !is.null( app.obj$lib_df ) ){
    message( "INFO: Updating Charge list from PQP..." )
    # app.obj$start_plotting <- TRUE
    ## get unique charge state for current peptide selection
    app.obj$lib_df %>%
      dplyr::filter( MODIFIED_SEQUENCE==input$Mod ) %>%
      dplyr::select( PRECURSOR_CHARGE ) %>%
      unique() %>%
      as.list() -> unique_charges
    names(unique_charges) <- unique_charges
    ## Update charge selection to charges available for currently selected peptide sequence.  
    updateSelectizeInput( session, inputId = 'Charge', choices = unique_charges )
  } else if ( !is.null(app.obj$osw_df) ) {
    message( "INFO: Updating Charge list from OSW..." )
    # app.obj$start_plotting <- TRUE
    app.obj$osw_df %>%
      dplyr::filter( FullPeptideName==input$Mod ) %>%
      dplyr::select( Charge ) %>%
      unique() %>%
      as.list() -> unique_charges
    names(unique_charges) <- unique_charges
    ## Update charge selection to charges available for currently selected peptide sequence.  
    updateSelectizeInput( session, inputId = 'Charge', choices = unique_charges )
  }
}