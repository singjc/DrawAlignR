if ( F ){

  filename <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR_Test_Data/Synthetic_Dilution_Phosphoproteomics_Full/pqp/psgs_phospho_optimized_decoys.pqp"

  tictoc::tic("PQP Library Extraction took:")
  lib <- DrawAlignR:::pqp_data_access( filename = filename )
  tictoc::toc()
  
  tictoc::tic("PQP Library Extraction took:")
  lib <- pqp_data_access( filename = filename, unmodified_sequence = 'ANSSPTTNIDHLK' )
  tictoc::toc()
  
  tictoc::tic("PQP Library Extraction took:")
  lib <- pqp_data_access( filename = filename, modified_sequence =  c('ANS(Phos)SNSLK', 'ANS(UniMod:21)SNSLK') )
  tictoc::toc()
  
  tictoc::tic("PQP Library Extraction took:")
  lib <- pqp_data_access( filename = filename, filter_decoy = 1 )
  tictoc::toc()
  
}

setClass(Class="pqp_data_access", 
         slots=list(
           filename="character",
           conn="SQLiteConnection",
           assay_data="list"
         ))

#' Generic pqp Data Access Function
#' 
#' This function will dispatch one of the method-family functions based on input arguments. \cr
#' The main purpose of this function is to extract chromatographic data stored in the sqMass sql-based database. \cr
#' Get data from multiple chromatograms \cr
#' - compression is one of 0 = no, 1 = zlib, 2 = np-linear, 3 = np-slof, 4 = np-pic, 5 = np-linear + zlib, 6 = np-slof + zlib, 7 = np-pic + zlib \cr
#' - data_type is one of 0 = mz, 1 = int, 2 = rt \cr
#' - data contains the raw (blob) data for a single data array \cr
#' @param filename (character) A character/string of the sqMass database file
#' @param init (logical) A logical value to establish a connection to the database, and return an S4 object with the connection
#' @param obj (S4) An S4 object that contains the filename, connection to the database, and the chromatoram data
#' @param filter_decoy (integer) An integer of 0 or 1. 0 will filter for only targets, 1 will filter for only decoys. Default: 0 
#' @param unmodified_sequence (numeric) A character vector for extraction of a specific peptide. I.e. 'ANSSPTTNIDHLK'
#' @param modified_sequence (chracter) An array of two string vectors indicating a specific modified peptide sequence with both UniMod annotation and actual modification name to extract information for. I.e. c(ANS(Phos)SNSLK, ANS(UniMod:21)SNSLK) (Default: '')
#' @export
setGeneric("pqp_data_access", 
           function(filename, 
                    init=NULL,
                    obj=NULL, 
                    filter_decoy=0, 
                    unmodified_sequence="",
                    modified_sequence="", ...){  standardGeneric("pqp_data_access") })

#' Establish Connection to sqMass database
#' 
#' @name sqmassdbConnection
#' @aliases pqp_data_access-method
#' @docType methods
#' @rdname pqp_data_access
setMethod("pqp_data_access",
          signature=c(filename="character", 
                      init="logical",
                      obj="missing", 
                      filter_decoy="missing",
                      unmodified_sequence="missing",
                      modified_sequence="missing"),
          function( filename, init ){
            # Create an ephemeral in-memory RSQLite database
            conn <- DBI::dbConnect(RSQLite::SQLite(), filename)
            obj.out <- new("pqp_data_access", filename=filename, conn=conn  )
          }
)

#' Extract All Chromatograms from sqMass database
#' 
#' Get data from multiple chromatograms 
#' 
#' @name getDataForAllChromatograms
#' @aliases pqp_data_access-method
#' @docType methods
#' @rdname pqp_data_access
#' @export
setMethod("pqp_data_access",
          signature = c(filename="character", 
                        init="missing",
                        obj="missing", 
                        filter_decoy="ANY",
                        unmodified_sequence="ANY",
                        modified_sequence="ANY"),
          function( filename, filter_decoy=0, unmodified_sequence="", modified_sequence="" ){
            obj <- pqp_data_access( filename=filename, init=TRUE )
            # Add Query statement to extract a specific peptide
            if ( unmodified_sequence !='' ){
              peptide_query = sprintf( "INNER JOIN PEPTIDE ON PRECURSOR_PEPTIDE_MAPPING.PEPTIDE_ID = PEPTIDE.ID AND PEPTIDE.UNMODIFIED_SEQUENCE=('%s')", unmodified_sequence )	
            } else if ( modified_sequence[1]!='' & modified_sequence[2]!='' ){
              peptide_query = sprintf("INNER JOIN PEPTIDE ON PRECURSOR_PEPTIDE_MAPPING.PEPTIDE_ID = PEPTIDE.ID AND ( PEPTIDE.MODIFIED_SEQUENCE=('%s') OR PEPTIDE.MODIFIED_SEQUENCE=('%s') )", modified_sequence[1], modified_sequence[2])
            } else {
              peptide_query = 'INNER JOIN PEPTIDE ON PRECURSOR_PEPTIDE_MAPPING.PEPTIDE_ID = PEPTIDE.ID'
            }
            # Create Query
            lib_query =  sprintf( "
                                    SELECT TRANSITION.ID AS TRANSITION_ID,
                                        TRANSITION.TRAML_ID,
                                        TRANSITION.PRODUCT_MZ,
                                        TRANSITION.CHARGE,
                                        TRANSITION.TYPE,
                                        TRANSITION.ORDINAL,
                                        TRANSITION.DETECTING,
                                        TRANSITION.IDENTIFYING,
                                        TRANSITION.QUANTIFYING,
                                        TRANSITION.LIBRARY_INTENSITY,
                                        TRANSITION.DECOY,
                                        TRANSITION_PRECURSOR_MAPPING.PRECURSOR_ID,
                                        PRECURSOR_PEPTIDE_MAPPING.PEPTIDE_ID,
                                        PEPTIDE.MODIFIED_SEQUENCE,
                                        PEPTIDE.UNMODIFIED_SEQUENCE,
                                        PRECURSOR.LIBRARY_RT AS PRECURSOR_LIBRARY_RT,
                                        PRECURSOR.PRECURSOR_MZ,
                                        PRECURSOR.CHARGE AS PRECURSOR_CHARGE 
                                    FROM TRANSITION
                                    INNER JOIN TRANSITION_PRECURSOR_MAPPING ON TRANSITION.ID=TRANSITION_PRECURSOR_MAPPING.TRANSITION_ID AND TRANSITION.DECOY=%s
                                    INNER JOIN PRECURSOR_PEPTIDE_MAPPING ON TRANSITION_PRECURSOR_MAPPING.PRECURSOR_ID=PRECURSOR_PEPTIDE_MAPPING.PRECURSOR_ID
                                    %s
                                    INNER JOIN PRECURSOR ON PRECURSOR.ID=PRECURSOR_PEPTIDE_MAPPING.PRECURSOR_ID", filter_decoy, peptide_query)
            # Query Databasse
            df_lib <- dplyr::collect( dplyr::tbl( obj@conn, dbplyr::sql(lib_query) ) )
            
            # Append class pqp.data.frame
            # class(df_lib) <- c(class(df_lib), 'pqp.data.frame')
            
            # Close Connection to database
            DBI::dbDisconnect(obj@conn)
            say_closed <- "Closed Connection"
            class(say_closed) <- "SQLiteConnection"
            obj@conn <- say_closed
            return(df_lib)
          }
)
