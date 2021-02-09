#// **********************************************************************************************
#//                         getTransitionScores.R
#// **********************************************************************************************
#//
#// 
#// **********************************************************************************************
#// @Maintainer: Justin Sing
#// @Author: Justin Sing


#' @export
#' @title Extract Transition Information from .osw results file
#' @description This function can be used to extract Transition Id and Transition Posterior Error Probability information from the OSW results file obtained from
#' running OpenSwathWorkflow
#' 
#' @param oswfile A character vector of the absolute path and filename of the osw results file. (Must be .osw format)
#' @param run_name A character vector for extraction of a specific run, this should be the same name as the file name in the .OSW RUN table.
#' @param precursor_id A numeric value for a specific precursor id to extract information for. (Default: '')
#' @param peptide_id A string vector indicatig a specific peptide to extract information for. (Default: '')
#' @param mod_peptide_id An array of two string vectors indicating a specific modified peptide sequence with both UniMod annotation and actual modification name to extract information for. I.e. c(ANS(Phos)SNSLK, ANS(UniMod:21)SNSLK) (Default: '') 
#' @return A data.table containing Transition IDs and Transition Posterior Error Probabilities, and other useful information. Similar to getOSWData.R.
#' 
#' @author Justin Sing \url{https://github.com/singjc}
#' 
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite 
#' @importFrom dplyr collect tbl
#' @importFrom dbplyr sql 
#' @importFrom MazamaCoreUtils logger.isInitialized logger.info logger.error logger.warn logger.trace
getTransitionScores_ <- function ( oswfile,
                                   run_name,
                                   precursor_id='',
                                   peptide_id='',
                                   mod_peptide_id=c('',''),
                                   detecting = TRUE,
                                   identifying = TRUE
) {
  
  ## Check if logging has been initialized
  if( !MazamaCoreUtils::logger.isInitialized() ){
    log_setup()
  }
  
  tryCatch(
    expr = {
      
      # Connect to database
      osw_db <- DBI::dbConnect( RSQLite::SQLite(), oswfile )
      
      MazamaCoreUtils::logger.trace(sprintf("[getTransitionScores_]\tConnected to %s\n", oswfile))
      MazamaCoreUtils::logger.trace(sprintf("[getTransitionScores_]\tInput Parameters:\noswfile: %s\nrun_name: %s\nprecursor_id: %s\npeptide_id: %s\n", oswfile, run_name, precursor_id, peptide_id))
      
      # Add to query statement for extracting data for a specific run, using run id.
      if ( run_name != '' ){
        run_id_df = getRunID_( oswfile, run_name )
        run_id_query = sprintf( "AND FEATURE.RUN_ID=(%s)", run_id_df$ID )
      } else {
        run_id_query = ''
      }
      
      # Add to query statement for extracting data for only a specific precursor, or extract everything
      if ( precursor_id != '' ){
        precursor_query = sprintf( "AND PRECURSOR.ID=(%s)", precursor_id )
      } else {
        precursor_query = ''
      }
      
      # Add to query statement for extracting data for only a specific peptide, or extract everything
      if ( peptide_id !='' ){
        peptide_query = sprintf( "AND PEPTIDE.UNMODIFIED_SEQUENCE=('%s')", peptide_id )	
      } else if ( mod_peptide_id[1]!='' & mod_peptide_id[2]!='' ){
        peptide_query = sprintf("AND ( PEPTIDE.MODIFIED_SEQUENCE=('%s') OR PEPTIDE.MODIFIED_SEQUENCE=('%s') )", mod_peptide_id[1], mod_peptide_id[2])
      } else {
        peptide_query = ''
      }
      
      # Construct Final Query Statement
  #     stmt = sprintf( 
  #   "SELECT PEPTIDE.MODIFIED_SEQUENCE || '_' || PRECURSOR.ID AS transition_group_id,
  #      PRECURSOR.DECOY AS decoy,
  #      RUN.ID AS run_id,
  #      RUN.FILENAME AS filename,
  #      FEATURE.EXP_RT AS RT,
  #      PRECURSOR.LIBRARY_RT AS lib_RT,
  #      FEATURE.ID AS id,
  #      PEPTIDE.UNMODIFIED_SEQUENCE AS Sequence,
  #      PEPTIDE.MODIFIED_SEQUENCE AS FullPeptideName,
  #      PRECURSOR.CHARGE AS Charge,
  #      PRECURSOR.PRECURSOR_MZ AS mz,
  #      FEATURE.LEFT_WIDTH AS leftWidth,
  #      FEATURE.RIGHT_WIDTH AS rightWidth,
  #      SCORE_TRANSITION.FEATURE_ID as transition_feature_id,
  # 	   SCORE_TRANSITION.PEP as transition_pep,
  # 	   SCORE_TRANSITION.PVALUE as transition_pval,
  # 	   SCORE_TRANSITION.QVALUE as transition_qval,
  # 	   SCORE_TRANSITION.RANK as transition_rank,
  # 	   SCORE_TRANSITION.SCORE as tranistion_score,
  # 	   SCORE_TRANSITION.TRANSITION_ID as transition_id
  #     FROM PRECURSOR
  #     INNER JOIN PRECURSOR_PEPTIDE_MAPPING ON PRECURSOR.ID = PRECURSOR_PEPTIDE_MAPPING.PRECURSOR_ID
  #     %s
  #     %s
  #     %s
  #     LEFT JOIN FEATURE_TRANSITION ON FEATURE_TRANSITION.FEATURE_ID = FEATURE.ID
  #     LEFT JOIN SCORE_TRANSITION ON SCORE_TRANSITION.FEATURE_ID = FEATURE.ID
  #     ORDER BY transition_group_id,
  #              transition_rank", peptide_query, precursor_query, run_id_query )
  #     
  #     
      stmt <- paste0(
            "SELECT 
                PEPTIDE.MODIFIED_SEQUENCE || '_' || PRECURSOR.ID AS transition_group_id,
                TRANSITION.DECOY AS decoy,
                RUN.ID AS run_id,
                RUN.FILENAME AS filename,
                replace(replace(RUN.FILENAME, rtrim(RUN.FILENAME, '1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM!@#$%^&*()_+-=`~[]\\{}|;:,.<>?'), '')
                     , '.' || replace(replace(RUN.FILENAME, rtrim(RUN.FILENAME, '1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM!@#$%^&*()_+-=`~[]\\{}|;:,.<>?'), '')
                                    , rtrim(replace(RUN.FILENAME, rtrim(RUN.FILENAME, '1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM!@#$%^&*()_+-=`~[]\\{}|;:,.<>?'), ''), replace(RUN.FILENAME, '.', '') )
                                    , '')
                     , '') AS run_name,
                FEATURE.EXP_RT AS RT,
                PRECURSOR.LIBRARY_RT AS lib_RT,
                FEATURE.ID AS feature_id,
                PEPTIDE.UNMODIFIED_SEQUENCE AS Sequence,
                PEPTIDE.MODIFIED_SEQUENCE AS FullPeptideName,
                PRECURSOR.CHARGE AS Charge,
                PRECURSOR.PRECURSOR_MZ AS mz,
                FEATURE.LEFT_WIDTH AS leftWidth,
                FEATURE.RIGHT_WIDTH AS rightWidth,
                FEATURE_TRANSITION.FEATURE_ID as transition_feature_id,
                SCORE_TRANSITION.PEP as transition_pep,
                SCORE_TRANSITION.PVALUE as transition_pval,
                SCORE_TRANSITION.QVALUE as transition_qval,
                SCORE_TRANSITION.RANK as transition_rank,
                SCORE_TRANSITION.SCORE as tranistion_score,
                TRANSITION.ID as transition_id
            FROM TRANSITION
            LEFT JOIN TRANSITION_PRECURSOR_MAPPING ON TRANSITION_PRECURSOR_MAPPING.TRANSITION_ID = TRANSITION.ID
            INNER JOIN PRECURSOR ON PRECURSOR.ID = TRANSITION_PRECURSOR_MAPPING.PRECURSOR_ID
            INNER JOIN PRECURSOR_PEPTIDE_MAPPING ON PRECURSOR.ID = PRECURSOR_PEPTIDE_MAPPING.PRECURSOR_ID
			      INNER JOIN PEPTIDE ON PRECURSOR_PEPTIDE_MAPPING.PEPTIDE_ID = PEPTIDE.ID
			      INNER JOIN FEATURE ON FEATURE.PRECURSOR_ID = PRECURSOR.ID
            LEFT JOIN FEATURE_TRANSITION ON (FEATURE_TRANSITION.TRANSITION_ID = TRANSITION.ID AND FEATURE_TRANSITION.FEATURE_ID = FEATURE.ID)
			      LEFT JOIN SCORE_TRANSITION ON (SCORE_TRANSITION.TRANSITION_ID=TRANSITION.ID AND SCORE_TRANSITION.FEATURE_ID = FEATURE.ID) 
            LEFT JOIN RUN ON RUN.ID = FEATURE.RUN_ID
            WHERE TRANSITION.DECOY=0 ",
            peptide_query, "\n" , "\n",
            precursor_query , "\n",
            run_id_query , "\n",
            " ORDER BY filename, 
			transition_group_id,
               feature_id,
			   --identifying,
			   transition_id" )
      
      MazamaCoreUtils::logger.trace(sprintf("[getTransitionScores_]\tQuery:\n%s\n", stmt))
      
      # Query Database
      tictoc::tic("Collecting Transition Level Data...")
      df_osw <- dplyr::collect( dplyr::tbl(osw_db, dbplyr::sql( stmt )) )
      tictoc::toc()
      ## Add run_name column with only filename with no extensions
      df_osw <- data.table::as.data.table( df_osw )
      # df_osw[, run_name := tstrsplit(basename(filename), ".", fixed=TRUE, keep=1L)]  
      
      MazamaCoreUtils::logger.trace(sprintf("[getTransitionScores_]\tdimension of dt results c(%s)\n", paste(dim(df_osw), collapse = ",")))
      
      # Disconnect form database
      DBI::dbDisconnect( osw_db )
      
      MazamaCoreUtils::logger.trace(sprintf("[getTransitionScores_]\tDisconnected from %s\n", oswfile))
      
      return( df_osw )
      
    },
    error = function(e){
      MazamaCoreUtils::logger.error(sprintf("[getTransitionScores_] There was the following error that occured during function call: %s\n", e$message))
    }
  ) # End tryCatch
  
}
