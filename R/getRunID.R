#// **********************************************************************************************
#//                         getRunID.R
#// **********************************************************************************************
#//
#// 
#// **********************************************************************************************
#// @Maintainer: Justin Sing
#// @Author: Justin Sing

#' @export
#' @title Extract Run ID information from a osw file fiven a run name
#' @description This function can be used to extract the Run ID for a given run
#' 
#' @param oswfile A character vector of the absolute path and filename of the osw file. (Must be .osw format)
#' @param run_name A character vector for extraction of a specific run.
#' @return A data.table containing Run ID information
#' 
#' @author Justin Sing \url{https://github.com/singjc}
#' 
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite 
#' @importFrom dplyr collect tbl
#' @importFrom dbplyr sql 
#' @importFrom MazamaCoreUtils logger.isInitialized logger.info logger.error logger.warn logger.trace
getRunID_ <- function( oswfile, run_name ){
  
  ## Check if logging has been initialized
  if( !MazamaCoreUtils::logger.isInitialized() ){
    log_setup()
  }
  
  if ( F ) {
    # oswfile <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/PTMs_Project/Synth_PhosoPep/Justin_Synth_PhosPep/results/lower_product_mz_threshold/pyprophet/group_id/merged_runs_group_id_MS1MS2_intergration_ipf.osw"
    # run_name <- '/project/def-hroest/data/synth_phospho_pep/mzML/chludwig_K150309_013_SW_0.mzXML'
    oswfile <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/PTMs_Project/Christian_Doerig_Dataset/results/M_pools_re-run/pyprophet_parametric_no_precursor/merged_runs_group_id_MS1MS2_intergration.osw"
    run_name <- "lgillet_L160918_002-Manchester_dirty_phospho_-_Pool_M1_-_SW.mzML.gz"
  }
  
  MazamaCoreUtils::logger.info( paste("** Getting Run  ID Information for: ", run_name, " **\n" ))
  
  # Connect to database
  MazamaCoreUtils::logger.trace(sprintf("[getRunID_] Connecting To Database: %s\n", oswfile))
  osw_db <- DBI::dbConnect( RSQLite::SQLite(), oswfile )
  
  # Query statement
  stmt = paste("SELECT * FROM RUN WHERE RUN.FILENAME LIKE '%", run_name, "%'",sep='')
  
  # Query Databasse
  MazamaCoreUtils::logger.trace(sprintf("[getRunID_] Querying Database: %s\n", stmt))
  df <- dplyr::collect( dplyr::tbl(osw_db, dbplyr::sql(stmt)) )

  # Disconnect from database
  MazamaCoreUtils::logger.trace(sprintf("[getRunID_] Disconnecting From Database: %s\n", oswfile))
  DBI::dbDisconnect(osw_db)
  
  if (dim(df)[1]==0){
    MazamaCoreUtils::logger.error( paste((run_name), (' was not found in database!\n'), sep='' ))
  }
  return(df)
}