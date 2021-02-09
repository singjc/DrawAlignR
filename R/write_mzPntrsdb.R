#' @param mzPntrs (list) a list object containing nested lists of chromatogram data and chromatogram header information
#' @param outfile (char) a character vector of where to write database
#' export
write_mzPntrsdb <- function( mzPntrs, out_file=NULL ){
  if ( is.null(out_file) ){
    out_file <- "cached_chromatogram_data.mzPntrs"
  }
  
  con <- DBI::dbConnect(RSQLite::SQLite(), out_file)
  
  run_ids <- data.frame(run_id=names(mzPntrs),
                        filename=unlist(lapply(mzPntrs, `[[`, "filename")),
                        run_name=unlist(lapply(mzPntrs, `[[`, "run_name")))
  # DBI::dbCreateTable( conn = con, name = DBI::dbQuoteIdentifier(con, "run"), fields = "run_id" )
  DBI::dbWriteTable( conn = con, name = "run", value = run_ids )
  
  chromHead <- data.table::rbindlist(lapply(mzPntrs, `[[`, "chromHead"), idcol = "run_id")
  # DBI::dbCreateTable( conn = con, name = "chromHead", fields = c("run_id", "chromatogramId",  "chromatogramIndex"))
  DBI::dbWriteTable(conn = con, name = "chromHead", value = chromHead )
  
  mz <- data.table::rbindlist(lapply(mzPntrs, `[[`, "mz"), idcol = "run_id")
  # DBI::dbCreateTable( conn = con, name = "mz", fields = c("run_id", "CHROMATOGRAM_ID", "FRAGMENT_ID", "SPECTRUM_ID", "COMPRESSION", "DATA_TYPE", "DATA"))
  DBI::dbWriteTable( conn = con, name = "mz", value = mz )
  
  DBI::dbExecute( conn = con, "CREATE INDEX 'chromhead_idx' ON 'chromHead' (
	'run_id',
	'chromatogramId',
	'chromatogramIndex'
);" )
  
  DBI::dbExecute( conn = con, "CREATE INDEX 'mz_idx' ON 'mz' (
	'run_id',
	'CHROMATOGRAM_ID',
	'FRAGMENT_ID',
	'COMPRESSION',
	'DATA_TYPE',
	'DATA'
);"
  )
  
  DBI::dbDisconnect( con )
  
}