if ( F ){
  filename <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR_Test_Data/Spyogenes_Small_Subset/sqmass/hroest_K120808_Strep10%PlasmaBiolRepl1_R03_SW_filt.chrom.sqMass"
  filename <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR_Test_Data/Synthetic_Dilution_Phosphoproteomics/sqmass/chludwig_K150309_013_SW_0.chrom.sqMass"
  filename <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR_Test_Data/Synthetic_Dilution_Phosphoproteomics_Full/sqmass/chludwig_K150309_013_SW_0.chrom.sqMass"
  filename <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/PTMs_Project/phospho_enriched_U2OS/Results/06292020_new_workflow/sqmass/yanliu_20180406_Phos_Noco_10.chrom.sqMass"
  ids <- c(0,1)
  native_id <- "103114"
  native_id <- c("103114", "342683" )
  tictoc::tic("Chromatogram Extraction took:")
  results <- sqmass_data_access( filename )
  tictoc::toc()
  tictoc::tic("Chromatogram Extraction took:")
  results <- sqmass_data_access( filename=filename, ids=ids )
  tictoc::toc()
  tictoc::tic("Chromatogram Extraction took:")
  results <- sqmass_data_access( filename=filename, native_id=native_id )
  tictoc::toc()
  filename <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR_Test_Data/Synthetic_Dilution_Phosphoproteomics_Small_Subset/sqmass/chludwig_K150309_012_SW_1_1.chrom.sqMass"
  native_id <- c("174", "175", "176", "177", "178", "179")
  
  
}

setClass(Class="sqmass_data_access", 
         slots=list(
           filename="character",
           conn="SQLiteConnection",
           chrom_data="list"
           ))

#' Generic sqMass Data Access Function
#' 
#' This function will dispatch one of the method-family functions based on input arguments. \cr
#' The main purpose of this function is to extract chromatographic data stored in the sqMass sql-based database. \cr
#' Get data from multiple chromatograms \cr
#' - compression is one of 0 = no, 1 = zlib, 2 = np-linear, 3 = np-slof, 4 = np-pic, 5 = np-linear + zlib, 6 = np-slof + zlib, 7 = np-pic + zlib \cr
#' - data_type is one of 0 = mz, 1 = int, 2 = rt \cr
#' - data contains the raw (blob) data for a single data array \cr
#' @param filename (character) A character/string of the sqMass database file
#' @param return_id (character) One of two options TRANSITION_ID or CHROMATOGRAM_ID. This will name list of chromatogram data with either transition ids or chromatogram ids.
#' @param init (logical) A logical value to establish a connection to the database, and return an S4 object with the connection
#' @param obj (S4) An S4 object that contains the filename, connection to the database, and the chromatoram data
#' @param data (data.frame) A data.frame that contains the CHROMATOGRAM table information from the database
#' @param ids (numeric) A numeric vector of CHROMATOGRAM_ID(s) that you want to extract chromatogram data for
#' @param native_id (chracter) A chracter vector of NATIVE_ID(s) that you want to extract a chromatogram data for
#' @export
setGeneric("sqmass_data_access", 
           function(filename, 
                    return_id="TRANSITION_ID",
                    init=NULL,
                    obj=NULL, 
                    data=NULL, 
                    ids=NULL,
                    native_id=NULL, ...){  standardGeneric("sqmass_data_access") })

#' Establish Connection to sqMass database
#' 
#' @name sqmassdbConnection
#' @aliases sqmass_data_access-method
#' @docType methods
#' @rdname sqmass_data_access
setMethod("sqmass_data_access",
          signature=c(filename="character",
                      return_id="missing",
                      init="logical",
                      obj="missing", 
                      data="missing",
                      ids="missing",
                      native_id="missing"),
          function( filename, init ){
            # Create an ephemeral in-memory RSQLite database
            conn <- DBI::dbConnect(RSQLite::SQLite(), filename)
            obj.out <- new("sqmass_data_access", filename=filename, conn=conn  )
          }
)

#' Method for actual RT/Intensity Data Extraction and Decompression
#' 
#' @name returnDataForChromatogram
#' @aliases sqmass_data_access-method
#' @docType methods
#' @rdname sqmass_data_access
setMethod("sqmass_data_access",
          signature = c(filename="missing", 
                        return_id="character",
                        init="missing",
                        obj="sqmass_data_access", 
                        data="data.frame",
                        ids="missing",
                        native_id="missing"),
          function( return_id, obj, data ){
            # Prepare result
            if ( return_id=="CHROMATOGRAM_ID" ){
              chr_ids <- unique(data$CHROMATOGRAM_ID)
            } else {
              chr_ids <- unique(data$TRANSITION_ID)
            }
            res <- list()
            
            for (row in seq(1, nrow(data))){
              data_row <- data[row,]
              if (data_row$COMPRESSION==5){
                result <- tryCatch( expr = {
                  result <- RMSNumpress::decodeLinear( as.raw(Rcompression::uncompress( data_row$DATA[[1]], asText = FALSE )) )
                }, error = function(e){
                  warning( sprintf("Was unable to decode np-linear data for chromatogram_id: %s.\nThis is most likely because the encoded data is small.\nOriginal error: %s", data_row$CHROMATOGRAM_ID, e$message) )
                  return("Empty Chromatogram!")
                })
              }
              if (data_row$COMPRESSION==6){
                result <- tryCatch( expr = {
                  result <- RMSNumpress::decodeSlof( as.raw(Rcompression::uncompress( data_row$DATA[[1]], asText = FALSE )) )
                }, error = function(e){
                  warning( sprintf("Was unable to decode np-slof data for chromatogram_id: %s.\nThis is most likely because the encoded data is small.\nOriginal error: %s", data_row$CHROMATOGRAM_ID, e$message) )
                  return("Empty Chromatogram!")
                })
              }
              
              if (data_row$DATA_TYPE==1){
                res[[as.character(data_row[[return_id]])]]["int"] <- list(result)
              } else if (data_row$DATA_TYPE==2){
                res[[as.character(data_row[[return_id]])]]["rt"] <- list(result)
              } else {
                stop("Only expected RT or Intensity data for chromatogram")
              }
            }
            return( res )
          }
)

#' Extract All Chromatograms from sqMass database
#' 
#' Get data from multiple chromatograms 
#' 
#' @name getDataForAllChromatograms
#' @aliases sqmass_data_access-method
#' @docType methods
#' @rdname sqmass_data_access
setMethod("sqmass_data_access",
          signature = c(filename="character", 
                        return_id="character",
                        init="missing",
                        obj="missing", 
                        data="missing",
                        ids="missing",
                        native_id="missing"),
          function( filename, return_id  ){
            obj <- sqmass_data_access( filename=filename, init=TRUE )
            stmt <- paste( "SELECT CHROMATOGRAM_ID, COMPRESSION, DATA_TYPE, DATA FROM DATA" )
            execute_query <- DBI::dbSendQuery(obj@conn, stmt)
            data <- DBI::dbFetch( execute_query )
            DBI::dbClearResult(execute_query)
            # TODO: Need to parallize this, or make it more efficient.
            # It's probably not good to have this method in the first place, but it's available if the use wants to extract all of the chromatograms from the database
            chrom_data <- sqmass_data_access( return_id=return_id, obj=obj, data=data )
            obj@chrom_data <- chrom_data
            # Close Connection to database
            DBI::dbDisconnect(obj@conn)
            say_closed <- "Closed Connection"
            class(say_closed) <- "SQLiteConnection"
            obj@conn <- say_closed
            return(chrom_data)
          }
)

#' Extract Chromtograms for a given list of Chromatogram IDs
#' 
#' Get data from multiple chromatograms or a single chromatogram given a list of chromatogram IDs 
#' 
#' @name getDataForChromatogramsFromID
#' @aliases sqmass_data_access-method
#' @docType methods
#' @rdname sqmass_data_access
setMethod("sqmass_data_access",
          signature = c(filename="character", 
                        return_id="character",
                        init="missing",
                        obj="missing", 
                        data="missing",
                        ids="numeric",
                        native_id="missing"),
          function( filename, return_id, ids ){
            obj <- sqmass_data_access( filename=filename, init=TRUE )
            stmt <- paste( "SELECT CHROMATOGRAM_ID, COMPRESSION, DATA_TYPE, DATA FROM DATA WHERE CHROMATOGRAM_ID IN (", paste(ids, collapse = ", "), ")", sep="" )
            execute_query <- DBI::dbSendQuery(obj@conn, stmt)
            data <- DBI::dbFetch( execute_query )
            DBI::dbClearResult(execute_query)
            chrom_data <- sqmass_data_access( return_id=return_id, obj=obj, data=data )
            obj@chrom_data <- chrom_data
            # Close Connection to database
            DBI::dbDisconnect(obj@conn)
            say_closed <- "Closed Connection"
            class(say_closed) <- "SQLiteConnection"
            obj@conn <- say_closed
            return(chrom_data)
          }
)

#' Extract Chromtograms for a given list of Native IDs
#' 
#' Get data from multiple chromatograms or a single chromatogram given a list of native IDs 
#' 
#' @name getDataForChromatogramFromNativeId
#' @aliases sqmass_data_access-method
#' @docType methods
#' @rdname sqmass_data_access
setMethod("sqmass_data_access",
          signature = c(filename="character", 
                        return_id="character",
                        init="missing",
                        obj="missing", 
                        data="missing",
                        ids="missing",
                        native_id="character"),
          function( filename, return_id, native_id ){
            tryCatch( expr = {
              obj <- sqmass_data_access( filename=filename, init=TRUE )
              stmt <- paste( "SELECT CHROMATOGRAM_ID, NATIVE_ID as TRANSITION_ID, COMPRESSION, DATA_TYPE, DATA FROM DATA INNER JOIN CHROMATOGRAM ON CHROMATOGRAM.ID = CHROMATOGRAM_ID WHERE NATIVE_ID IN (", paste(native_id, collapse = ", "), ")", sep="" )
              MazamaCoreUtils::logger.trace( paste0("[sqmass_data_access::getDataForChromatogramFromNativeId] stmt: ", stmt) )
              #stmt <- paste( "SELECT CHROMATOGRAM_ID, COMPRESSION, DATA_TYPE, DATA FROM DATA INNER JOIN CHROMATOGRAM ON CHROMATOGRAM.ID = CHROMATOGRAM_ID WHERE NATIVE_ID = ", native_id, sep="" )
              MazamaCoreUtils::logger.trace( paste0("[sqmass_data_access::getDataForChromatogramFromNativeId] Send query") )
              execute_query <- DBI::dbSendQuery(obj@conn, stmt)
              MazamaCoreUtils::logger.trace( paste0("[sqmass_data_access::getDataForChromatogramFromNativeId] Fetch query results") )
              data <- DBI::dbFetch( execute_query )
              DBI::dbClearResult(execute_query)
              chrom_data = sqmass_data_access( return_id=return_id, obj=obj, data=data )
              MazamaCoreUtils::logger.trace( paste0("[sqmass_data_access::getDataForChromatogramFromNativeId] length(chrom_data): ", length(chrom_data)) )
              obj@chrom_data <- chrom_data
              # Close Connection to database
              DBI::dbDisconnect(obj@conn)
              say_closed <- "Closed Connection"
              class(say_closed) <- "SQLiteConnection"
              obj@conn <- say_closed
              return(chrom_data)
            }, error=function(e){
              MazamaCoreUtils::logger.error( paste0("[sqmass_data_access::getDataForChromatogramFromNativeId] The following error occured: ", e$message) )
              # stop()
            })
            
            
          }
)
