## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


#' Get string information for Maintainer
#' @param email A character vector for email address to contact maintainer
#' @param github A character vector for github repository to submit issue/feature requests
#' @return A chracter vector with Maintainer information
#' @export
getMaintainer <- function(email='justincsing@gmail.com', github='https://github.com/singjc/DrawAlignR'){
  sprintf('%s (%s)\n', email, utils::URLdecode(github))
}

#' Get string of function call
#' @param func_args_list A list object containing information of arguments to a function call
#' @return A chracter vector representin function call
#' @export
getFunctionCallArgs <- function( func_args_list ){
  
  function_name <- as.character( func_args_list[[1]] )
  
  function_args <- func_args_list[-1]
  
  # print( eval(function_args[["analytes"]]) )
  
  function_args_names <- as.character(names( function_args ))
  
  arg_evals <- unlist(lapply(function_args_names, function( arg_name ){
    list_obj <- eval(getListObj(function_args, arg_name))
    class_check <- (is.character(list_obj) | is.numeric(list_obj) | is.logical(list_obj))
    is_character_check <- is.character(list_obj)
    ## Check if list obj is more than one element
    if ( length(list_obj)>1 & is_character_check ) {
      list_obj <- paste0( "c(", paste0( paste0("'", list_obj, "'"), collapse = ', '), ")" )
    } else if ( length(list_obj)==1 & is_character_check ) {
      list_obj <- paste0("'", list_obj, "'")
    } else if ( length(list_obj)>1 & !is_character_check ) {
      list_obj <- paste0( "c(", paste0( list_obj, collapse = ', '), ")" )
    }
    
    sprintf( "%s = %s", arg_name, ifelse( class_check, 
                                          list_obj, 
                                          class(list_obj) ) ) 
  }))
  
  # print(function_name)
  # print(paste(arg_evals, collapse = ', '))
  
  sprintf( "Function Call:\n\n%s( %s )\n", function_name, paste(arg_evals, collapse = ', ') )
  
}


#' Fetch the reference run-index.
#'
#' Provides the reference run-index based on lowest m-score.
#' @importFrom dplyr %>%
#' @author Shubham Gupta, \email{shubh.gupta@mail.utoronto.ca}
#'
#' ORCID: 0000-0003-3500-8152
#'
#' License: (c) Author (2019) + MIT
#' Date: 2019-12-13
#' @param oswFiles (list of data-frames) it is output from getOswFiles function.
#' @param analyte (string) analyte is as PRECURSOR.GROUP_LABEL or as PEPTIDE.MODIFIED_SEQUENCE and PRECURSOR.CHARGE from osw file.
#' @return An integer
#' @examples
#' data(oswFiles_DIAlignR, package="DIAlignR")
#' \dontrun{
#' getRefRun(oswFiles = oswFiles_DIAlignR, analyte = "AQPPVSTEY_2")
#' getRefRun(oswFiles = oswFiles_DIAlignR, analyte = "14299_QFNNTDIVLLEDFQK/3")
#' }
#' @seealso \code{\link{getOswFiles}, \link{getOswAnalytes}}
#' @export
getRefRun <- function(oswFiles, analyte){
  # Select reference run based on m-score
  minMscore <- 1
  refRunIdx <- NULL
  for(runIdx in seq_along(oswFiles)){
    ## Filter first on m_score to find the best candiate peptide feature
    m_score <- oswFiles[[runIdx]] %>%
      # dplyr::filter(transition_group_id == analyte )
      dplyr::group_by( transition_group_id ) %>%
      dplyr::filter(transition_group_id == analyte & m_score==min(m_score) ) 
    ## Check to see if there are still more than one peak group per peptide option.
    ## If there is then do a second pass filter for the lowest peakgroup rank
    if ( dim(m_score)[1]>1 ){
      m_score %>%
        dplyr::filter(transition_group_id == analyte & peak_group_rank==min(peak_group_rank) ) -> m_score
      if ( dim(m_score)[1]>1 ){
        m_score %>%
          dplyr::filter( dplyr::row_number()==1 ) -> m_score
      }
    }
    ## Extract on the m_score
    m_score %>%
      dplyr::ungroup() %>% .$m_score -> m_score
    # Check for numeric(0) condition and proceed.
    tryCatch(
      expr = {
        if(length(m_score) != 0){
          if(m_score < minMscore){
            minMscore <- m_score
            refRunIdx <- runIdx
          }
        }
      }, 
      error = function(e){
        message( sprintf("[DrawAlignR::utils::getRefRun] There was an error that occured during reference run index extraction.\n%s", e$message))
      }
    )
    
  }
  ## Return refRunIdx
  refRunIdx
}

#' Chromatogram indices of analyte in a run.
#'
#' select chromatogram indices by matching analyte and runname in oswFiles.
#' @importFrom dplyr %>%
#' @author Shubham Gupta, \email{shubh.gupta@mail.utoronto.ca}
#'
#' ORCID: 0000-0003-3500-8152
#'
#' License: (c) Author (2019) + MIT
#' Date: 2019-12-13
#' @param oswFiles (list of data-frames) it is the output from getOswFiles function.
#' @param runname (string) Must be a combination of "run" and an iteger e.g. "run2".
#' @param analyte (string) analyte is as PRECURSOR.GROUP_LABEL or as PEPTIDE.MODIFIED_SEQUENCE and PRECURSOR.CHARGE from osw file.
#' @return A vector of Integers
#' @examples
#' data(oswFiles_DIAlignR, package="DIAlignR")
#' \dontrun{
#' selectChromIndices(oswFiles = oswFiles_DIAlignR, runname = "run2", analyte = "AQPPVSTEY_2")
#' selectChromIndices(oswFiles = oswFiles_DIAlignR, runname = "run0",
#'  analyte = "14299_QFNNTDIVLLEDFQK/3")
#' }
#' @seealso \code{\link{getOswFiles}, \link{getOswAnalytes}}
#' @export
selectChromIndices <- function(oswFiles, runname, analyte, product_mz_filter_list=NULL, return_index="chromatogramIndex",  keep_all_detecting=T){
  
  ## TMP Fix
  ## Second pass filter to ensure only one analyte is being mapped once to the same peak
  ## There are cases for ipf where different assays would result in the same peptide being mapped to the same peak multiple times due to being the winning hypothesis
  oswFiles[[runname]] %>%
    dplyr::group_by( transition_group_id, filename ) %>%
    dplyr::add_count() %>%
    dplyr::ungroup() -> tmp
  tmp %>%
    dplyr::group_by( transition_group_id, filename ) %>%
    dplyr::filter( ifelse( n>1, ifelse(m_score==min(m_score), T, F), T ) ) -> tmp
  ## Remove count column
  tmp$n <- NULL
  ## count again to check if there are sitll more than one entry
  tmp %>%
    dplyr::group_by( transition_group_id, filename ) %>%
    dplyr::add_count() %>%
    dplyr::ungroup() -> tmp
  ## Remove count column
  tmp$n <- NULL
  
  # Pick chromatrogram indices from osw table.
  chromIndices <- tmp %>%
    dplyr::filter(transition_group_id == analyte) %>% dplyr::pull( !!rlang::sym(return_index) )
  
  # Check for character(0) condition and proceed.
  if(length(chromIndices) != 0){
    ### TODO: Make this more stream-line
    if ( !is.null(product_mz_filter_list) ){
      oswFiles[[runname]] %>%
        dplyr::filter(transition_group_id == analyte) %>%
        dplyr::filter( m_score == min(m_score) ) %>%
        dplyr::filter( peak_group_rank==min(peak_group_rank) ) %>%
        dplyr::slice(1L) %>%
        tidyr::separate_rows( product_mz, detecting_transitions, identifying_transitions, chromatogramIndex, transition_ids ) %>%
        dplyr::mutate( product_mz_detecting=paste(product_mz, detecting_transitions, sep='_') ) -> tmp_long_oswFiles
      ## Should all detecting transitions be forced to be kept even if mz is not overlapping
      if ( keep_all_detecting ){
        transition_boolean_filter <- (tmp_long_oswFiles$product_mz_detecting %in% product_mz_filter_list) | tmp_long_oswFiles$detecting_transitions==1
      } else {
        transition_boolean_filter <- (tmp_long_oswFiles$product_mz_detecting %in% product_mz_filter_list)
      }
      
      tmp_long_oswFiles %>%
        dplyr::filter( transition_boolean_filter ) %>% 
        unique() %>%
        dplyr::pull( !!rlang::sym(return_index) ) %>% as.integer() -> chromIndices
      
    } else {
      chromIndices <- as.integer(strsplit(chromIndices, split = ",")[[1]])
    }
  } else {
    return(NULL)
  }
  if(any(is.na(chromIndices))){
    # Indices for one or more fragment-ions are missing. This could happen if .chrom.mzML file doesn't have them.
    return(NULL)
  }
  # Select the first row if there are many peak-groups.
  chromIndices
}

#' Find if an object exists in a list
#' Searches name structure for name search parameter
#' @param x A list object
#' @param name Name of sub-components in list
#' @return logical value if name is present in list
#' @export
isListObj <- function(x, name) {
  pos <- match(name, names(x))
  if (!is.na(pos)){ return(TRUE) } else { return(FALSE) }
  for (el in x) {
    if (class(el) == "list") {
      out <- Recall(el, name)
      if (!is.null(out)) return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' Find and Extract an obj from a structured list
#' @param x A list object
#' @param name Name of sub-components in list
#' @return returns the named object in list
#' @export
getListObj <- function(x, name) {
  pos <- match(name, names(x))
  if (!is.na(pos)) return(x[[pos]])
  for (el in x) {
    if (class(el) == "list") {
      out <- Recall(el, name)
      if (!is.null(out)) return(out)
    }
  }
}

#' check_sqlite_table
#' @param conn Connection to database
#' @param table Character vector to test for, if present in database
#' @param msg A character to pre-append to stop error message. (Optional)
#' @return Logical value, TRUE if table is present
#' 
#' @importFrom DBI dbExistsTable
#' @export
check_sqlite_table <- function( conn, table, msg="" ) {
  if( !DBI::dbExistsTable( conn, table ) ){
    out.msg <- sprintf("%s An Error occured! There was no %s Table found in %s.\nCheck to see if you are using the right file, or if the file is corrupted.\n", msg, table, conn@dbname)
    stop( out.msg, call.=FALSE )
  }  
}

#' Check is SCORE_IPF is in database
#' @param oswFile A character vector pointing to path of osw file
#' @return returns a logical value if SCORE_IPF is present or not
#' @export
Score_IPF_Present <- function( oswFile ){
  db <- DBI::dbConnect( RSQLite::SQLite(), oswFile )
  if ( DBI::dbExistsTable( db, "SCORE_IPF" ) ){
    use_ipf_score <- TRUE
  } else {
    use_ipf_score <- FALSE
  }
  DBI::dbDisconnect( db )
  return( use_ipf_score )
}

#' Convert Variable to character including NULL
#' @param x An object to coerce to character
#' @return returns a character vector
#' @export
as_character_null <- function( x ){
  
  if ( is.null(x) ){
    return( 'NULL' )
  } else {
    return( as.character( x ) )
  }
  
}

#' @description Check Dataframe, if empty return an object and stop function call
#' 
#' @param df A data.frame/data.table/matrix object to check for number of rows
#' @param return_item 
#' @param msg error message to return
#' @return if data.frame has 0 rows, return true, otherwise return false
checkDataframe <- function( df, return_item, msg ){
  if ( dim(df)[1]==0 ){
    MazamaCoreUtils::logger.error(crayon::bold(crayon::red(msg)))
    return( TRUE )
  } else {
    return( FALSE )
  }
}

#' @description  Check numeric values if not NULL
#' 
#' @param numeric_obj Check if an numeric object is not NULL 
#' @param  signif.not Return significant notation
#' @return If numeric object is not null, round numeric object to 4 digits
checkNumeric <- function( numeric_obj, signif.not=TRUE ){
  if( !is.null( numeric_obj ) ){
    if ( signif.not==FALSE ){
      return( signif(numeric_obj, digits = 4) )
    } else {
      return( formatC(numeric_obj, format = "e", digits = 3) )
    }
  } else {
    return( NULL )
  }
}

#' Convert list object to printable character vectory
#' @param list_obj A list object to coerce to character
#' @param collapse A character vector to collapse characters on
#' @return returns a character vector
#' @export
listTostring <- function( list_obj, collapse = '\n' ){
  paste( paste(names(list_obj), list_obj, sep=' = '), collapse = collapse )
}

testAlignObj <- function(analyteInGroupLabel = FALSE){
  if(analyteInGroupLabel){
    AlignObj <- new("AffineAlignObjLight",
                    indexA_aligned = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,0,20,0,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,0,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,0,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,0,162,0,163,164,165,166,0,167,0,168,169,170,171,172,173,174,175,176),
                    indexB_aligned = c(0,0,0,1,0,2,0,3,0,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,0,92,0,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176),
                    score = c(0,0,0,2.675751,2.385165,4.745081,4.454496,6.67706,6.386474,9.641135,15.48268,26.87968,46.67034,77.78939,121.8607,170.8698,214.2358,244.4554,262.8537,262.5631,270.8538,270.5632,288.2477,319.3287,364.6418,413.4873,453.1931,479.6588,496.5207,506.6607,512.7442,515.267,516.8242,518.2747,519.8424,521.2872,522.6472,524.2912,525.6285,526.5892,526.9713,527.1531,527.4022,527.1116,530.9457,547.7525,588.2834,658.8819,748.3079,833.8337,898.3289,935.5809,948.8015,952.0709,952.8035,953.4267,954.0863,954.8143,955.4842,956.0834,956.802,957.535,958.2853,959.0355,959.7972,960.7983,961.8922,963.0142,964.2597,965.5837,966.878,968.0037,968.4412,968.1507,968.1958,968.9242,985.3144,1085.833,1364.976,1846.928,2409.31,2869.416,3132.509,3231.061,3257.015,3264.422,3269.377,3275.003,3282.515,3290.524,3297.864,3304.43,3310.324,3314.403,3316.806,3317.992,3318.933,3318.642,3319.328,3319.038,3320.17,3321.781,3323.71,3325.64,3327.855,3330.382,3332.989,3335.319,3337.555,3339.96,3342.381,3344.48,3346.456,3348.605,3350.446,3352.092,3353.829,3355.911,3358.256,3360.576,3363.292,3367.099,3372.687,3380.124,3389.957,3401.498,3414.81,3428.762,3441.046,3451.052,3459.235,3466.392,3473.212,3480.14,3490.173,3506.584,3530.062,3561.003,3595.718,3624.828,3642.574,3650.352,3653.893,3656.295,3658.798,3661.361,3663.704,3665.936,3667.714,3669.478,3670.721,3671.991,3673.278,3674.689,3676.068,3677.317,3678.688,3680.062,3681.513,3683.097,3684.786,3686.565,3688.24,3689.741,3690.859,3690.568,3691.496,3691.205,3692.31,3693.465,3694.458,3695.352,3695.061,3695.892,3695.602,3696.512,3697.468,3698.18,3698.799,3699.363,3699.94,3700.634,3701.585,3702.988))
  } else {
    AlignObj <- new("AffineAlignObjLight",
                    indexA_aligned = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,0,52,0,53,0,54,0,55,0,56,0,57,0,58,0,59,0,60,0,61,0,62,0,63,0,64,0,65,0,66,0,67,0,68,0,69,0,70,0,71,0,72,0,73,0,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,0,162,0,163,164,165,166,0,167,0,168,169,170,171,172,173,174,175,176),
                    indexB_aligned = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,0,10,0,11,0,12,0,13,0,14,0,15,0,16,0,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,0,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176),
                    score = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8.258636,21.70066,37.77944,56.28491,77.49291,102.7556,131.1216,159.8879,186.0392,185.7486,199.3454,199.0548,208.9874,208.6968,224.2224,223.9318,235.3893,235.0987,239.616,239.3254,239.8545,239.5639,239.7825,239.4919,264.0039,345.6801,518.224,760.9532,1002.997,1171.843,1255.645,1283.27,1289.328,1289.037,1289.66,1289.37,1289.694,1289.404,1289.829,1289.538,1290.128,1289.838,1290.421,1290.13,1290.703,1290.413,1291.675,1291.385,1292.812,1292.522,1293.994,1293.704,1295.055,1294.764,1295.694,1295.403,1296.342,1296.051,1297.042,1296.752,1297.599,1297.308,1298.15,1297.859,1298.766,1298.475,1299.462,1299.172,1300.227,1299.936,1300.543,1300.252,1300.388,1300.097,1300.442,1300.151,1301.528,1301.238,1350.073,1535.732,1925.536,2471.722,2995.898,3355.553,3518.7,3568.132,3579.685,3585.838,3592.471,3599.173,3606.964,3614.774,3621.711,3627.712,3632.502,3635.622,3637.255,3638.218,3638.903,3639.589,3639.299,3640.431,3642.042,3643.971,3645.901,3648.116,3650.643,3653.25,3655.58,3657.816,3660.221,3662.642,3664.741,3666.717,3668.866,3670.707,3672.353,3674.09,3676.172,3678.516,3680.837,3683.553,3687.36,3692.948,3700.384,3710.218,3721.758,3735.07,3749.023,3761.307,3771.312,3779.496,3786.653,3793.472,3800.401,3810.434,3826.845,3850.323,3881.263,3915.979,3945.088,3962.835,3970.613,3974.154,3976.556,3979.059,3981.622,3983.965,3986.197,3987.975,3989.738,3990.982,3992.252,3993.539,3994.95,3996.329,3997.578,3998.949,4000.323,4001.774,4003.358,4005.047,4006.826,4008.501,4010.002,4011.12,4010.829,4011.757,4011.466,4012.571,4013.726,4014.719,4015.612,4015.322,4016.153,4015.862,4016.773,4017.729,4018.441,4019.06,4019.624,4020.201,4020.895,4021.846,4023.249))
  }
  AlignObj
}