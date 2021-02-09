#// **********************************************************************************************
#//                         getUnimodMapping.R
#// **********************************************************************************************
#//
#// 
#// **********************************************************************************************
#// @Maintainer: Justin Sing
#// @Author: Justin Sing

#' @export
#' @title Generate a UniMod mapping table
#' @description This function can be used to generate a data.table containing
#' UniMod ID to Modification code name mapping
#' 
#' @param unimod_xml A character path indicating the path to a unimod.xml path
#' @return A data.table containing UniMod ID to Modification Code Name mapping
#' 
#' @author Justin Sing \url{https://github.com/singjc}
#' 
#' @importFrom dplyr %>% select
#' @importFrom xml2 read_xml xml_ns xml_find_all xml_attrs
#' @importFrom purrr map map_df
#' @importFrom stringi stri_rand_strings
generateUniModMapping_ <- function( unimod_xml=NULL ){
  # library(xml2)
  # library(purrr)
  if ( is.null(unimod_xml) ){
    ## Path to unimod xml database
    unimod_xml <- paste0(getwd(), "/R/sysdata/unimod_tables.xml")
  }
  ## Read in unimod xml
  unimod_dt <- xml2::read_xml( unimod_xml )
  ## Establish list of namespace in file
  ns <- xml2::xml_ns(unimod_dt)
  ## Extract modifications row information from unimod xml
  tmp <- xml2::xml_find_all(unimod_dt, "//d1:modifications_row", ns)
  ## Extract record id and modifications code name and map to datatable
  purrr::map(tmp, xml2::xml_attrs) %>%
    purrr::map_df( as.list ) %>%
    dplyr::select( record_id, ex_code_name ) -> unimod_mapping
  ## Generate random char id for each entry
  unimod_mapping$rand_char_id <- stringi::stri_rand_strings(dim(unimod_mapping)[1], 5)
  # save(unimod_mapping, file = './data/unimod_mapping.RData', version = 2)
  ## Return a datable with UniMod ID and modification code name mapping
  return( unimod_mapping )
}

## Dummy Documentation
#' @title Conversion from UniMod ID to modification name
#' @usage unimodTocodename( mod_seq, out='sequence' )
#' @param mod_seq A character vector containing of a peptide sequence, containing UniMod IDs to be converted to code names
#' @param out A character vector. Default: 'sequence' will return mod_seq with covnerted unimod to codename. Otherwise, will return converted codename
#' @return A character vector. Returned mod_seq with converted codename or returned codename
#' @name unimodTocodename
#' @examples 
#' mod_seq <- "EGHAQNPM(UniMod:35)EPS(UniMod:21)VPQLS(UniMod:21)LM(UniMod:35)DVK"
#' ## Will return peptide sequence with modifciation code name
#' unimodTocodename( mod_seq )
#' ## Out
#' ## [1] "EGHAQNPM(Oxidation)EPS(Phospho)VPQLS(Phospho)LM(Oxidation)DVK"
#' @importFrom plyr mapvalues
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
NULL

#' @export
unimodTocodename <- function( mod_seq, out='sequence', modification_labels=NULL ){
  DEBUG = F
  if ( DEBUG ){
    mod_seq <- "EGHAQNPMEPSVPQLSLMDVK"
    mod_seq <- "EGHAQNPMEPSVPQLS(UniMod:21)LMDVK"
    mod_seq <- "EGHAQNPM(UniMod:35)EPS(UniMod:21)VPQLS(UniMod:21)LM(UniMod:35)DVK"
    mod_seq <- "ADEIC(UniMod:4)IAGS(UniMod:21)PLTPR"
  }
  ## Get Modifications
  if ( is.null(modification_labels) ){
    modification_labels <- unique(unlist(base::regmatches(mod_seq, gregexpr("\\(.*?\\)", mod_seq))))
  }
  ## Check if there were any modifications
  if ( length(modification_labels) >= 1 ){
  ## get UniMod IDs
  query_unimod_ids <- gsub( "\\(UniMod:(\\d+)\\)", "\\1", (modification_labels) )
  ## convert query UniMod IDs to modification code name
  out_code_names <- plyr::mapvalues(query_unimod_ids, from = unimod_mapping$record_id, to = unimod_mapping$ex_code_name, warn_missing = FALSE)
  names(out_code_names) <- gsub('\\(|\\)','',modification_labels)
  ## Convery to code name
  mod_seq %>%
    stringr::str_replace_all( out_code_names ) -> converted_mod_seq
  } else {
    out_code_names <- NULL
    converted_mod_seq <- mod_seq
  }
  if ( out=='sequence' ){
    return( converted_mod_seq )
  } else {
    return( out_code_names )
  }
}

## Dummy Documentation
#' @title Conversion from modification codename to UniMod ID
#' @usage codenameTounimod( mod_seq, out='sequence' )
#' @param mod_seq A character vector containing of a peptide sequence, containing modification code names to be converted to UniMod IDs
#' @param out A character vector. Default: 'sequence' will return mod_seq with covnerted codename to unimod. Otherwise, will return converted unimod
#' @return A character vector. Returned mod_seq with converted unimod or returned unimod
#' @name codenameTounimod 
#' @examples 
#' mod_seq <- "EGHAQNPM(Oxidation)EPS(Phospho)VPQLS(Phospho)LM(Oxidation)DVK"
#' ## Will return peptide sequence with UniMod ID
#' codenameTounimod ( mod_seq )
#' ## Out
#' ## [1] "EGHAQNPM(UniMod:35)EPS(UniMod:21)VPQLS(UniMod:21)LM(UniMod:35)DVK"
#' @importFrom plyr mapvalues
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
NULL

#' @export
codenameTounimod <- function( mod_seq, out='sequence', modification_labels=NULL ){
  DEBUG = F
  if ( DEBUG ){
    mod_seq <- "EGHAQNPMEPSVPQLSLMDVK"
    mod_seq <- "EGHAQNPMEPSVPQLS(Phospho)LMDVK"
    mod_seq <- "EGHAQNPM(Oxidation)EPS(Phospho)VPQLS(Phospho)LM(Oxidation)DVK"
    mod_seq <- "ANS(Phospho)SPTTNIDHLK(Label:13C(6)15N(2))"
  }
  ## Get Modifications
  ## Need to make this more robust, somehow
  ## Get Modifications
  if ( is.null(modification_labels) ){
    modification_labels <- unique(unlist(base::regmatches(mod_seq, gregexpr("\\(.*?\\)", mod_seq))))
  }
  ## Check if there were any modifications
  if ( length(modification_labels) >= 1 ){
    ## convert query UniMod IDs to modification code name
    out_code_names <- plyr::mapvalues( gsub('^\\(|\\)$','',modification_labels), from = unimod_mapping$ex_code_name, to = paste0("UniMod:", unimod_mapping$record_id), warn_missing = FALSE)
    names(out_code_names) <- gsub('^\\(|\\)$','',modification_labels)
    ## Convert to code name
    mod_seq %>%
      stringr::str_replace_all( out_code_names ) -> converted_mod_seq
    ## Check if Label is present on the end
    if ( grepl('\\Label:13C\\(\\d+\\)15N\\(\\d+\\)', converted_mod_seq) ){
    converted_mod_seq <- gsub('\\Label:13C\\(\\d+\\)15N\\(\\d+\\)', out_code_names[grepl('Label.*', names(out_code_names))], converted_mod_seq )
    }
  } else {
    out_code_names <- NULL
    converted_mod_seq <- mod_seq
  }
  if ( out=='sequence' ){
    return( converted_mod_seq )
  } else {
    return( out_code_names )
  }
}

## Dummy Documentation
#' @title Conversion from UniMod ID to a unique string isomeric id
#' @usage unimodTouniisoid( mod_seq, out='sequence' )
#' @param mod_seq A character vector containing of a peptide sequence, containing UniMod IDs to be converted to code names
#' @param out A character vector. Default: 'sequence' will return mod_seq with covnerted unimod to codename. Otherwise, will return converted codename
#' @param mod_exclusion A vector of unimod record id modifications to exclude, and replace by nothing. Example: c("35")
#' @param mod_grouping A dataframe contained uninod ids and random characters to group isomers by. Example: data.frame( record_id=c(4, 259, 267), rand_char_id="B", stringsAsFactors = F )
#' @return A character vector. Returned mod_seq with converted codename or returned codename
#' @name unimodTouniisoid
#' @examples 
#' mod_seq <- "EGHAQNPM(UniMod:35)EPS(UniMod:21)VPQLS(UniMod:21)LM(UniMod:35)DVK"
#' ## Will return a unique string id that can be used to group isomers
#' unimodTouniisoid( mod_seq )
#' ## Out
#' ## [1] "EGHAQNPM(Oxidation)EPS(Phospho)VPQLS(Phospho)LM(Oxidation)DVK"
#' @importFrom plyr mapvalues
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all str_sort
NULL

#' @export
unimodTouniisoid <- function( mod_seq, out="sequence", mod_exclusion=NULL, mod_grouping=NULL, modification_labels=NULL ){
  DEBUG = F
  if ( DEBUG ){
    mod_seq <- "EGHAQNPMEPSVPQLSLMDVK"
    mod_seq <- "EGHAQNPMEPSVPQLS(UniMod:21)LMDVK"
    mod_seq <- "EGHAQNPM(UniMod:35)EPS(UniMod:21)VPQLS(UniMod:21)LM(UniMod:35)DVK"
    mod_seq <- "ADEIC(UniMod:4)IAGS(UniMod:21)PLTPR"
    mod_exclusion <- c("35")
    mod_grouping <- data.frame( record_id=c(4, 259, 267), rand_char_id="B", stringsAsFactors = F )
  }
  ## Get Modifications
  if ( is.null(modification_labels) ){
    modification_labels <- unique(unlist(base::regmatches(mod_seq, gregexpr("\\.?\\(.*?\\)", mod_seq))))
  }
  ## Get unimod mapping table
  unimod_mapping <- unimod_mapping
  ## Check if there were any modifications
  if ( length(modification_labels) >= 1 ){
    ## Check for any modifications to exclude from conversion, replace with nothing
    if ( !is.null(mod_exclusion) ){
      ## Convert to character if a numeric vector is given
      if ( is.numeric(mod_exclusion) ){
        mod_exclusion <- as.character(mod_exclusion)
      }
      ## Replace entries that are not to have a covnerted seqeunce if with an empty char
      unimod_mapping$rand_char_id[ which( unimod_mapping$record_id %in% mod_exclusion ) ] <- ""
    }
    ## Check for any entries that should be grouped by the same repalcement
    if( !is.null(mod_grouping) ){
      ## Replace random char id with provided character
      unimod_mapping$rand_char_id[ which( unimod_mapping$record_id %in% mod_grouping$record_id ) ] <- mod_grouping$rand_char_id
    }
    ## get UniMod IDs
    query_unimod_ids <- gsub( "\\.?\\(UniMod:(\\d+)\\)", "\\1", (modification_labels) )
    ## convert query UniMod IDs to modification code name
    out_code_names <- plyr::mapvalues(query_unimod_ids, from = unimod_mapping$record_id, to = unimod_mapping$rand_char_id, warn_missing = FALSE)
    names(out_code_names) <- gsub('\\(|\\)','',modification_labels)
    ## Convery to code name
    mod_seq %>%
      stringr::str_replace_all( out_code_names ) -> converted_mod_seq
    ## Remove brackets
    converted_mod_seq <- gsub('\\(|\\)','',converted_mod_seq)
    ## Sort based on character to create a unique string TODO: DECAPRECATE, because there are chances when the sequence is composed of the exact same letters, but in different order. This will cause these sequences to be grouped together.
    # NOTE: You have to sort, otherwise groups won't have their letters in the right order... Need to so something else to fix the issue of a.a. similarity.      
    converted_mod_seq <- lapply(base::strsplit(converted_mod_seq,""), function(split_seq){ paste( stringr::str_sort(split_seq, ), collapse="") }) 
  } else {
    out_code_names <- NULL
    converted_mod_seq <- mod_seq
    converted_mod_seq <- paste(stringr::str_sort(base::strsplit(converted_mod_seq,"")[[1]]),collapse="")
  }
  if ( out=='sequence' ){
    return( as.character( converted_mod_seq ) )
  } else {
    return( out_code_names )
  }
}