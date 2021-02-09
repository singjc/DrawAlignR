#// **********************************************************************************************
#//                         getXIC.R
#// **********************************************************************************************
#//
#// 
#// **********************************************************************************************
#// @Maintainer: Justin Sing
#// @Author: Justin Sing

#' @export
#' @title    Plot an Extracted Ion Chromatogram
#' @description This function can be used to plot an Extracted Ion Chromatogram
#' 
#' @param graphic_obj A ggplot() graphics handle. Initialize with g <- ggplot() to create an empty ggplot handle
#' @param mod A character vector for specific peptide/modified peptide to extract I.e. 'ANSSPTTNIDHLK'/'ANS(UniMod:21)SPTTNIDHLK(UniMod:259)'. The MODIFIED_SEQUENCE column is used
#' @param df_lib A data.table containing spectral library information
#' @param chromatogram_file A character vector of the absolute path and filename of the chromatogram file. (Must be .mzML or sqMass format)
#' @param chromatogram_data_points_list (Optional) Pass a list containing chromatogram data.
#' @param in_osw A character vector of the absolute path and filename of the OpenSwath Output file. (Must be .osw) @TODO maybe make this more robust for tsv files as well?
#' @param df_osw An optional dataframe containing OpenSwath Results information. (Use this if you have a dataframe cached in memory)
#' @param SCORE_IPF Do you want to extract IPF Score if present? (Default: FALSE. will use MS2 m-scores)
#' @param annotate_best_pkgrp Annotate the ebst peak group
#' @param transition_type A vector containing possible choices for dispalying one of the following transition group types. getXIC needs to be called for each option (Options: c('precursor', 'detecting', 'identifying') )
#' @param unit_mod_list A list of potential modifiable forms. (Default: NULL)
#' @param max_Int A numeric value indicating the maximum intensity. This is used for FacetZooming the y-axis. (Default: NULL)
#' @param smooth_chromatogram  A list object containing the polynomial filter order (p), and the bandwidth (number of data-points to smooth over, n). (Default: list(p=4, n=9)) 
#' @param doFacetZoom A logical value for calling Facet_Zoom function to zoom in the y axis based on the max_Int/4. (Default: FALSE)
#' @param FacetFcnCall A personalized function call to Facet_Zoom. I.e. FacetFcnCall = facet_zoom(xlim = c(3950, 4050), ylim = c(0, 10000)). (Default: NULL)
#' @param top_trans_mod_list A list containing a data.table/data.frame of the current peptide(mod peptide) with information for transition ids and top posterior error probabilities.
#' @param transition_selection_list A list containing transitions to display for unique identifying. i.e. transition_selection_list <- list( y = c(3), b = c(8:10) )
#' @param RT_pkgrps A numeric vector of the alternative peak group ranks to display. I.e. c(2, 4) (Default: NULL)
#' @param show_manual_annotation a dataframe with leftWidth and rightWidth retention time boundary values of a manually annotated peak. Will draw a transparent blue shaded rectangle indicating manual annotation. I.e data.frame(leftWidth=300, rightWidth=330)
#' @param show_peak_info_tbl A logical value. Show peak information table containing RT, rank, ms2-mscore, ipf_pep, ipf-mscore. (Default: FALSE)
#' @param plotIdentifying.Unique A logical value. TRUE will plot unique identifying transitions. (Default: NULL)
#' @param plotIdentifying.Shared A logical value. TRUE will plot shared identifying transitions. (Default: NULL)
#' @param plotIdentifying.Against A logical value. TRUE will plot against identifying transitions. (Default: NULL)
#' @param show_n_transitions A numeric value indicating the number of transitions to draw. (Default: NULL, default is 6 transitions)
#' @param transition_dt A data.table containing TRANSITION_SCORE Information obtained from the OSW file using getTransitionScores_. (Default: NULL)
#' @param show_legend A logical value. Display legend information for transition id, m/z and charge. (Default: TRUE)
#' @param mzPntrs A list object containing cached mzR objects.
#' @return A list containing graphic_obj = the graphic handle for the ggplot filled with data and max_Int = the maximun intensity 
#'
#' @author Justin Sing \url{https://github.com/singjc}
#' 
#' @importFrom ggplot2 ggplot geom_line geom_vline geom_rect aes guides guide_legend ggtitle labs theme element_text scale_alpha_identity scale_fill_manual theme_bw
#' @importFrom ggforce facet_zoom
#' @importFrom grid unit
#' @importFrom gridExtra ttheme_default tableGrob arrangeGrob 
#' @importFrom ggpubr as_ggplot 
#' @importFrom data.table set
#' @importFrom plyr mapvalues
#' @importFrom dplyr %>% filter select bind_rows mutate group_by top_n arrange contains
#' @importFrom tibble rownames_to_column
#' @importFrom rlang sym
#' @importFrom signal sgolayfilt
#' @importFrom MazamaCoreUtils logger.isInitialized logger.info logger.error logger.warn 
getXIC <- function( graphic_obj=ggplot2::ggplot(), 
                    mod, 
                    df_lib, 
                    chromatogram_file, 
                    chromatogram_data_points_list=NULL,
                    Isoform_Target_Charge, 
                    in_osw=NULL, 
                    df_osw=NULL,
                    SCORE_IPF=FALSE,
                    annotate_best_pkgrp=TRUE,
                    transition_type=c('detecting'), 
                    uni_mod_list=NULL, 
                    max_Int=NULL, 
                    retention_time_limits = list(minRT=-Inf, maxRT=Inf),
                    smooth_chromatogram=list(p = 4, n = 9),
                    doFacetZoom=FALSE, 
                    FacetFcnCall=NULL,
                    top_trans_mod_list=NULL, 
                    transition_selection_list=NULL,
                    RT_pkgrps=NULL,
                    show_manual_annotation=NULL,
                    show_peak_info_tbl=F,
                    plotIdentifying.Unique=NULL, 
                    plotIdentifying.Shared=NULL, 
                    plotIdentifying.Against=NULL,
                    show_n_transitions=NULL,
                    transition_dt=NULL,
                    show_legend=T, 
                    mzPntrs=NULL
){
  
  
  
  ## Check if logging has been initialized
  if( !MazamaCoreUtils::logger.isInitialized() ){
    log_setup()
  }
  
  # filename <- "/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/PTMs_Project/Synth_PhosoPep/Justin_Synth_PhosPep/results/mzML_Chroms_Decomp/chludwig_K150309_013_SW_0_osw_chrom.mzML"
  

  # Main --------------------------------------------------------------------
  
  ##*******************************************************
  ##    Extract Precursor Information from Library
  ##*******************************************************
  if ( 'precursor' %in% transition_type ){
    MazamaCoreUtils::logger.info('[DrawAlignR::getXIC]  Extracting Precursor Transition...')
    df_lib %>%
      dplyr::filter( if( is.null(chromatogram_data_points_list) ){ MODIFIED_SEQUENCE==mod  } else { UNMODIFIED_SEQUENCE==gsub("\\([a-zA-Z0-9:.]+\\)", '', mod) } ) %>%
      # dplyr::filter( MODIFIED_SEQUENCE==mod ) %>%
      dplyr::filter( TYPE=="" ) %>%
      dplyr::filter( PRECURSOR_CHARGE==Isoform_Target_Charge )-> df_lib_filtered
    if ( checkDataframe( df_lib_filtered, graphic_obj, msg='There was no data found for precursor transition in library\n' ) ){ return( list(graphic_obj=graphic_obj, max_Int=max_Int) ) }
  }
  
  ##******************************************************************
  ##    Extract Detecting Transitions Information from Library
  ##******************************************************************
  if ( 'detecting' %in% transition_type){
    MazamaCoreUtils::logger.info('[DrawAlignR::getXIC] Extracting Detecting Transitions...')
    df_lib %>%
      dplyr::filter( if( is.null(chromatogram_data_points_list) ){ MODIFIED_SEQUENCE==mod  } else { UNMODIFIED_SEQUENCE==gsub("\\([a-zA-Z0-9:.]+\\)", '', mod) }  ) %>% ## TODO What is the best choice here? Modified or unmodified?
      # dplyr::filter( MODIFIED_SEQUENCE==mod ) %>%
      dplyr::filter( PRECURSOR_CHARGE==Isoform_Target_Charge ) %>%
      dplyr::filter( DETECTING==1 ) -> df_lib_filtered
    if ( checkDataframe( df_lib_filtered, graphic_obj, msg='There was no data found detecting transitions in library\n' ) ){ return( list(graphic_obj=graphic_obj, max_Int=max_Int) ) }
  } 
  
  ##******************************************************************
  ##    Extract Identifying Transitions Information from Library
  ##******************************************************************
  if ( 'identifying' %in% transition_type ){
    MazamaCoreUtils::logger.info('[DrawAlignR::getXIC]  Extracting Identifying Transitions...')
    df_lib %>%
      dplyr::filter( if( is.null(chromatogram_data_points_list) ){ MODIFIED_SEQUENCE==mod  } else { UNMODIFIED_SEQUENCE==gsub("\\([a-zA-Z0-9:.]+\\)", '', mod) } ) %>%
      # dplyr::filter( MODIFIED_SEQUENCE==mod ) %>%
      dplyr::filter( PRECURSOR_CHARGE==Isoform_Target_Charge ) %>%
      dplyr::filter( DETECTING==0 ) -> df_lib_filtered
    if ( checkDataframe( df_lib_filtered, graphic_obj, msg='There was no data found for identifying transitions in library\n' ) ){ return( list(graphic_obj=graphic_obj, max_Int=max_Int) ) }
  }
  
  ##******************************************************************
  ##    Extract OpenSwath Information from .osw
  ##******************************************************************
  
  if ( !is.null( in_osw ) | !is.null(df_osw) ){
    MazamaCoreUtils::logger.info('[DrawAlignR::getXIC]  Extracting OpenSwathResults Info...')
    ## Filter library dataframe for matching evaluated modification sequence, precursor charge and for detecting transitions. 
    df_lib %>%
      dplyr::filter( MODIFIED_SEQUENCE==mod ) %>%
      dplyr::filter( PRECURSOR_CHARGE==Isoform_Target_Charge ) %>%
      dplyr::filter( DETECTING==1 ) -> df_lib_filtered
    
    ## @TODO: Need to make this more robust for other file formats or naming conventions
    run_name <- gsub('_osw_chrom[.]sqMass$|[.]chrom.mzML$|[.]chrom.sqMass$|_osw[.]chrom[.]sqMass', '', basename(chromatogram_file))
    ## @TODO: Maybe remove this or leave the whole filename. This is very specific for 3 datasets..
    run <- gsub('_SW*|_SW_0|(*_-_SW[.]mzML[.]gz)', '', gsub('yanliu_I170114_\\d+_|chludwig_K150309_|lgillet_L\\d+_\\d+-Manchester_dirty_phospho_-_', '', run_name))
    ## Replace UniMod name with actual modification name
    ## @TODO: Need to make this more robust for other types of modifications, or for naked peptides.
    mod_form_rename <- gsub('UniMod:4','Carbamidomethyl', gsub('UniMod:35','Oxidation', gsub('UniMod:259','Label:13C(6)15N(2)', gsub('UniMod:267','Label:13C(6)15N(4)', gsub('UniMod:21','Phospho', mod)))))
    ## Filter for precursor id that matches target charge state
    df_lib_filtered %>%
      dplyr::filter( PRECURSOR_CHARGE==Isoform_Target_Charge ) %>%
      dplyr::select( PRECURSOR_ID ) %>%
      unique() %>% as.matrix() %>% as.numeric() -> target_charge_precursor
    ## @TODO: Need to make this more robust for later
    if ( is.null(df_osw) ){
      if( mod==mod_form_rename ){
        osw_df <- getOSWData_( in_osw, run_name, precursor_id=target_charge_precursor, peptide_id='', mod_peptide_id='', mod_residue_position='', peak_group_rank_filter=F, pep_list='', ipf_filter='', ms2_score=T, ipf_score=SCORE_IPF )
        # # Original OSW Peptide Names 
        # osw_pep_names <- gsub('UniMod:4','Carbamidomethyl', gsub('UniMod:35','Oxidation', gsub('UniMod:259','Label:13C(6)15N(2)', gsub('UniMod:267','Label:13C(6)15N(4)', gsub('UniMod:21','Phospho', osw_df$FullPeptideName)))))
        # # Keep only Rows that correspond to the correct Assay
        # osw_df %>% dplyr::filter( osw_pep_names == osw_df$ipf_FullPeptideName )
      } else {
        osw_df <- getOSWData_( in_osw, run_name, precursor_id=target_charge_precursor, peptide_id='', mod_peptide_id="", mod_residue_position='', peak_group_rank_filter=F, pep_list='', ipf_filter='', ms2_score=T, ipf_score=SCORE_IPF )
      }
    } else {
      osw_df <- df_osw
    }
    ## Check if openswath dataframe is empty
    if ( checkDataframe( osw_df, graphic_obj, msg='There was no data found in OpenSwath Dataframe' ) ){ 
      ## If osw_df is empty, return graphic object and max_Int value.
      graphic_obj <- graphic_obj +
        ggtitle(  mod ) +
        labs(subtitle = paste('Run: ', run, 
                              ' | Precursor: ', df_lib_filtered$PRECURSOR_ID, 
                              ' | Peptide: ', df_lib_filtered$PEPTIDE_ID, 
                              ' | Charge: ', df_lib_filtered$PRECURSOR_CHARGE, 
                              ' | m/z: ', df_lib_filtered$PRECURSOR_MZ, sep=''))
      return( list(graphic_obj=graphic_obj, max_Int=max_Int) ) 
    }
    ## Filter OSW dataframe for precursor with target charge.
    ## @TODO: Maybe this is repetitive and pointless, since we use target_charge_precursor in OSW data extraction in getOSWData_
    if ( !is.null(Isoform_Target_Charge) ){
      osw_df %>%
        dplyr::filter( Charge==Isoform_Target_Charge ) -> osw_df
    }
    ## Filter for Specific Modification
    osw_df %>%
      dplyr::filter( FullPeptideName==mod ) -> osw_df
    
    if ( "ipf_pep" %in% colnames(osw_df) ) {
      
      ## TODO: Could remove this potentially, since the updated getOSWData function should correct for the other NA options
      if ( !is.null( unlist(osw_df$ipf_pep) ) ){
        ## Remove rows with NULL value in ipf_pep
        osw_df %>%
          dplyr::filter( !is.null(ipf_pep) ) %>%
          dplyr::filter( !is.nan(ipf_pep) ) -> osw_df
      }
    }
    if ( "m_score" %in% colnames(osw_df) ) {
      ## Filter OSW dataframe for Best Peak Feature.
      ## Get data for the best peak as defined by the feature with the lowest m_score 
      osw_df %>%
        dplyr::filter( m_score==min(m_score) ) -> osw_df_filtered #### No longer filter by peak group
    } else {
      ## Filter OSW dataframe for Best Peak Feature.
      ## Get data for the best peak as defined by the feature with the lowest m_score 
      osw_df %>%
        dplyr::filter( ms2_m_score==min(ms2_m_score) ) -> osw_df_filtered #### No longer filter by peak group
    }
    
    if ( dim(osw_df_filtered)[1] > 1 ){
      osw_df_filtered %>%
        dplyr::filter( peak_group_rank==min(peak_group_rank) ) -> osw_df_filtered
      
    } 
    ## Check if openswath dataframe is empty after filtering
    if ( checkDataframe( osw_df, graphic_obj, msg='There was no data found in OpenSwath Dataframe after filtering for peptide and top peak' ) ){ 
      ## If osw_df is empty, return graphic object and max_Int value.
      graphic_obj <- graphic_obj +
        ggtitle(  mod ) +
        labs(subtitle = paste('Run: ', run, 
                              ' | Precursor: ', df_lib_filtered$PRECURSOR_ID, 
                              ' | Peptide: ', df_lib_filtered$PEPTIDE_ID, 
                              ' | Charge: ', df_lib_filtered$PRECURSOR_CHARGE, 
                              ' | m/z: ', df_lib_filtered$PRECURSOR_MZ, sep=''))
      
      return( list(graphic_obj=graphic_obj, max_Int=max_Int) ) 
    }
    
    ## Extract some useful scores
    m_score <- checkNumeric( osw_df_filtered$ms2_m_score[[1]] )
    if ( "precursor_pep" %in% colnames(osw_df_filtered) ){
      prec_pkgrp_pep <- checkNumeric( osw_df_filtered$precursor_pep[[1]] )
    } else {
      prec_pkgrp_pep <- NaN
    }
    if ( "ipf_pep" %in% colnames(osw_df_filtered) ) {
      ipf_pep <- checkNumeric( osw_df_filtered$ipf_pep[[1]] )
    } else {
      ipf_pep <- NaN
    }
    if ( "m_score" %in% colnames(osw_df_filtered) ) {
      ipf_m_score <- checkNumeric( osw_df_filtered$m_score[[1]] )
    } else {
      ipf_m_score <- NaN
    }
    ms2_pkgrp_rank <- checkNumeric( osw_df_filtered$peak_group_rank[[1]], signif.not=F )
    ##****************************************************
    ## Append OSW information to Chromatogram ggplot.
    ##****************************************************
    if ( annotate_best_pkgrp ) {
      MazamaCoreUtils::logger.info('[DrawAlignR::getXIC]  Adding Best Peak Annotation...')
      graphic_obj <- graphic_obj +
        # geom_vline(xintercept = osw_df_filtered$RT, color='red', size = 1.3, alpha = 0.65 ) +
        geom_vline(data = osw_df_filtered, aes(xintercept = osw_df_filtered$RT, text = sprintf("Peak RT: %s\nLeft Width: %s\nRight Width: %s\nPeak Rank: %s\nms2_m-score: %s\nprec-pkgrp pep: %s\nipf pep: %s\nipf_m-score: %s",
                                                                                               osw_df_filtered$RT, round(osw_df_filtered$leftWidth,3), round(osw_df_filtered$rightWidth,3), ms2_pkgrp_rank,
                                                                                               checkNumeric(m_score), checkNumeric(prec_pkgrp_pep), checkNumeric(ipf_pep), checkNumeric(ipf_m_score) )), color='red', size = 1.3, alpha = 0.65 ) +
        geom_vline(xintercept = osw_df_filtered$leftWidth, color='red', linetype='dotted', size=1, alpha = 0.85 ) +
        geom_vline(xintercept = osw_df_filtered$rightWidth, color='red', linetype='dotted', size=1, alpha = 0.85 )  # default size 0.65
    }
    ## Append title and subtitle information
    # print(osw_df_filtered)
    graphic_obj <- graphic_obj + ggtitle(  mod ) +
      labs(subtitle = paste(
        'Run: ', run, 
        ' | Precursor: ', df_lib_filtered$PRECURSOR_ID,
        ' | Peptide: ', df_lib_filtered$PEPTIDE_ID,
        ' | Charge: ', osw_df_filtered$Charge,
        ' | m/z: ', osw_df_filtered$mz, 
        ' | RT(s): ', osw_df_filtered$RT, 
        ' | RT(m): ', round((osw_df_filtered$RT/60), digits = 2),
        '\nlib_RT: ', round(osw_df_filtered$assay_iRT, digits = 4),
        ' | ms2_m-score: ', m_score,
        ' | ipf_m-score: ', ipf_m_score,
        ' | ipf_pep: ', ipf_pep,
        ' | ms2_pkgrp_rank: ', ms2_pkgrp_rank,
        sep='')) 
    ##*************************************
    ## Plot Other peak rank groups
    ##*************************************
    if( !is.null(RT_pkgrps) ){
      MazamaCoreUtils::logger.info('[DrawAlignR::getXIC]  Adding Other Peak Group Annotations...')
      if ( is.null(df_osw) ){
        ## Get OSW data for other potential peaks/features
        osw_RT_pkgrps <- getOSWData_( in_osw, run_name, precursor_id=target_charge_precursor, peptide_id='', mod_peptide_id='', mod_residue_position='', peak_group_rank_filter=F, pep_list='', ipf_filter='', ms2_score=T, ipf_score=SCORE_IPF )
      } else {
        osw_RT_pkgrps <- df_osw
      }
      ## Filter out the top best feature  
      osw_RT_pkgrps %>%
        dplyr::filter( RT %in% RT_pkgrps ) %>%
        dplyr::filter( RT != osw_df_filtered$RT ) %>%
        dplyr::select( RT, leftWidth, rightWidth, peak_group_rank, ms2_m_score, dplyr::contains("ipf_pep"), dplyr::contains("m_score"), dplyr::contains("precursor_pep") ) -> osw_RT_pkgrps_filtered

      if ( dim(osw_RT_pkgrps_filtered)[1]!=0 ){
        
        # TODO: Change this to be more robust
        ## Make dumy columns for precursor_pep, ipf_pep etc.
        if ( !SCORE_IPF ){
          cat("Non IPF scores is being used, pre-filling with NaN")
          osw_RT_pkgrps_filtered$precursor_pep <- NaN
          osw_RT_pkgrps_filtered$ipf_pep <- NaN
          osw_RT_pkgrps_filtered$m_score <- NaN
        }
        # Define unique set of colors to annotate different peak rank groups
        jBrewColors <- RColorBrewer::brewer.pal(n = dim(osw_RT_pkgrps_filtered)[1], name = "Dark2")
        
        osw_RT_pkgrps_filtered$Color <- jBrewColors[seq(1, dim(osw_RT_pkgrps_filtered)[1]) ]
        
        graphic_obj <- graphic_obj +
          geom_vline(data = osw_RT_pkgrps_filtered, aes(xintercept = RT, text = sprintf("Peak RT: %s\nLeft Width: %s\nRight Width: %s\nPeak Rank: %s\nms2_m-score: %s\nprec-pkgrp pep: %s\nipf pep: %s\nipf_m-score: %s",
                                                                                        RT, round(leftWidth, 3), round(rightWidth, 3), peak_group_rank,
                                                                                        checkNumeric(ms2_m_score), checkNumeric(precursor_pep), checkNumeric(ipf_pep), checkNumeric(m_score) )), color=osw_RT_pkgrps_filtered$Color, alpha=0.65, size = 1.3 ) +
          geom_vline(data = osw_RT_pkgrps_filtered, aes(xintercept = leftWidth), color=osw_RT_pkgrps_filtered$Color, linetype='dotted', alpha=0.85, size=1 ) +
          geom_vline(data = osw_RT_pkgrps_filtered, aes(xintercept = rightWidth), color=osw_RT_pkgrps_filtered$Color, linetype='dotted', alpha=0.85, size=1 ) 
        
        #**********************************************
        # Add Table on bottom of plot with OSW info
        #**********************************************
        if ( show_peak_info_tbl ){
          MazamaCoreUtils::logger.info('[DrawAlignR::getXIC]  Adding Peak Information Table...')
          tryCatch( expr = {
            for( RT_idx in seq(1, dim(osw_RT_pkgrps_filtered)[1],1) ){
              ## get scores and format them                                                                                                             
              rank <- osw_RT_pkgrps_filtered$peak_group_rank[RT_idx]                                                                      
              ms2_m_score <- formatC(osw_RT_pkgrps_filtered$ms2_m_score[RT_idx], format = "e", digits = 3)      
              prec_pkgrp_pep <- checkNumeric( osw_RT_pkgrps_filtered$precursor_pep[RT_idx] )
              ipf_pep <- formatC(osw_RT_pkgrps_filtered$ipf_pep[RT_idx], format = "e", digits = 3)                                                       
              ipf_m_score <- formatC(osw_RT_pkgrps_filtered$m_score[RT_idx], format = "e", digits = 3) 
              point_dataframe <- data.frame(RT=(osw_RT_pkgrps_filtered$RT[RT_idx]),
                                            RT_m = round((osw_RT_pkgrps_filtered$RT[RT_idx])/60, digits=2),
                                            rank = rank, 
                                            ms2_m_score = ms2_m_score,
                                            ipf_pep = ipf_pep,
                                            ipf_m_score = ipf_m_score
              )
              
              
              #****************************************************                                                                                       
              # Check to see if master annotation table exits                                                                                             
              #****************************************************                                                                                       
              if ( !exists("master_annotation_table") ){                                                                                                  
                master_annotation_table <- point_dataframe                                                                                                
              } else {                                                                                                                                    
                master_annotation_table <- rbind(master_annotation_table, point_dataframe)                                                                
              }
              
              ## Clearup
              # rm(osw_RT_pkgrps_filtered_subset)
            } # End for loop
          },
          error = function(e){
            MazamaCoreUtils::logger.error(sprintf('[DrawAlignR::getXIC]  The following error occured while generating peak info table for plotting:\n%s', e$message))
          })
          
          
        } # End show_peak_info_tbl
        
        if ( show_peak_info_tbl ){
          theme_col <- gridExtra::ttheme_default(core = list(fg_params = list( col = c( jBrewColors )),                                                                                                                                                                                                            
                                                             bg_params = list( col = NA)),                                                                 
                                                 rowhead = list(bg_params = list(col = NA)),                                                                
                                                 colhead = list(bg_params = list(col = NA))                                                                 
          )                                                                                                                                                
          
          #*****************************************************                                                                                           
          # Make tableGrob object with annotation information                                                                                              
          # and get height of table                                                                                                                         
          #*****************************************************                                                                                           
          annotationGrob <- gridExtra::tableGrob(master_annotation_table, theme = theme_col, row = NULL)                                                             
          th <- sum(annotationGrob$heights)
        } # End show_peak_info_tbl
      } # End osw_RT_pkgrps_filtered Check, ensure not empty
    } # End RT_pkgrps Arg Check
    
    #***************************************
    # Plot Mannual Annotation Coordinates
    #***************************************
    if ( !is.null(show_manual_annotation) ){
      MazamaCoreUtils::logger.info('[DrawAlignR::getXIC]  Adding Manual Peak Annotation...')
      graphic_obj <- graphic_obj + 
        geom_rect(data = data.frame(xmin = show_manual_annotation[1],
                                    xmax = show_manual_annotation[2],
                                    ymin = -Inf,
                                    ymax = Inf),
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = "blue", alpha = 0.1)
      
    }
    
    ##*************************************
    ##  Facet Zoom Chromatogram Plot
    ##*************************************
    if ( doFacetZoom==TRUE ){
      ## Check to see if user supplied there own face_zoom function
      if ( !is.null(FacetFcnCall) ){
        graphic_obj <- graphic_obj + FacetFcnCall
      } else {
        ## If the Max Intensity is greater than 1000, zoom into the chromatogram taking the max Intensity divided by 4
        if ( max(max_Int) > 1000 ){
          graphic_obj <- graphic_obj +
            # facet_zoom(ylim = c(0, (1000) ))
            ggforce::facet_zoom(ylim = c(0, (max(max_Int)/ 4 ) ))
          # facet_zoom(ylim = c(0, (max(max_Int)/ (max(max_Int)-mean(max_Int)) ) ))
        }
      }
    }
    if ( !is.null(top_trans_mod_list) ){
      graphic_obj <- graphic_obj +
        theme(plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              legend.text = element_text(size = 2),
              legend.key.size = grid::unit(0.5, "cm")) +
        # theme( panel.background = element_rect( fill='lightblue', color='lightblue', size=0.5, linetype='solid'),
        #       panel.border = element_rect( fill=NA, color='black', size=0.5, linetype='solid'),
        #       panel.grid.major = element_line( color='white', size=0.5, linetype='solid'),
        #       panel.grid.minor = element_line( color='white', size=0.25, linetype='solid') ) 
        theme_bw()
      
      if ( show_legend==FALSE ){
        graphic_obj <- graphic_obj + theme(legend.position="none") 
      }
      
    } else {
      graphic_obj <- graphic_obj +
        theme(plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5, size = 10)) +
        # theme(panel.background = element_rect( fill='#c1e1ec', color='#c1e1ec', size=0.5, linetype='solid'),
        #       panel.border = element_rect( fill=NA, color='black', size=0.5, linetype='solid'),
        #       panel.grid.major = element_line( color='white', size=0.5, linetype='solid'),
        #       panel.grid.minor = element_line( color='white', size=0.25, linetype='solid') ) 
        # guides( Transition=FALSE ) +
        theme_bw()
      if ( show_legend==FALSE ){
        graphic_obj <- graphic_obj + theme(legend.position="none") 
      }
    }
    
    if ( exists("annotationGrob") & show_peak_info_tbl ){                                                                                                                    
      graphic_obj <- ggpubr::as_ggplot( gridExtra::arrangeGrob( graphic_obj, annotationGrob, nrow = 2, heights = grid::unit.c(grid::unit(1, "null"), th )) )                    
    }    
    MazamaCoreUtils::logger.info(sprintf('[DrawAlignR::getXIC]  Returning Final XIC plot object for: %s', mod))
    return( list(graphic_obj=graphic_obj, max_Int=max_Int) )
  } #HEREEEE
  
  ##******************************************************** 
  ## Get Transition IDs for chromatogram data extraction
  ##********************************************************
  MazamaCoreUtils::logger.info('[DrawAlignR::getXIC]  Getting Transition IDs and Extracting Chromatogram Data...')
  frag_ids <- list()
  if( length(as.character( df_lib_filtered$TRANSITION_ID ))>1 ){
    frag_ids[[1]] <- as.character( df_lib_filtered$TRANSITION_ID )
  } else {
    frag_ids[[1]] <- list(as.character( df_lib_filtered$TRANSITION_ID ))
  }
  
  if ( is.null(chromatogram_data_points_list) ) {
    
    ##***************************************
    ##    Extract Chromatogram Data    
    ##***************************************
    MazamaCoreUtils::logger.info('[DrawAlignR::getXIC] Retreiving Chromatogram data...')
    chrom <- getChromatogramDataPoints_( filename = chromatogram_file, frag_ids = frag_ids, mzPntrs=mzPntrs  )

    
    ##***************************************
    ##    Smooth Chromatogram
    ##***************************************
    if ( length(smooth_chromatogram)>0 ){
      MazamaCoreUtils::logger.info('[DrawAlignR::getXIC] Smoothing Chromatogram Data...')
    }
    ## Smooth Intensity values to make Chromatogram look nice
    for (i in seq(1:length(chrom))){
      names(chrom[[i]]) <- c('RT','Int')
      if ( length(smooth_chromatogram)>0 ){
        chrom[[i]]$Int <- signal::sgolayfilt( chrom[[i]]$Int, p = smooth_chromatogram$p, n = smooth_chromatogram$n )
      }
    }
    ## Combine list of Intensity dataframs and Retention Time dataframes into one Dataframe mapping by Transition ID 
    df_plot <- dplyr::bind_rows(parallel::mclapply(chrom, data.frame), .id='Transition')
  } else {
    
    if ( F ){
      tmp <- AlignObjOutput$`ANSS(UniMod:21)PTTNIDHLK(UniMod:259)_2`[["chludwig_K150309_013_SW_0"]]
      names(tmp) <- unlist(lapply(tmp, function(x){ gsub("^X*", "", names(x)[2]) }))
      
      for (i in seq(1:length(tmp))){
        names(tmp[[i]]) <- c('RT','Int')
        if ( length(smooth_chromatogram)>0 ){
          tmp[[i]]$Int <- signal::sgolayfilt( tmp[[i]]$Int, p = smooth_chromatogram$p, n = smooth_chromatogram$n )
        }
      }
    }
    MazamaCoreUtils::logger.info('[DrawAlignR::getXIC] Chromatogram Data Points were supplied...') 
    ## Combine list of Intensity dataframs and Retention Time dataframes into one Dataframe mapping by Transition ID 
    df_plot <- dplyr::bind_rows(parallel::mclapply(chromatogram_data_points_list, data.frame), .id='Transition')
    
  }
  
  ##****************************
  ##  Chromatogram Data Check
  ##****************************
  ## Do a check to see if there was any data extracted, otherwise return null
  if ( sum(is.na(df_plot$RT))==length(df_plot$RT) & sum(is.na(df_plot$Int))==length(df_plot$Int) ) { warning("There was no data to add to plot..."); return( list(graphic_obj=graphic_obj, max_Int=max_Int) ) }
  
  ##****************************
  ##  Filter RT
  ##****************************
  df_plot %>%
    data.table::as.data.table() %>%
    dplyr::filter( (RT > retention_time_limits$minRT ) & (RT < retention_time_limits$maxRT) ) -> df_plot
  
  ## Append Transition information to Int-RT dataframe, filter for precursor transition or MS2 transitions 
  if ( transition_type=='precursor' ){
    ## Extraction Transtion Information
    df_lib_filtered %>% 
      dplyr::filter( TRANSITION_ID %in% unique(df_plot$Transition) ) %>%
      dplyr::select( TRANSITION_ID, PRECURSOR_CHARGE ) -> transition_info
    
    ## Append information to Precursor Transition ID
    transition_ids <- paste( paste('0 - ',transition_info$TRANSITION_ID,sep=''), paste(transition_info$PRECURSOR_CHARGE,'+',sep=''), sep='_')
    tmp <- sapply(seq(1,length(df_plot$Transition)), function(i){ transition_ids[grepl(paste('0 - ',df_plot$Transition[i],'_*',sep=''), transition_ids)] } )
  } else {
    ## Similar to above, but extract MS2 Transition ID information
    df_lib_filtered %>% 
      dplyr::filter(  TRANSITION_ID %in% unique(df_plot$Transition) ) %>%
      dplyr::select( TRANSITION_ID, CHARGE, TYPE, ORDINAL, PRODUCT_MZ ) -> transition_info
    transition_ids <- paste( transition_info$TRANSITION_ID, paste(transition_info$CHARGE,'+',sep=''), paste(transition_info$TYPE, transition_info$ORDINAL,sep=''), transition_info$PRODUCT_MZ, sep='_')
    tmp <- sapply(seq(1,length(df_plot$Transition)), function(i){ transition_ids[grepl(paste('^',df_plot$Transition[i],'_.*',sep=''), transition_ids)] } )
  }
  df_plot$TRANSITION_ID <- df_plot$Transition
  df_plot$Transition <- tmp
  
  if ( !is.null(transition_dt) ){
    
    transition_dt %>%
      base::unique() %>%
      dplyr::filter( transition_id %in% base::unique(df_plot$TRANSITION_ID) ) -> transition_dt_subset
    
    tictoc::tic()
    # if ( !("run_name" %in% colnames(transition_dt)) ){
    #   transition_dt[, run_name := tstrsplit(basename(filename), ".", fixed=TRUE, keep=1L)]  
    # }
    transition_dt %>%
      dplyr::filter( FullPeptideName==mod & Charge==Isoform_Target_Charge & run_name==(strsplit(basename(in_sqMass), "\\."))[[1]][[1]] ) %>%
      base::unique() %>%
      dplyr::filter( transition_id %in% base::unique(df_plot$TRANSITION_ID) ) -> transition_dt_subset
    tictoc::toc()
    
    if ( dim(transition_dt_subset)[1]!=0 ) {
      ## Pre-Assign Vars in Table as NaN
      df_plot$RT.Map <- NaN
      ### Get pk borders for features.
      # pk_borders <- unique( dplyr::select( transition_dt_subset, c(leftWidth, rightWidth) ) )
      ## Get a RT.Mapping corresponding to peaks
      for( row in seq(1,dim(transition_dt_subset)[1]) ) {
        ## Subset for row_i
        current_pk_transition_dt <- transition_dt_subset[row,]
        ## Replace NaN where continuous RT is withing range of current peak borders
        df_plot$RT.Map[ findInterval(df_plot$RT, c(current_pk_transition_dt$RT-10, current_pk_transition_dt$RT+10))==1 ] <- current_pk_transition_dt$RT
      }
      ## Assign Vars as RT.Map to replace values with actual values
      df_plot$pep <- paste(df_plot$TRANSITION_ID, df_plot$RT.Map, sep="_")
      df_plot$pval <- paste(df_plot$TRANSITION_ID, df_plot$RT.Map, sep="_")
      df_plot$qval <- paste(df_plot$TRANSITION_ID, df_plot$RT.Map, sep="_")
      df_plot$score <- paste(df_plot$TRANSITION_ID, df_plot$RT.Map, sep="_")
      ## Update Values With Actual Values
      df_plot$pep <- plyr::mapvalues(df_plot$pep, from = paste(transition_dt_subset$transition_id, transition_dt_subset$RT, sep="_"), to = transition_dt_subset$transition_pep, warn_missing = FALSE)
      df_plot$pval <- plyr::mapvalues(df_plot$pval, from = paste(transition_dt_subset$transition_id, transition_dt_subset$RT, sep="_"), to = transition_dt_subset$transition_pval, warn_missing = FALSE)
      df_plot$qval <- plyr::mapvalues(df_plot$qval, from = paste(transition_dt_subset$transition_id, transition_dt_subset$RT, sep="_"), to = transition_dt_subset$transition_qval, warn_missing = FALSE)
      df_plot$score <- plyr::mapvalues(df_plot$score, from = paste(transition_dt_subset$transition_id, transition_dt_subset$RT, sep="_"), to = transition_dt_subset$tranistion_score, warn_missing = FALSE)
      ## Text to Display in Interactive Mode
      text_info <- sprintf("Transition: %s\nPk_Grp_RT: %s\nPEP: %s\npval: %s\nqval: %s\nscore: %s", 
                           df_plot$Transition, 
                           df_plot$RT.Map, 
                           checkNumeric(df_plot$pep), 
                           checkNumeric(df_plot$pval), 
                           checkNumeric(df_plot$qval), 
                           checkNumeric(df_plot$score) )
    } else {
      text_info <- sprintf("Transition: %s", df_plot$Transition)
    }
  } else {
    text_info <- sprintf("Transition: %s", df_plot$Transition)
  }
  
  ##****************************
  ## Check Max Intensity
  ##****************************
  if ( transition_type!='precursor' & dim(df_plot)[1]>0 ){
    # if ( max(df_plot$Int) > max_Int ){ max_Int <- max(df_plot$Int) }
    max_Int <- c(max_Int, max(df_plot$Int[is.finite(df_plot$Int)]))
  }
  
  ##***************************
  ## Plotting Action
  ##***************************
  if ( transition_type=='precursor' ){
    ##*********************************
    ## Plotting PRECURSOR Trace
    ##*********************************
    graphic_obj <- graphic_obj + 
      geom_line( data=df_plot, aes(RT, Int, group=Transition, alpha=0.65, text=text_info), show.legend = show_legend, linetype='solid', col='black' )  +
      scale_alpha_identity(name="Precursor", guide='legend', labels=unique(df_plot$Transition))
  } else if ( transition_type=='detecting' ){
    ##*********************************
    ## Plotting DETECTING Traces
    ##*********************************
    graphic_obj <- graphic_obj + 
      geom_line(data=df_plot, aes(RT, Int, group=Transition, fill=Transition, text=paste('Transition: ', Transition, sep='')), alpha=0.75, col='gray', linetype='solid', show.legend = show_legend) +
      scale_fill_manual(name="Detecting",values=rep('gray', length(unique(df_plot$Transition))) )
    
  } else{
    ##*********************************
    ## Plotting IDENTIFYING Traces
    ##*********************************
    class(df_plot$TRANSITION_ID) <- "integer"
    df_plot <- merge( df_plot, select( df_lib_filtered[df_lib_filtered$TRANSITION_ID %in% unique(df_plot$TRANSITION_ID),], c(TRANSITION_ID, TRAML_ID) ), by='TRANSITION_ID' )
    
    ##_________________________________________
    ##
    ##    Get unique peptide information
    ##_________________________________________
    
    if ( is.null(transition_selection_list) ){
      
      uni_forms <- unique (unlist( strsplit( unique(gsub(".*\\{|\\}.*", "", df_plot$TRAML_ID)), split='\\|') ) )
      
      # uni_forms <- c("EGHAQNPM(UniMod:35)EPS(UniMod:21)VPQLSLM(UniMod:35)DVK", "EGHAQNPM(UniMod:35)EPSVPQLS(UniMod:21)LM(UniMod:35)DVK")
      
      peptide_modification_positions <- lapply( uni_forms, function(pep){ mstools::getModificationPosition_( pep ) } )
      names(peptide_modification_positions) <- uni_forms
      
      do.call( rbind, lapply(peptide_modification_positions, function(x){ x[match(names(peptide_modification_positions[[1]]), names(x))]}) ) %>% ## Turn list into dataframe, and ensure items in list are matching by name
        as.data.frame() %>%
        tibble::rownames_to_column( "peptides" ) %>%
        mutate( unimod_peptides = mstools::codenameTounimod(peptides) ) %>%
        mutate( target_peptide = unimod_peptides==mod ) -> peptides_information_df
      
      ## Get modification columns
      modification_col_indices <- grep('modification_.*', colnames(peptides_information_df))
      
      ## convert naked peptide length and modification position columns to nemeric vectors
      lapply(modification_col_indices, function(col){
        data.table::set( peptides_information_df, NULL, col, as.numeric(peptides_information_df[[col]]) )
      })
      class(peptides_information_df$naked_peptide_length) <- "numeric"
      
      if ( length(modification_col_indices)>1 ){
        modifications_position_df <- peptides_information_df[,modification_col_indices] 
        
        target_modification <- apply(modifications_position_df, 2, function( positions ){
          if( length(unique(positions)) > 1){
            return( TRUE )
          } else {
            return( FALSE )
          }
        })
        
        target_modification <- names(target_modification)[target_modification]
      } else {
        target_modification <- grep('modification_.*', colnames(peptides_information_df), value = TRUE)
      }
      
      peptides_information_df %>%
        dplyr::mutate( 
          modification_position = ifelse( !!rlang::sym(target_modification)==max( !!rlang::sym(target_modification) ), 'most_right',
                                          ifelse( !!rlang::sym(target_modification)==min( !!rlang::sym(target_modification) ), 'most_left',
                                                  ifelse( !!rlang::sym(target_modification)!=max( !!rlang::sym(target_modification) | !!rlang::sym(target_modification)!=max( !!rlang::sym(target_modification) ) ), 'inbetween', 'nan') 
                                          ) )
        ) -> peptides_information_df
      
      ### Get series starting from up to modification amino acid
      
      peptides_information_df %>%
        dplyr::mutate(
          
          ion_series_start = ifelse( modification_position=='most_right' &  target_peptide, 
                                     dplyr::filter( dplyr::select(peptides_information_df, !!rlang::sym(target_modification)), peptides_information_df$unimod_peptides!=mod )+1, 
                                     dplyr::filter( dplyr::select(peptides_information_df, !!rlang::sym(target_modification)), peptides_information_df$unimod_peptides==mod )-1
                                     
          ),
          ion_series_end = ifelse( modification_position=='most_right' &  target_peptide, 
                                   dplyr::filter( dplyr::select(peptides_information_df, !!rlang::sym(target_modification)), peptides_information_df$unimod_peptides==mod ), 
                                   dplyr::filter( dplyr::select(peptides_information_df, !!rlang::sym(target_modification)), peptides_information_df$unimod_peptides!=mod )
                                   
          ),
          use_ion_series = ifelse( modification_position=='most_right', 'y', 'b')
          
          
        ) -> peptides_information_df
      
      
      peptides_information_df %>%
        dplyr::mutate( 
          
          ion_series_start_final = ifelse( length(ion_series_start)>1 & use_ion_series=='y', max(unlist(ion_series_start)), min(unlist(ion_series_start)) 
          ),
          ion_series_end_final = ifelse( length(ion_series_end)>1 & use_ion_series=='y', max(unlist(ion_series_end)), min(unlist(ion_series_end)) 
          )
          
        ) -> peptides_information_df
      
      class(peptides_information_df$ion_series_start_final) <- "numeric"
      class(peptides_information_df$ion_series_end_final) <- "numeric"
    } 
    
    ## @TODO: Will need a better way to do this
    if ( !(is.null(top_trans_mod_list)) ){
      
      #### Unique Transitions
      if ( plotIdentifying.Unique==TRUE ){
        df_plot %>%
          dplyr::filter( grepl(glob2rx( paste('*', gsub('\\(UniMod:259\\)|\\(UniMod:267\\)|\\(Label.*)','', gsub('UniMod:35', 'Oxidation', gsub('UniMod:4', 'Carbamidomethyl', gsub('UniMod:21','Phospho',mod)))) ,'*',sep='')), 
                               gsub('.*\\{|\\}.*','',TRAML_ID)) &
                           (!grepl(glob2rx('*\\|*'), 
                                   gsub('.*\\{|\\}.*','',TRAML_ID))) ) %>%
          dplyr::filter( TRANSITION_ID %in% (top_trans_mod_list[[mod]]$transition_id[top_trans_mod_list[[mod]]$transition_pep<1]) ) -> tmp_plot
        
        # Number of Transitions to display
        if ( !is.null(show_n_transitions) ){
          if ( show_n_transitions==-1 ){
            show_n_transitions_val <- length(unique(tmp_plot$TRAML_ID))
          } else {
            show_n_transitions_val <- show_n_transitions
          }
        } else {
          show_n_transitions_val <- 6
        }
        
        tmp_plot %>%
          dplyr::group_by(TRANSITION_ID) %>%
          dplyr::top_n(n=1, wt=Int) %>%
          dplyr::arrange(desc(Int)) %>%
          dplyr::select(TRANSITION_ID) %>%
          head(n=show_n_transitions_val) -> Ordered_top_Int
        
        tmp_plot %>%
          dplyr::filter( TRANSITION_ID %in% as.matrix(Ordered_top_Int) ) -> tmp_plot
        
        
        
        if ( dim(tmp_plot)[1] > 0){
          graphic_obj <- graphic_obj + 
            geom_line(data=tmp_plot, aes(RT, Int, col=Transition), linetype='solid', alpha=0.5, size=1.5, show.legend = show_legend) #+ 
          # theme(legend.text = element_text(size = 2),
          #       legend.key.size = grid::unit(0.5, "cm"))
          #       
          
          # graphic_obj + geom_line(data=tmp_plot, aes(RT, Int, col=Transition), linetype='solid', alpha=0.5, size=1.5, show.legend = show_legend) + facet_zoom(ylim = c(0, 5000))
          
        }
      }
      #### Shared Transitions
      if ( plotIdentifying.Shared==TRUE ){
        # if ( transition_type=='identifying' ){
        #   if( !(is.null(top_trans_mod_list)) ){
        #     df_plot <- df_plot_org
        #     df_plot %>%
        #       dplyr::filter( TRANSITION_ID %in% (top_trans_mod_list[[mod]]$transition_id[top_trans_mod_list[[mod]]$transition_pep<1])[1:10] ) -> df_plot
        #   }
        # }
        df_plot %>%
          dplyr::filter( grepl(glob2rx( paste('*', gsub('\\(UniMod:259\\)|\\(UniMod:267\\)|\\(Label.*)','', gsub('UniMod:35', 'Oxidation', gsub('UniMod:4', 'Carbamidomethyl', gsub('UniMod:21','Phospho',mod)))) ,'*',sep='')), 
                               gsub('.*\\{|\\}.*','',TRAML_ID)) &
                           grepl(glob2rx('*\\|*'), 
                                 gsub('.*\\{|\\}.*','',TRAML_ID)) ) %>%
          dplyr::filter( TRANSITION_ID %in% (top_trans_mod_list[[mod]]$transition_id[top_trans_mod_list[[mod]]$transition_pep<0.6]) ) -> tmp_plot
        
        # Number of Transitions to display
        if ( !is.null(show_n_transitions) ){
          if ( show_n_transitions==-1 ){
            show_n_transitions_val <- length(unique(tmp_plot$TRAML_ID))
          } else {
            show_n_transitions_val <- show_n_transitions
          }
        } else {
          show_n_transitions_val <- 6
        }
        
        tmp_plot %>%
          group_by(TRANSITION_ID) %>%
          top_n(n=1, wt=Int) %>%
          arrange(desc(Int)) %>%
          select(TRANSITION_ID) %>%
          head(n=show_n_transitions_val) -> Ordered_top_Int
        
        tmp_plot %>%
          dplyr::filter( TRANSITION_ID %in% as.matrix(Ordered_top_Int) ) -> tmp_plot
        
        if ( dim(tmp_plot)[1] > 0){
          graphic_obj <- graphic_obj + 
            geom_line(data=tmp_plot, aes(RT, Int, col=Transition), linetype='F1', alpha=0.5, show.legend = show_legend) #+ 
          # theme(legend.text = element_text(size = 2),
          #       legend.key.size = grid::unit(0.5, "cm"))
        }
      }
      ##### Transitions against
      if ( plotIdentifying.Against==TRUE ){
        df_plot %>%
          dplyr::filter( !grepl(glob2rx( paste('*', gsub('\\(UniMod:259\\)|\\(UniMod:267\\)|\\(Label.*)','', gsub('UniMod:35', 'Oxidation', gsub('UniMod:4', 'Carbamidomethyl', gsub('UniMod:21','Phospho',mod)))) ,'*',sep='')), 
                                gsub('.*\\{|\\}.*','',TRAML_ID)) ) %>%
          dplyr::filter( TRANSITION_ID %in% (top_trans_mod_list[[mod]]$transition_id[ top_trans_mod_list[[mod]]$transition_pep<1 ]) ) -> tmp_plot
        
        # Number of Transitions to display
        if ( !is.null(show_n_transitions) ){
          if ( show_n_transitions==-1 ){
            show_n_transitions_val <- length(unique(tmp_plot$TRAML_ID))
          } else {
            show_n_transitions_val <- show_n_transitions
          }
        } else {
          show_n_transitions_val <- 6
        }
        
        tmp_plot %>%
          dplyr::group_by(TRANSITION_ID) %>%
          dplyr::top_n(n=1, wt=Int) %>%
          dplyr::arrange(desc(Int)) %>%
          dplyr::select(TRANSITION_ID) %>%
          head(n=show_n_transitions_val) -> Ordered_top_Int
        
        tmp_plot %>%
          dplyr::filter( TRANSITION_ID %in% as.matrix(Ordered_top_Int) ) -> tmp_plot
        
        if ( dim(tmp_plot)[1] > 0){
          graphic_obj <- graphic_obj + 
            geom_line(data=tmp_plot, aes(RT, Int, col=Transition), linetype='dashed', show.legend = show_legend) #+ 
          # theme(legend.text = element_text(size = 2),
          #       legend.key.size = grid::unit(0.5, "cm"))
        }
      }
    } else {
      #### Unique Identifying Transitions
      if ( plotIdentifying.Unique==TRUE ){
        if( F ) {
          unique(df_plot$TRAML_ID)[!grepl(".*\\|.*", unique(df_plot$TRAML_ID))]
        }
        if ( is.null(transition_selection_list) ) {
          ion_series_start <- peptides_information_df$ion_series_start_final[peptides_information_df$target_peptide]
          ion_series_end <- peptides_information_df$ion_series_end_final[peptides_information_df$target_peptide]
          ## Check which ion series
          if ( peptides_information_df$use_ion_series[ peptides_information_df$target_peptide ]=='y' ){
            ion_series_start <- peptides_information_df$naked_peptide_length[ peptides_information_df$target_peptide ] - ion_series_start + 1
            ion_series_end <- peptides_information_df$naked_peptide_length[ peptides_information_df$target_peptide ] - ion_series_end + 1
          }
          seq_from <- min(c(ion_series_start, ion_series_end))
          seq_to <- max(c(ion_series_start, ion_series_end))
          df_plot %>%
            dplyr::filter( 
              grepl( glob2rx( paste( paste0( peptides_information_df$use_ion_series[peptides_information_df$target_peptide], 
                                             base::seq(
                                               from=seq_from, 
                                               to=seq_to, 
                                               by=1 
                                             )  
              ), collapse = '|' ) 
              ), gsub('.*\\{.*}_\\d+.\\d+_\\d+.\\d+_-\\d+.*_([abcxyz0-9]+).*', '\\1', df_plot$TRAML_ID) 
              
              )
            ) -> tmp_plot
        } else {
          
          # transition_selection_list
          ion_series_keep_regex <- ""
          for ( ion_series in names(transition_selection_list) ){
            
            transition_selection_list[[ ion_series ]]
            
            seq_from <- min(transition_selection_list[[ ion_series ]])
            seq_to <- max(transition_selection_list[[ ion_series ]])
            
            ion_series_keep_regex <- paste(ion_series_keep_regex, paste( paste0( ion_series, 
                                                                                 base::seq(
                                                                                   from=seq_from, 
                                                                                   to=seq_to, 
                                                                                   by=1 
                                                                                 )  
            ), collapse = '|' ), sep="|" )
          }
          ion_series_keep_regex <- gsub("^\\|", "", ion_series_keep_regex)
          message(sprintf("Chosen Identifying Transitions: %s", ion_series_keep_regex))
          df_plot %>%
            dplyr::filter(
              grepl( glob2rx( ion_series_keep_regex ), gsub('.*\\{.*}_\\d+.\\d+_\\d+.\\d+_-\\d+.*_([abcxyz0-9]+).*', '\\1', df_plot$TRAML_ID)
              )
            ) -> tmp_plot
          
        }
        
        
        
        # Number of Transitions to display
        if ( !is.null(show_n_transitions) ){
          if ( show_n_transitions==-1 ){
            show_n_transitions_val <- length(unique(tmp_plot$TRAML_ID))
          } else {
            show_n_transitions_val <- show_n_transitions
          }
        } else {
          show_n_transitions_val <- 6
        }
        
        tmp_plot %>%
          dplyr::group_by(TRANSITION_ID) %>%
          dplyr::top_n(n=1, wt=Int) %>%
          dplyr::arrange(desc(Int)) %>%
          dplyr::select(TRANSITION_ID) %>%
          head(n=show_n_transitions_val) -> Ordered_top_Int
        
        tmp_plot %>%
          dplyr::filter( TRANSITION_ID %in% as.matrix(Ordered_top_Int) ) -> tmp_plot
        
        if ( dim(tmp_plot)[1] > 0){
          
          if( !is.null(transition_dt) ) {
            
            text_info <- sprintf("Transition: %s\nPk_Grp_RT: %s\nPEP: %s\npval: %s\nqval: %s\nscore: %s", 
                                 tmp_plot$Transition, 
                                 tmp_plot$RT.Map, 
                                 checkNumeric(tmp_plot$pep), 
                                 checkNumeric(tmp_plot$pval), 
                                 checkNumeric(tmp_plot$qval), 
                                 checkNumeric(tmp_plot$score) )
            
            graphic_obj <- graphic_obj + 
              ggplot2::geom_line(data=dplyr::select(tmp_plot, c("RT", "Int", "Transition")), aes(RT, Int, col=Transition, text=text_info, group=Transition), linetype='solid', alpha=0.5, size=1.5, show.legend = show_legend) +
              guides(col=guide_legend(title="Identifying")) 
            
          } else {
            ## TODO WHAT IS THIS DOING!?
            graphic_obj <- graphic_obj + 
              ggplot2::geom_line(data= tmp_plot, aes(RT, Int, col=Transition, text=sprintf("Transition: %s", Transition)), 
                                 linetype='solid', alpha=0.5, size=1.5, show.legend = show_legend) +
              guides(col=guide_legend(title="Identifying")) 
          }
          
          
        }
      }
      ##### Shared Identifying Transitions
      if ( plotIdentifying.Shared==TRUE ){
        df_plot %>%
          dplyr::filter( grepl(glob2rx( paste('*', gsub('\\(UniMod:259\\)|\\(UniMod:267\\)|\\(Label.*)','', gsub('UniMod:35', 'Oxidation', gsub('UniMod:4', 'Carbamidomethyl', gsub('UniMod:21','Phospho',mod)))) ,'*',sep='')), 
                               gsub('\\(Label:13C\\(6\\)15N\\(4\\)\\)', '', gsub('.*\\{|\\}.*','',df_plot$TRAML_ID))) &
                           grepl(glob2rx('*\\|*'), 
                                 gsub('\\(Label:13C\\(6\\)15N\\(4\\)\\)', '', gsub('.*\\{|\\}.*','',df_plot$TRAML_ID))) ) -> tmp_plot
        
        # Number of Transitions to display
        if ( !is.null(show_n_transitions) ){
          if ( show_n_transitions==-1 ){
            show_n_transitions_val <- length(unique(tmp_plot$TRAML_ID))
          } else {
            show_n_transitions_val <- show_n_transitions
          }
        } else {
          show_n_transitions_val <- 6
        }
        
        tmp_plot %>%
          dplyr::group_by(TRANSITION_ID) %>%
          dplyr::top_n(n=1, wt=Int) %>%
          dplyr::arrange(desc(Int)) %>%
          dplyr::select(TRANSITION_ID) %>%
          head(n=show_n_transitions_val) -> Ordered_top_Int
        
        tmp_plot %>%
          dplyr::filter( TRANSITION_ID %in% as.matrix(Ordered_top_Int) ) -> tmp_plot
        
        if ( dim(tmp_plot)[1] > 0){
          
          graphic_obj <- graphic_obj + 
            geom_line(data=tmp_plot, aes(RT, Int, col=Transition, text=paste('Transition: ', Transition, sep='')), linetype='F1', show.legend = show_legend) +
            guides(col=guide_legend(title="Identifying"))  #+ 
          # theme(legend.text = element_text(size = 2),
          #       legend.key.size = grid::unit(0.5, "cm"))
        }
      }
      ##### Transitions Against
      if ( plotIdentifying.Against==TRUE ){
        df_plot %>%
          dplyr::filter( !grepl(glob2rx( paste('*', gsub('\\(UniMod:259\\)|\\(UniMod:267\\)|\\(Label.*)','', gsub('UniMod:35', 'Oxidation', gsub('UniMod:4', 'Carbamidomethyl', gsub('UniMod:21','Phospho',mod)))) ,'*',sep='')), 
                                gsub('\\(Label:13C\\(6\\)15N\\(4\\)\\)', '', gsub('.*\\{|\\}.*','',TRAML_ID)) ))  -> tmp_plot
        
        # Number of Transitions to display
        if ( !is.null(show_n_transitions) ){
          if ( show_n_transitions==-1 ){
            show_n_transitions_val <- length(unique(tmp_plot$TRAML_ID))
          } else {
            show_n_transitions_val <- show_n_transitions
          }
        } else {
          show_n_transitions_val <- 6
        }
        
        tmp_plot %>%
          dplyr::group_by(TRANSITION_ID) %>%
          dplyr::top_n(n=1, wt=Int) %>%
          dplyr::arrange(desc(Int)) %>%
          dplyr::select(TRANSITION_ID) %>%
          head(n=show_n_transitions_val) -> Ordered_top_Int
        
        tmp_plot %>%
          dplyr::filter( TRANSITION_ID %in% as.matrix(Ordered_top_Int) ) -> tmp_plot
        
        if ( dim(tmp_plot)[1] > 0){
          graphic_obj <- graphic_obj + 
            geom_line(data=tmp_plot, aes(RT, Int, col=Transition, text=paste('Transition: ', Transition, sep='')), linetype='dashed', show.legend = show_legend) +
            guides(col=guide_legend(title="Identifying"))  #+ 
          # theme(legend.text = element_text(size = 2),
          #       legend.key.size = grid::unit(0.5, "cm"))
        }
      }
    }
  }
  
  MazamaCoreUtils::logger.trace('[DrawAlignR::getXIC] Returning XIC plot object..') 
  
  return( list(graphic_obj=graphic_obj, max_Int=max_Int) )
  
}

