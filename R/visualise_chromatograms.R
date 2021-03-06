## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("time", "Transition"))

#' Plot an aligned XIC-group.
#'
#' @details
#' x-axis cannot have the same time-values, therefore, x-axis is indecized.
#'
#' @importFrom zoo na.locf
#' @author Shubham Gupta, \email{shubh.gupta@mail.utoronto.ca}
#'
#' ORCID: 0000-0003-3500-8152
#'
#' License: (c) Author (2019) + MIT
#' Date: 2019-12-13
#' @param XIC_group (list) It is a list of dataframe which has two columns. First column is for time
#'  and second column indicates intensity.
#' @param idx (integer) Indices of aligned chromatograms.
#' @return An adjusted list containing aligned data
getSingleAlignedChrom <- function(XIC_group, idx, t.ref){
  XIC_group_out <- list()
  # Update intensities with aligned time indices.
  for(k in seq_along(XIC_group)){
    tmp_chrom_list <- list()
    mutateInt <- XIC_group[[k]][[2]][idx]
    mutateInt <- zoo::na.locf(zoo::na.locf(mutateInt, na.rm = FALSE),fromLast = TRUE)
    #TODO: interpolate mutateT so that it can be plotted on x-axis.
    mutateT <- mapIdxToTime(XIC_group[[1]][["time"]], idx)
    tmp_chrom_list[[ names(XIC_group[[k]])[1] ]] <- t.ref#c(1:length(mutateInt))
    
    tmp_chrom_list[[ names(XIC_group[[k]])[2] ]] <- mutateInt
    XIC_group_out[[k]] <- tmp_chrom_list
    
  }
  return(XIC_group_out)
}

#' Plot aligned XICs group for a specific peptide.
#'
#' @description
#' AlignObjOutput is the output from getAlignObjs fucntion.
#'
#' @importFrom ggplot2 geom_vline xlab scale_y_continuous
#' @importFrom scales scientific_format
#' @author Shubham Gupta, \email{shubh.gupta@mail.utoronto.ca}
#'
#' ORCID: 0000-0003-3500-8152
#'
#' License: (c) Author (2019) + MIT
#' Date: 2019-12-13
#'
#' @param AlignObj (S4 object)
#' @param refRun A character vector
#' @param eXpRun A character vector
#' @param XICs.ref (list) List of extracted ion chromatograms (dataframe) from reference run. The dataframe has two columns: first column is for time
#'  and second column indicates intensity.
#' @param XICs.eXp (list) List of extracted ion chromatograms (dataframe) from experiment run.The dataframe has two columns: first column is for time
#'  and second column indicates intensity.
#' @param refPeakLabel (numeric vector) It contains peak apex, left width and right width.
#' @param annotatePeak (logical) TRUE: Peak boundaries and apex will be highlighted.
#' @param globalA global variable contaiing chromfiles, oswFile and lib file
#' @param input An input from shiny
#' @return A plot to the current device.
#'
#' @examples
#' dataPath <- system.file("extdata", package = "DIAlignR")
#' runs <- c("hroest_K120809_Strep0%PlasmaBiolRepl2_R04_SW_filt",
#'  "hroest_K120809_Strep10%PlasmaBiolRepl2_R04_SW_filt")
#' AlignObjOutput <- getAlignObjs(analytes = "QFNNTDIVLLEDFQK_3", runs, dataPath = dataPath)
#' AlignObj <- AlignObjOutput[["QFNNTDIVLLEDFQK_3"]][[1]]
#' XICs.ref <- AlignObjOutput[["QFNNTDIVLLEDFQK_3"]][[2]]
#' XICs.eXp <- AlignObjOutput[["QFNNTDIVLLEDFQK_3"]][[3]]
#' refPeakLabel <- AlignObjOutput[["QFNNTDIVLLEDFQK_3"]][[4]]
getAlignedFigs <- function(AlignObj, refRun, eXpRun,  XICs.ref, XICs.eXp, refPeakLabel,
                           annotatePeak = FALSE, annotateOrgPeak = FALSE, app.obj = NULL, input = NULL, smooth_chromatogram = NULL){
  
  AlignedIndices <- cbind(AlignObj@indexA_aligned, AlignObj@indexB_aligned,
                          AlignObj@score)
  colnames(AlignedIndices) <- c("indexAligned.ref", "indexAligned.eXp", "score")
  AlignedIndices <- AlignedIndices[(AlignedIndices[,"indexAligned.ref"] != 0L), ]
  AlignedIndices[, 1:2][AlignedIndices[, 1:2] == 0] <- NA
  t.ref <- XICs.ref[[1]][["time"]]
  t.eXp <- mapIdxToTime( timeVec = XICs.eXp[[1]][["time"]], idx = AlignedIndices[,"indexAligned.eXp"] )
  
  
  ###################### Plot unaligned chromatogram ######################################
  
  ## Get transition information from the osw file
  app.obj$lib_df %>%
    dplyr::filter( UNMODIFIED_SEQUENCE==gsub("\\([a-zA-Z0-9:.]+\\)", '', input$Mod) ) -> transition_table
  
  ## Get OSW data
  m_score_filter_var <- ifelse( length(grep( "m_score|ms2_m_score", colnames(app.obj$osw_df), value = T))==2, "m_score", "ms2_m_score" )
  app.obj$osw_df %>%
    dplyr::filter( Sequence==gsub("\\([a-zA-Z0-9:.]+\\)", '', input$Mod) ) %>%
    dplyr::filter( FullPeptideName==input$Mod ) %>%
    dplyr::filter( !is.na( !!rlang::sym(m_score_filter_var) ) ) %>%
    dplyr::filter( Charge==input$Charge ) -> tmp_osw_df
  
  ##**************************
  ## REFERENCE Chromatogram 
  ##**************************
  
  XICs.ref.rename <- XICs.ref
  
  ## Rename list using transition ids
  names(XICs.ref.rename) <- unlist(lapply(XICs.ref.rename, function(x){ gsub("^X*", "", names(x)[2]) }))
  
  for (i in seq(1:length(XICs.ref.rename))){
    names(XICs.ref.rename[[i]]) <- c('RT','Int')
    if ( length(smooth_chromatogram)>0 ){
      XICs.ref.rename[[i]]$Int <- signal::sgolayfilt( XICs.ref.rename[[i]]$Int, p = smooth_chromatogram$p, n = smooth_chromatogram$n )
    }
  }
  g <- ggplot2::ggplot()
  g <- getXIC( graphic_obj = g, 
                        df_lib = transition_table, 
                        mod = input$Mod, 
                        Isoform_Target_Charge = input$Charge,
                        SCORE_IPF = Score_IPF_Present(app.obj$oswFile[[1]]),
                        chromatogram_file = app.obj$chromFile[ grepl(refRun, names(app.obj$chromFile)) ][[1]], 
                        chromatogram_data_points_list=XICs.ref.rename,
                        transition_type = 'detecting', 
                        uni_mod_list = NULL, 
                        max_Int = NULL, 
                        in_osw=NULL, 
                        smooth_chromatogram=NULL, 
                        doFacetZoom=F, 
                        top_trans_mod_list=NULL, 
                        show_n_transitions=input$nIdentifyingTransitions,
                        transition_dt=NULL )
  max_Int <- g$max_Int
  g <- g$graphic_obj
  
  ##*********************************
  ##     ADD OSW RESULTS INFO     
  ##*********************************
  g <- getXIC( graphic_obj = g, 
                        df_lib = transition_table, 
                        mod = input$Mod, 
                        Isoform_Target_Charge = input$Charge,
                        chromatogram_file = app.obj$chromFile[ grepl(refRun, names(app.obj$chromFile)) ][[1]], 
                        transition_type='none', 
                        max_Int = max_Int, 
                        in_osw = app.obj$oswFile[[1]], 
                        df_osw = subset(tmp_osw_df, grepl(refRun, filename)),
                        SCORE_IPF = Score_IPF_Present( app.obj$oswFile[[1]] ),
                        doFacetZoom=F, 
                        top_trans_mod_list=NULL, 
                        RT_pkgrps=NULL, 
                        show_manual_annotation=NULL, 
                        show_peak_info_tbl=F,
                        FacetFcnCall=NULL, 
                        show_legend = T  )
  max_Int <- g$max_Int
  g <- g$graphic_obj
  prefU <- g + ggplot2::xlab("Reference RT")
  
  ##**************************
  ## Experiment Chromatogram
  ##**************************
  if ( F ){
    message("Plotting Experiment Chromatogram")
    
    XICs.eXp.rename <- XICs.eXp
    
    ## Rename list using transition ids
    names(XICs.eXp.rename) <- unlist(lapply(XICs.eXp.rename, function(x){ gsub("^X*", "", names(x)[2]) }))
    
    for (i in seq(1:length(XICs.eXp.rename))){
      names(XICs.eXp.rename[[i]]) <- c('RT','Int')
      if ( length(smooth_chromatogram)>0 ){
        XICs.eXp.rename[[i]]$Int <- signal::sgolayfilt( XICs.eXp.rename[[i]]$Int, p = smooth_chromatogram$p, n = smooth_chromatogram$n )
      }
    }
    
    g <- ggplot2::ggplot()
    invisible( capture.output(suppressWarnings(
      g <- getXIC( graphic_obj = g,
                            df_lib = transition_table,
                            mod = input$Mod,
                            Isoform_Target_Charge = input$Charge,
                            SCORE_IPF = Score_IPF_Present( app.obj$oswFile[[1]] ),
                            chromatogram_file = app.obj$chromFile[ grepl(eXpRun, names(app.obj$chromFile)) ][[1]],
                            chromatogram_data_points_list=XICs.eXp.rename,
                            transition_type = 'detecting',
                            uni_mod_list = NULL,
                            max_Int = NULL,
                            in_osw=NULL,
                            smooth_chromatogram=NULL,
                            doFacetZoom=F,
                            top_trans_mod_list=NULL,
                            show_n_transitions=input$nIdentifyingTransitions,
                            transition_dt=NULL )
    )))
    max_Int <- g$max_Int
    g <- g$graphic_obj
    
    ##*********************************
    ##     ADD OSW RESULTS INFO     
    ##*********************************
    invisible( capture.output(suppressWarnings(
      g <- getXIC( graphic_obj = g,
                            df_lib = transition_table,
                            mod = input$Mod,
                            Isoform_Target_Charge = input$Charge,
                            chromatogram_file = app.obj$chromFile[ grepl(eXpRun, names(app.obj$chromFile)) ][[1]],
                            transition_type='none',
                            max_Int = max_Int,
                            in_osw = app.obj$oswFile[[1]],
                            SCORE_IPF = Score_IPF_Present( app.obj$oswFile[[1]] ),
                            doFacetZoom=F,
                            top_trans_mod_list=NULL,
                            RT_pkgrps=NULL,
                            show_manual_annotation=NULL,
                            show_peak_info_tbl=F,
                            FacetFcnCall=NULL,
                            show_legend = T  )
    )))
    max_Int <- g$max_Int
    g <- g$graphic_obj
    
    peXpU <- g + ggplot2::xlab("Experiment RT")
  }
  ###################### Plot aligned chromatogram ######################################
  
  ##***********************************
  ## Aligned Experiment Chromatogram 
  ##***********************************
  
  XICs.eXp.aligned <- getSingleAlignedChrom(XIC_group = XICs.eXp, idx = AlignedIndices[,"indexAligned.eXp"], t.ref)
  
  ## Rename list using transition ids
  names(XICs.eXp.aligned) <- unlist(lapply(XICs.eXp.aligned, function(x){ gsub("^X*", "", names(x)[2]) }))
  
  for (i in seq(1:length(XICs.eXp.aligned))){
    names(XICs.eXp.aligned[[i]]) <- c('RT','Int')
    if ( length(smooth_chromatogram)>0 ){
      XICs.eXp.aligned[[i]]$Int <- signal::sgolayfilt( XICs.eXp.aligned[[i]]$Int, p = smooth_chromatogram$p, n = smooth_chromatogram$n )
    }
  }
  
  g <- ggplot2::ggplot()
  g <-  getXIC( graphic_obj = g, 
                         df_lib = transition_table, 
                         mod = input$Mod, 
                         Isoform_Target_Charge = input$Charge,
                         SCORE_IPF = Score_IPF_Present( app.obj$oswFile[[1]] ),
                         chromatogram_file = app.obj$chromFile[ grepl(eXpRun, names(app.obj$chromFile)) ][[1]], 
                         chromatogram_data_points_list=XICs.eXp.aligned,
                         transition_type = 'detecting', 
                         uni_mod_list = NULL, 
                         max_Int = NULL, 
                         in_osw=NULL, 
                         smooth_chromatogram=NULL, 
                         doFacetZoom=F, 
                         top_trans_mod_list=NULL, 
                         show_n_transitions=input$nIdentifyingTransitions,
                         transition_dt=NULL )
  max_Int <- g$max_Int
  g <- g$graphic_obj
  
  ##*********************************
  ##     ADD OSW RESULTS INFO     
  ##*********************************
  g <-  getXIC( graphic_obj = g, 
                         df_lib = transition_table, 
                         mod = input$Mod, 
                         Isoform_Target_Charge = input$Charge,
                         chromatogram_file = app.obj$chromFile[ grepl(eXpRun, names(app.obj$chromFile)) ][[1]], 
                         transition_type='none', 
                         max_Int = max_Int, 
                         in_osw = app.obj$oswFile[[1]], 
                         df_osw = subset(tmp_osw_df, grepl(eXpRun, filename)),
                         SCORE_IPF = Score_IPF_Present( app.obj$oswFile[[1]] ),
                         annotate_best_pkgrp=F,
                         doFacetZoom=F, 
                         top_trans_mod_list=NULL, 
                         RT_pkgrps=NULL, 
                         show_manual_annotation=NULL, 
                         show_peak_info_tbl=F,
                         FacetFcnCall=NULL, 
                         show_legend = T  )
  max_Int <- g$max_Int
  g <- g$graphic_obj
  
  peXpA <- g + ggplot2::xlab("Experiment Aligned RT")
  
  if(annotatePeak){
    # peXpA <- peXpA +
    #   geom_vline(xintercept=which.min(abs(t.ref - refPeakLabel$RT[1])),
    #              color='red', size = 1.3, alpha = 0.65) +
    #   geom_vline(xintercept=which.min(abs(t.ref - refPeakLabel$leftWidth[1])),
    #              color='red', linetype='dotted', size=1, alpha = 0.8) +
    #   geom_vline(xintercept=which.min(abs(t.ref - refPeakLabel$rightWidth[1])),
    #              color='red', linetype='dotted', size=1, alpha = 0.8)
    
    peXpA <- peXpA +
      geom_vline(xintercept=refPeakLabel$RT[1],
                 color='red', size = 1.3, alpha = 0.65) +
      geom_vline(xintercept=refPeakLabel$leftWidth[1],
                 color='red', linetype='dotted', size=1, alpha = 0.8) +
      geom_vline(xintercept=refPeakLabel$rightWidth[1],
                 color='red', linetype='dotted', size=1, alpha = 0.8)
    
  }
  
  if ( annotateOrgPeak ){
    
    use_ipf_score <- Score_IPF_Present( app.obj$oswFile[[1]] )
    
    ## Experiment
    subset(tmp_osw_df, grepl(eXpRun, filename)) -> osw_dt
    
    osw_dt %>%
      dplyr::filter( Charge==input$Charge ) -> osw_dt
    
    ## Get data for the best peak as defined by the feature with the lowest IPF posterior error probability 
    if ("m_score" %in% colnames(osw_dt)) {
      osw_dt <- osw_dt %>% dplyr::filter(m_score == min(m_score))
    }  else {
      osw_dt <- osw_dt %>% dplyr::filter(ms2_m_score == min(ms2_m_score))
    }
    
    if ( dim(osw_dt)[1] > 1 ){
      osw_dt %>%
        dplyr::filter( peak_group_rank==min(peak_group_rank) ) -> osw_dt
    }
    
    ## Reference
    osw_dt_ref <- subset(tmp_osw_df, grepl(refRun, filename))
    
    osw_dt_ref %>%
      dplyr::filter( Charge==input$Charge ) -> osw_dt_ref
    
    # ## Get data for the best peak as defined by the feature with the lowest IPF posterior error probability 
    # if ("m_score" %in% colnames(osw_dt)) {
    #   osw_dt <- osw_dt %>% dplyr::filter(m_score == min(m_score))
    # }  else {
    #   osw_dt <- osw_dt %>% dplyr::filter(ms2_m_score == min(ms2_m_score))
    # }
    # 
    # if ( dim(osw_dt)[1] > 1 ){
    #   osw_dt %>%
    #     dplyr::filter( peak_group_rank==min(peak_group_rank) ) -> osw_dt
    # }
    
    df <- data.frame(
      index.ref = AlignedIndices[, 'indexAligned.ref'],
      index.exp = AlignedIndices[, 'indexAligned.eXp'],
      rt.ref = XICs.ref[[1]][['time']][ AlignedIndices[, 'indexAligned.ref'] ],
      rt.exp = XICs.eXp[[1]][['time']][ AlignedIndices[, 'indexAligned.eXp'] ]
    )
    
    # refPeakLabelIndex <- df$index.ref[ which.min(abs(df$rt.ref - refPeakLabel$RT)) ]
    # expCorresponding_refPeakLabelIndex <- df$index.exp[ which.min(abs(df$index.exp - refPeakLabelIndex)) ]
    
    expPeakLabelIndex <- which.min(abs(df$rt.exp - osw_dt$RT))
    
    # matching_refIndex <- df$index.ref[ match(df$index.exp[ expPeakLabelIndex ], df$index.ref) ]
    
    refCorresponding_rt <- df$rt.ref[ expPeakLabelIndex ]
    
    refCorresponding_rt_osw <- osw_dt_ref[ which.min(abs(osw_dt_ref$RT - refCorresponding_rt)), ]
    
    
    # XICs.eXp[[1]][['time']][ AlignedIndices[, 'indexAligned.eXp'] ]
    # XICs.ref[[1]][["time"]][ AlignedIndices[, 'indexAligned.ref'] ]
    if ( dim(refCorresponding_rt_osw)[1]>0 ){
      peXpA <- peXpA +
        geom_rect(data = data.frame(xmin = refCorresponding_rt_osw$leftWidth,
                                    xmax = refCorresponding_rt_osw$rightWidth,
                                    ymin = max(unlist(lapply(XICs.eXp.aligned, function(x){ x$Int}))),
                                    ymax = 0),
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = "blue", alpha = 0.2)
    } else {
      warning( "[DrawAlignR::getAlignedFigs] Could no find Original Peak relative to index...")
    }
  }
  
  ###################### return ggplot objects ######################################
  figs <- list("prefU" = prefU, "peXpA" = peXpA)
  figs
}

#' Plot aligned XICs group for a specific peptide.
#' AlignObjOutput is the output from getAlignObjs fucntion.
#'
#' @importFrom gridExtra grid.arrange
#' @author Shubham Gupta, \email{shubh.gupta@mail.utoronto.ca}
#'
#' ORCID: 0000-0003-3500-8152
#'
#' License: (c) Author (2019) + MIT
#' Date: 2019-12-13
#'
#' @param AlignObjOutput (list) The list contains AlignObj, raw XICs for reference and experiment, and reference-peak label.
#' @param plotType This must be one of the strings "All", "onlyUnaligned" and "onlyAligned".
#' @param DrawAlignR (logical) TRUE: ggplot objects will be returned.
#' @param annotatePeak (logical) TRUE: Peak boundaries and apex will be highlighted.
#' @param saveFigs (logical) TRUE: Figures will be saved in AlignedAnalytes.pdf .
#' @return A plot to the current device.
#'
#' @examples
#' dataPath <- system.file("extdata", package = "DIAlignR")
#' runs <- c("hroest_K120809_Strep0%PlasmaBiolRepl2_R04_SW_filt",
#'  "hroest_K120809_Strep10%PlasmaBiolRepl2_R04_SW_filt")
#' AlignObjOutput <- getAlignObjs(analytes = "QFNNTDIVLLEDFQK_3", runs, dataPath = dataPath)
#' plotAlignedAnalytes(AlignObjOutput)
#' @export
plotAlignedAnalytes <- function(AlignObjOutput, plotType = "All", DrawAlignR = FALSE,
                                annotatePeak = FALSE, annotateOrgPeak = FALSE, saveFigs = FALSE, app.obj = NULL, input = NULL){
  
  if((length(AlignObjOutput) > 1) | saveFigs){
    grDevices::pdf("AlignedAnalytes.pdf")
  }
  
  for(i in seq_along(AlignObjOutput)){
    if(is.null(AlignObjOutput[[i]])){
      next
    }
    AlignObj <- AlignObjOutput[[i]][[1]]
    XICs.ref <- AlignObjOutput[[i]][[2]]
    XICs.eXp <- AlignObjOutput[[i]][[3]]
    refPeakLabel <- AlignObjOutput[[i]][[4]]
    analyte <- names(AlignObjOutput)[i]
    refRun <- names(AlignObjOutput[[i]])[2]
    eXpRun <- names(AlignObjOutput[[i]])[3]
    
    tictoc::tic()
    figs <- getAlignedFigs(AlignObj, refRun, eXpRun, XICs.ref, XICs.eXp, refPeakLabel, annotatePeak, annotateOrgPeak, app.obj, input)
    exec_time <- tictoc::toc(quiet = TRUE)
    message( sprintf("[DrawAlignR::plotAlignedAnalytes::getAlignedFigs] Generating reference chromatogram and aligned experiment chomatogram took %s seconds", round(exec_time$toc - exec_time$tic, 3) ))
    
    if(DrawAlignR){
      message("Succesfully drew ref, and exp aligned figs\n")
      return(figs)
    }
    
    if(plotType == "onlyAligned"){
      grid.arrange(figs[["prefU"]], figs[["peXpA"]], nrow=2, ncol=1,
                   top = paste0(analyte,"\n", "ref: ", refRun, "\n", "eXp: ", eXpRun ))
    } else if(plotType == "onlyUnaligned"){
      grid.arrange(figs[["prefU"]], figs[["peXpU"]], nrow=2, ncol=1,
                   top = paste0(analyte,"\n", "ref: ", refRun, "\n", "eXp: ", eXpRun ))
    } else{
      grid.arrange(figs[["peXpU"]], figs[["prefU"]], figs[["peXpA"]],
                   nrow=3, ncol=1, top = paste0(analyte,"\n", "ref: ", refRun, "\n", "eXp: ", eXpRun ))
    }
  }
  if((length(AlignObjOutput) > 1) | saveFigs){
    grDevices::dev.off()
  }
}


#' Visualize alignment path through similarity matrix
#'
#' Plot aligned path through the similarity matrix. Reference run has indices on X-axis, eXp run has them on Y-axis.
#' In getAlignObjs function, objType must be set to medium.
#'
#' @author Shubham Gupta, \email{shubh.gupta@mail.utoronto.ca}
#'
#' ORCID: 0000-0003-3500-8152
#'
#' License: (c) Author (2019) + MIT
#' Date: 2019-12-13
#' @param AlignObjOutput (list) The list contains AlignObj, raw XICs for reference and experiment, and reference-peak label.
#' @param title (char) Title to use for plot
#' @return A plot to the current device.
#'
#' @examples
#' library(lattice)
#' dataPath <- system.file("extdata", package = "DIAlignR")
#' runs <- c("hroest_K120809_Strep0%PlasmaBiolRepl2_R04_SW_filt",
#'  "hroest_K120809_Strep10%PlasmaBiolRepl2_R04_SW_filt")
#' AlignObjOutput <- getAlignObjs(analytes = "QFNNTDIVLLEDFQK_3", runs, dataPath = dataPath,
#'  objType = "medium")
#' plotAlignmentPath(AlignObjOutput)
#' @importFrom ggplot2 ggplot geom_tile geom_contour ggtitle labs theme aes element_blank
#' @importFrom plotly ggplotly layout hide_legend
#' @importFrom ggpubr clean_theme rotate
#' @export
plotAlignmentPath <- function(AlignObjOutput, title=NULL){
  tictoc::tic()
  
  Alignobj <- AlignObjOutput[[1]][[1]]
  XICs.ref <- AlignObjOutput[[1]][[2]]
  XICs.eXp <- AlignObjOutput[[1]][[3]]
  refPeakLabel <- AlignObjOutput[[1]][[4]]
  refRun <- names(AlignObjOutput[[1]])[2]
  eXpRun <- names(AlignObjOutput[[1]])[3]
  analyte <- names(AlignObjOutput)[1]
  
  s <- Alignobj@s
  Path <- Alignobj@path[2:nrow(Alignobj@path), 2:ncol(Alignobj@path)]
  # lattice::levelplot(s, axes = TRUE, xlab = "ref index", ylab = "eXp index",
  #                    main = paste0("Hybrid alignment through the similarity matrix\n for ",
  #                                  analyte), fontsize = 7) +
  #   latticeExtra::as.layer(lattice::levelplot(Path, col.regions = c("transparent", "green"),
  #                                             alpha = 1, axes = FALSE))
  suppressWarnings(
    Weight_dt <- data.table::melt( s )
  )
  suppressWarnings(
    Path_dt <- data.table::melt( Path )
  )
  
  plot_path <- ggplot2::ggplot() +
    ggplot2::geom_tile(data=Weight_dt, ggplot2::aes(Var1, Var2, fill = value)) + 
    ggplot2::geom_contour(data=Weight_dt, ggplot2::aes(Var1, Var2, z = value)) 
  
  if ( 1 %in% Path ){
    message("[DrawAlignR::plotAlignmentPath] Adding alignment path to weighted contour plot.\n")
    plot_path <- plot_path + ggplot2::geom_contour(data=Path_dt, ggplot2::aes(Var1, Var2, z = value), colour = "red") +
      # ggplot2::ggtitle( title ) +
      ggplot2::labs(x="ref Index", y="eXp Index") +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     legend.position = "none")
    plot_subtitle <- ""
  } else {
    message("[DrawAlignR::plotAlignmentPath] No alignment path found.\n")
    plot_path <- plot_path + 
      ggplot2::ggtitle( title ) +
      ggplot2::labs(x="ref Index", y="eXp Index", subtitle = "Algorithm was not able to find a path..") +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     legend.position = "none" )
    plot_subtitle <- "Algorithm was not able to find a path.."
  }
  
  p0 <- plotly::ggplotly(plot_path, dynamicTicks = T)  %>% layout(showlegend = FALSE)
  
  ref_data <- data.table::rbindlist(XICs.ref, idcol="transition")
  colnames(ref_data)[3] <- "int"
  ref_data %>%
    dplyr::group_by( transition ) %>%
    dplyr::mutate( index = seq(1:length(transition)) ) %>%
    dplyr::ungroup() -> ref_data
  
  exp_data <- data.table::rbindlist(XICs.eXp, idcol="transition")
  colnames(exp_data)[3] <- "int"
  exp_data %>%
    dplyr::group_by( transition ) %>%
    dplyr::mutate( index = seq(1:length(transition)) ) %>%
    dplyr::ungroup()  -> exp_data
  
  class(ref_data$transition) <- "character"
  class(exp_data$transition) <- "character"
  
  p1 <- plotly::ggplotly( ggplot(ref_data, aes(x=index, y=int, col=transition)  ) + geom_line()  + theme_bw() +
                            theme(axis.line = element_line(colour = "black"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_blank(),
                                  panel.background = element_blank())  + ggpubr::clean_theme(),
                          dynamicTicks = T
  ) %>% layout(showlegend = FALSE)
  
  p2 <- plotly::ggplotly( ggplot(exp_data, aes(x=index, y=int, col=transition)  ) + geom_line()  +
                            theme(axis.line = element_line(colour = "black"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_blank(),
                                  panel.background = element_blank()) + ggpubr::rotate() + ggpubr::clean_theme(),
                          dynamicTicks = T
  ) %>% layout(showlegend = FALSE)
  
  # alignment_path_plot <- ggpubr::ggarrange(p1, NULL, plotly::ggplotly(plot_path), p2, 
  #           ncol = 2, nrow = 2,  align = "hv", 
  #           widths = c(5, 1), heights = c(1, 5),
  #           common.legend = TRUE, legend = 'none')
  
  alignment_path_plot <- plotly::hide_legend( plotly::subplot( p1, plotly_empty(), p0, p2, nrows = 2, shareX = T, shareY = T, which_layout = c(3), margin = c(0) ) ) %>%
    layout(title = list(text = paste0( title,
                                       '<br>',
                                       '<sup>',
                                       plot_subtitle,
                                       '</sup>')))
  
  exec_time <- tictoc::toc(quiet = TRUE)
  message( sprintf("[DrawAlignR::plotAlignmentPath] Generating alignment path plot for %s took %s seconds.\n", title, round(exec_time$toc - exec_time$tic, 3) ))
  
  
  return( alignment_path_plot )
}

