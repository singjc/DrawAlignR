#' Generic drawChromatogram definition
#' 
#' @param  app.obj(ReactiveValues/list) Object containing app related inputs and parameters, of class DrawAlignR. TODO Add list of inputs with details.
#' @export
setGeneric("drawChromatogram", 
           function(app.obj,
                    type="default", ...){  standardGeneric("drawChromatogram") })

#' drawChromatogram method for class DrawAlignR
#' 
#' @aliases sqmass_data_access-method
#' @docType methods
#' @rdname drawChromatogram
setMethod("drawChromatogram",
          signature=c(app.obj="ANY", 
                      type="character" ),
          function( app.obj, type ){
           
            if ( type=="default" ) {
              MazamaCoreUtils::logger.info(sprintf("[DrawAlignR::drawChromatogram] Generating default XIC Plot..."))
              # saveRDS(app.obj, "app.obj.rds")
              ## TODO Change this to a simple plain chromatogram plot
              out.plot.h <- curateXICplot( pep=app.obj$current_naked_peptide, 
                                           uni_mod=app.obj$current_peptide,
                                           in_sqMass=app.obj$current_chrom_input,  df_lib=app.obj$lib_df, 
                                           in_osw=app.obj$current_osw_input, df_osw=app.obj$osw_df,
                                           plotPrecursor=app.obj$Precursor,
                                           plotDetecting=app.obj$Detecting,
                                           plotIdentifying=app.obj$Identifying_Unique,
                                           plotIdentifying.Unique=app.obj$Identifying_Unique,
                                           plotIdentifying.Shared=F,
                                           plotIdentifying.Against=F,
                                           doFacetZoom=F,
                                           doPlot=T,
                                           Charge_State=app.obj$Charge,
                                           printPlot=F,
                                           store_plots_subdir=NULL,
                                           use_top_trans_pep=F,
                                           transition_selection_list=app.obj$transition_selection_list,
                                           show_n_transitions=app.obj$nIdentifyingTransitions,
                                           show_transition_scores=app.obj$ShowTransitionScores,
                                           transition_dt=app.obj$transition_dt,
                                           show_all_pkgrprnk=app.obj$ShowAllPkGrps,
                                           show_manual_annotation = app.obj$current_manual_annotation_coordinates,
                                           show_peak_info_tbl=F,
                                           show_legend=T,
                                           # mzPntrs=app.obj$mzPntrs[[app.obj$current_run_id]]
                                           mzPntrsdb=app.obj$mzPntrsdb[1],
                                           run_id = app.obj$current_run_id
              )
              MazamaCoreUtils::logger.info(sprintf("[DrawAlignR::drawChromatogram] Returning annotated XIC plot..."))
              return( out.plot.h )
            } else {
              MazamaCoreUtils::logger.info(sprintf("[DrawAlignR::drawChromatogram] Generating other? XIC Plot..."))
              out.plot.h <- curateXICplot( pep=app.obj$current_naked_peptide, 
                                           uni_mod=app.obj$current_peptide,
                                           in_sqMass=app.obj$current_chrom_input,  df_lib=app.obj$lib_df, 
                                           in_osw=app.obj$current_osw_input, df_osw=app.obj$osw_df,
                                           plotPrecursor=app.obj$Precursor,
                                           plotDetecting=app.obj$Detecting,
                                           plotIdentifying=app.obj$Identifying_Unique,
                                           plotIdentifying.Unique=app.obj$Identifying_Unique,
                                           plotIdentifying.Shared=F,
                                           plotIdentifying.Against=F,
                                           doFacetZoom=F,
                                           doPlot=T,
                                           Charge_State=app.obj$Charge,
                                           printPlot=F,
                                           store_plots_subdir=NULL,
                                           use_top_trans_pep=F,
                                           transition_selection_list=app.obj$transition_selection_list,
                                           show_n_transitions=app.obj$nIdentifyingTransitions,
                                           show_transition_scores=app.obj$ShowTransitionScores,
                                           transition_dt=app.obj$transition_dt,
                                           show_all_pkgrprnk=app.obj$ShowAllPkGrps,
                                           show_manual_annotation = app.obj$current_manual_annotation_coordinates,
                                           show_peak_info_tbl=F,
                                           show_legend=T,
                                           # mzPntrs=app.obj$mzPntrs[[app.obj$current_run_id]]
                                           mzPntrsdb = app.obj$mzPntrsdb[1],
                                           run_id = app.obj$current_run_id
              )
              MazamaCoreUtils::logger.info(sprintf("[DrawAlignR::drawChromatogram] Returning annotated XIC plot..."))
              return( out.plot.h )
            }
          }
)