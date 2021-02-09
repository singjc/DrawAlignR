cacheAlignmentPlots <- function( input, output, app.obj, session ){
  MazamaCoreUtils::logger.setLevel("FATAL")
  # observeEvent(input$Reference, {
  withProgress(message = sprintf('Generating and Caching Plots for %s runs...', length(input$Experiment)),
               detail = 'Generating aligned chromatogram plots, and alignment path plots...', value = 0, expr = {
                 for ( i in input$Experiment ) {
                   # Need local so that each item gets its own number. Without it, the value
                   # of i in the renderPlotly() will be the same across all instances, because
                   # of when the expression is evaluated
                   local({
                     message("Generating and Saving Plots\n")
                     # Define Experiment_i
                     current_experiment <- i
                     
                     # Get run Indec
                     run_index <- app.obj$run_index_map[[ current_experiment ]]
                     # Get plot tag for chromatogram output
                     plotname <- paste("plot_run_", run_index, sep="")
                     # Get plot tag for alignment path plot
                     path_plotname <- paste("pathplot_run_", run_index, sep="")
                     message( sprintf("Current Exp: %s of %s with run_idx %s and chrom plot tag %s and alignment path plot tag %s\n", current_experiment, length(input$Experiment), run_index, plotname, path_plotname ) )
                     
                     tryCatch(
                       expr = { 
                         message( sprintf( "Class of AlignObj for run %s is %s with length %s\n", current_experiment, class(app.obj$AlignObj_List[[current_experiment]]), length(app.obj$AlignObj_List[[current_experiment]]) ))

                         ## Generate Plot
                         if ( class(app.obj$AlignObj_List[[current_experiment]])!= "character" & length(app.obj$AlignObj_List[[current_experiment]]) > 0 ){
                           message(sprintf("Generating Aligned Chromatogram from Current Exp: %s of %s with run_idx %s and plotname  %s\n", current_experiment, length(input$Experiment), run_index, plotname ))
                           ## Get Alignment Plots
                           alignedChromsPlot_errorhandle <- tryCatch(
                             expr = {
                             append_error_title <- ""
                             alignedChromsPlot <- plotAlignedAnalytes(AlignObjOutput = app.obj$AlignObj_List[[current_experiment]], DrawAlignR = T, annotatePeak = T, annotateOrgPeak = input$OriginalRTAnnotation, app.obj = app.obj, input = input)
                             alignedChromsPlot_errorhandle <- list(alignedChromsPlot=alignedChromsPlot, append_error_title=append_error_title) 
                           }, error = function(e){
                             alignedChromsPlot <- list()
                             warning( sprintf("Alignment object was returned, but failed alignment plot generation.\nFalling back onto regular plotting\nLast error was %s:", e$message) )
                             append_error_title <- "Alignment Failed - "
                             
                             app.obj$current_chrom_input <- app.obj$chromFile[[ which(grepl(paste0(".*", current_experiment, ".*"), names(app.obj$chromFile))) ]]
                             app.obj$current_osw_input <- app.obj$oswFile[[1]]
                             app.obj$current_peptide <- input$Mod
                             app.obj$current_modification_labels <- regmatches(peptide, gregexpr("\\(.*?\\)", peptide))[[1]]
                             app.obj$current_naked_peptide <-  gsub( paste(gsub('\\)','\\\\)',gsub('\\(','\\\\(',modification_labels)), collapse = '|'), '', peptide )
                             app.obj$current_run_id <- app.obj$run_mapping_table$run_number[app.obj$run_mapping_table$runs==current_experiment]
                             app.obj$current_manual_annotation_coordinates <- NULL
                             
                             alignedChromsPlot$peXpA <- drawChromatogram( app.obj )
                             
                             app.obj$current_chrom_input <- app.obj$chromFile[[ which(grepl(paste0(".*", app.obj$Reference, ".*"), names(app.obj$chromFile))) ]]
                             app.obj$current_osw_input <- app.obj$oswFile[[1]]
                             app.obj$current_peptide <- input$Mod
                             app.obj$current_modification_labels <- regmatches(peptide, gregexpr("\\(.*?\\)", peptide))[[1]]
                             app.obj$current_naked_peptide <-  gsub( paste(gsub('\\)','\\\\)',gsub('\\(','\\\\(',modification_labels)), collapse = '|'), '', peptide )
                             app.obj$current_run_id <- app.obj$run_mapping_table$run_number[app.obj$run_mapping_table$runs==app.obj$Reference]
                             app.obj$current_manual_annotation_coordinates <- NULL
                             
                             alignedChromsPlot$prefU <- drawChromatogram( app.obj )
                             
                             return( list(alignedChromsPlot=alignedChromsPlot, append_error_title=append_error_title) )
                           })

                           
                           ## Store Aligned Experiment Plot
                           pt1 <- plotly::ggplotly( (alignedChromsPlot_errorhandle$alignedChromsPlot$peXpA), tooltip = c("x", "y", "text"), dynamicTicks = T) %>%
                             layout(title = list(text = paste0( paste0(alignedChromsPlot_errorhandle$append_error_title, alignedChromsPlot_errorhandle$alignedChromsPlot$peXpA$labels$title),
                                                                '<br>',
                                                                '<sup>',
                                                                gsub( ' \\| Precursor: \\d+ \\| Peptide: \\d+ \\| Charge: \\d+ | \\| ms2_m-score: .*' , ' ', gsub('\\\n', ' | ', alignedChromsPlot_errorhandle$alignedChromsPlot$peXpA$labels$subtitle)),
                                                                '</sup>'))) %>%
                             event_register(event="plotly_relayout")
                           ## Store plot for experiment run
                           app.obj$alignedChromsPlot[[plotname]] <- pt1
                           
                           ## Check and add Reference plot if not added yet
                           # message(sprintf("HERE REFERENCE PLOTTED?: %s\n", app.obj$reference_plotted))
                           # if ( app.obj$reference_plotted == FALSE ){
                           ref_plotname <- paste("plot_run_", app.obj$run_index_map[[ app.obj$Reference ]], sep="")
                           message(sprintf("Storing Reference plot taken from Current Exp: %s of %s with run_idx %s and plotname  %s\n", current_experiment, length(input$Experiment), run_index, ref_plotname ))
                           pt2 <- plotly::ggplotly( (alignedChromsPlot_errorhandle$alignedChromsPlot$prefU), tooltip = c("x", "y", "text"), dynamicTicks = T) %>%
                             layout(title = list(text = paste0( alignedChromsPlot_errorhandle$alignedChromsPlot$prefU$labels$title,
                                                                '<br>',
                                                                '<sup>',
                                                                gsub( ' \\| Precursor: \\d+ \\| Peptide: \\d+ \\| Charge: \\d+ | \\| ms2_m-score: .*' , ' ', gsub('\\\n', ' | ', alignedChromsPlot_errorhandle$alignedChromsPlot$prefU$labels$subtitle)),
                                                                '</sup>'))) %>%
                             event_register(event="plotly_relayout")
                           ## Store plot for reference run
                           app.obj$alignedChromsPlot[[ref_plotname]] <- pt2
                           # app.obj$reference_plotted <- TRUE
                           # }
                           
                           ## Get Alignment Path Plot
                           alignmentPathPlot <- plotAlignmentPath( AlignObjOutput = app.obj$AlignObj_List[[current_experiment]], title = sprintf("%s - %s Aligned to %s", input$Mod, current_experiment, input$Reference) )

                           pt3 <- alignmentPathPlot
                           ## Store plot for alignment path plot
                           app.obj$alignmentPathPlot[[path_plotname]] <- pt3
                           
                           ref_path_plotname <- paste("pathplot_run_", app.obj$run_index_map[[ app.obj$Reference ]], sep="")
                           plot_text <- sprintf("This is the reference run.\nThere is no self alignment performed.\n ")
                           
                           app.obj$alignmentPathPlot[[ref_path_plotname]] <- plotly::ggplotly( ggplot() +
                                                                                            annotate( "text", x = 4, y = 25, size = 8, label = plot_text ) +
                                                                                            ggtitle( sprintf("Reference Run: %s", app.obj$Reference) ) +
                                                                                            theme_bw() + 
                                                                                            theme( panel.grid.major = element_blank(),
                                                                                                   panel.grid.minor = element_blank(),
                                                                                                   axis.title = element_blank(),
                                                                                                   axis.text = element_blank()
                                                                                            ) )
                           
                           if ( app.obj$Previous_Reference!='' & app.obj$Reference!=app.obj$Previous_Reference ){
                             message( sprintf("Clearing Previous Reference Alignment Path Plot: %s\n", app.obj$Previous_Reference) )
                             ref_path_plotname <- paste("plot_run_", app.obj$run_index_map[[ app.obj$Previous_Reference ]], sep="")
                             app.obj$alignmentPathPlot[[ref_path_plotname]] <- plotly::ggplotly(ggplot())
                           }
                         } else {
                           
                           warning("Alignment Object was empty. Turning back onto default plotting\n")
                           
                           app.obj$current_chrom_input <- app.obj$chromFile[[ which(grepl(paste0(".*", current_experiment, ".*"), names(app.obj$chromFile))) ]]
                           app.obj$current_osw_input <- app.obj$oswFile[[1]]
                           app.obj$current_peptide <- input$Mod
                           app.obj$current_modification_labels <- regmatches(peptide, gregexpr("\\(.*?\\)", peptide))[[1]]
                           app.obj$current_naked_peptide <-  gsub( paste(gsub('\\)','\\\\)',gsub('\\(','\\\\(',modification_labels)), collapse = '|'), '', peptide )
                           app.obj$current_run_id <- app.obj$run_mapping_table$run_number[app.obj$run_mapping_table$runs==current_experiment]
                           app.obj$current_manual_annotation_coordinates <- NULL

                           out.plot.h <- drawChromatogram( app.obj )
                           
                           app.obj$alignedChromsPlot[[plotname]] <- plotly::ggplotly( p = (out.plot.h), source = plotname, tooltip = c("x", "y", "text"), dynamicTicks = T ) %>%
                             plotly::layout(title = list( text = unique(paste0( paste0("Alignment Failed - ", out.plot.h$labels$title),
                                                                                '<br>',
                                                                                '<sup>',
                                                                                gsub( ' \\| Precursor: \\d+ \\| Peptide: \\d+ \\| Charge: \\d+ | \\| ms2_m-score: .*' , ' ', gsub('\\\n', ' | ', out.plot.h$labels$subtitle)),
                                                                                '</sup>')) ) ) %>%
                             event_register(event="plotly_relayout")
                           
                           app.obj$current_chrom_input <- app.obj$chromFile[[ which(grepl(paste0(".*", app.obj$Reference, ".*"), names(app.obj$chromFile))) ]]
                           app.obj$current_osw_input <- app.obj$oswFile[[1]]
                           app.obj$current_peptide <- input$Mod
                           app.obj$current_modification_labels <- regmatches(peptide, gregexpr("\\(.*?\\)", peptide))[[1]]
                           app.obj$current_naked_peptide <-  gsub( paste(gsub('\\)','\\\\)',gsub('\\(','\\\\(',modification_labels)), collapse = '|'), '', peptide )
                           app.obj$current_run_id <- app.obj$run_mapping_table$run_number[app.obj$run_mapping_table$runs==app.obj$Reference]
                           app.obj$current_manual_annotation_coordinates <- NULL
                           

                           out.plot.h <- drawChromatogram( app.obj )
                           
                           ref_plotname <- paste("plot_run_", app.obj$run_index_map[[ app.obj$Reference ]], sep="")
                           app.obj$alignedChromsPlot[[ref_plotname]] <- plotly::ggplotly( p = (out.plot.h + ggplot2::xlab("Reference RT")), source = plotname, tooltip = c("x", "y", "text"), dynamicTicks = T ) %>%
                             plotly::layout(title = list( text = unique(paste0( paste0("Alignment Failed - ", out.plot.h$labels$title),
                                                                                '<br>',
                                                                                '<sup>',
                                                                                gsub( ' \\| Precursor: \\d+ \\| Peptide: \\d+ \\| Charge: \\d+ | \\| ms2_m-score: .*' , ' ', gsub('\\\n', ' | ', out.plot.h$labels$subtitle)),
                                                                                '</sup>')) ) ) %>%
                             event_register(event="plotly_relayout")
                           
                           plot_text <- sprintf("There was an issue while performing alignment.\nWas unable to find an alignment path from similarity matrix.\n You can try adjust one of the alignment settings.\n ")
                           
                           app.obj$alignmentPathPlot[[path_plotname]] <- plotly::ggplotly( ggplot() +
                                                                                            annotate( "text", x = 4, y = 25, size = 8, label = plot_text ) +
                                                                                            ggtitle( sprintf("Run: %s", current_experiment) ) +
                                                                                            theme_bw() + 
                                                                                            theme( panel.grid.major = element_blank(),
                                                                                                   panel.grid.minor = element_blank(),
                                                                                                   axis.title = element_blank(),
                                                                                                   axis.text = element_blank()
                                                                                            ) )
                           
                           ref_path_plotname <- paste("pathplot_run_", app.obj$run_index_map[[ app.obj$Reference ]], sep="")
                           plot_text <- sprintf("This is the reference run.\nThere is no self alignment performed.\n ")
                           
                           app.obj$alignmentPathPlot[[ref_path_plotname]] <- plotly::ggplotly( ggplot() +
                                                                                                annotate( "text", x = 4, y = 25, size = 8, label = plot_text ) +
                                                                                                ggtitle( sprintf("Reference Run: %s", app.obj$Reference) ) +
                                                                                                theme_bw() + 
                                                                                                theme( panel.grid.major = element_blank(),
                                                                                                       panel.grid.minor = element_blank(),
                                                                                                       axis.title = element_blank(),
                                                                                                       axis.text = element_blank()
                                                                                                ) )
                           
                         }
                         
                       }, 
                       error = function(e){
                         message(sprintf("[DrawAlignR::cacheAlignmentPlots] There was the following error that occured during cacheing plots: %s\n", e$message))
                       }
                     ) # End tryCatch
                   }) # End Local 477
                   incProgress(1/length(input$Experiment))
                 } # End for
               }) # End of Progress Tracker
  # }) # End observe
  
}