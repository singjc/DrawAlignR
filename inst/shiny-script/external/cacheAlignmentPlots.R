cacheAlignmentPlots <- function( input, output, global, values, session ){
  MazamaCoreUtils::logger.setLevel("FATAL")
  for ( i in input$Experiment ) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlotly() will be the same across all instances, because
    # of when the expression is evaluated
    local({
      message("Generating and Saving Plots\n")
      # Define Experiment_i
      current_experiment <- i
      
      # Get run Indec
      run_index <- values$run_index_map[[ current_experiment ]]
      plotname <- paste("plot_run_", run_index, sep="")
      path_plotname <- paste("pathplot_run_", run_index, sep="")
      cat(sprintf("Current Exp: %s of %s with run_idx and plotname %s : %s\n", current_experiment, length(input$Experiment), run_index, plotname ))
      
      tryCatch(
        expr = { 
          ## Generate Plot
          if ( class(values$AlignObj_List[[current_experiment]])!= "character" ){
            ## Get Alignment Plots
            alignedChromsPlot <- plotAlignedAnalytes(AlignObjOutput = values$AlignObj_List[[current_experiment]], DrawAlignR = T, annotatePeak = T, annotateOrgPeak = input$OriginalRTAnnotation, global = global, input = input)
            ## Store Aligned Experiment Plot
            pt3 <- plotly::ggplotly( (alignedChromsPlot$peXpA), tooltip = c("x", "y", "text"), dynamicTicks = T) %>%
              layout(title = list(text = paste0(alignedChromsPlot$peXpA$labels$title,
                                                '<br>',
                                                '<sup>',
                                                gsub( ' \\| Precursor: \\d+ \\| Peptide: \\d+ \\| Charge: \\d+ | \\| ms2_m-score: .*' , ' ', gsub('\\\n', ' | ', alignedChromsPlot$peXpA$labels$subtitle)),
                                                '</sup>'))) %>%
              event_register(event="plotly_relayout")
            values$alignedChromsPlot[[plotname]] <- pt3
            ## Check and add Reference plot if not added yet
            # message(sprintf("HERE REFERENCE PLOTTED?: %s\n", values$reference_plotted))
            # if ( values$reference_plotted == FALSE ){
              ref_plotname <- paste("plot_run_", values$run_index_map[[ values$Reference ]], sep="")
              cat(sprintf("Storing Reference plot taken from Current Exp: %s of %s with run_idx and plotname %s : %s\n", current_experiment, length(values$Experiment), run_index, plotname ))
              pt1 <- plotly::ggplotly( (alignedChromsPlot$prefU), tooltip = c("x", "y", "text"), dynamicTicks = T) %>%
                layout(title = list(text = paste0(alignedChromsPlot$prefU$labels$title,
                                                  '<br>',
                                                  '<sup>',
                                                  gsub( ' \\| Precursor: \\d+ \\| Peptide: \\d+ \\| Charge: \\d+ | \\| ms2_m-score: .*' , ' ', gsub('\\\n', ' | ', alignedChromsPlot$prefU$labels$subtitle)),
                                                  '</sup>'))) %>%
                event_register(event="plotly_relayout")
              
              values$alignedChromsPlot[[ref_plotname]] <- pt1
              # values$reference_plotted <- TRUE
            # }
            ## Get Alignment Path Plot
            alignmentPathPlot <- plotAlignmentPath( AlignObjOutput = values$AlignObj_List[[current_experiment]], title = sprintf("%s Aligned to %s", current_experiment, values$Reference) )
            suppressWarnings(
              pt3 <- plotly::ggplotly( (alignmentPathPlot), tooltip = c("all"), dynamicTicks = T)
            )
            values$alignmentPathPlot[[path_plotname]] <- pt3
            if ( values$Previous_Reference!='' & values$Reference!=values$Previous_Reference ){
              ref_path_plotname <- paste("plot_run_", values$run_index_map[[ values$Previous_Reference ]], sep="")
              values$alignmentPathPlot[[ref_path_plotname]] <- plotly::ggplotly(ggplot())
            }
          } else {
            text <- sprintf("There was an issue while performing alignment.\nYou most likely need to adjust one of the alignment settings.\n The following error occured: %s",  values$AlignObj_List[[current_experiment]])
            
            values$alignedChromsPlot[[plotname]] <- plotly::ggplotly( ggplot() +
                                                                        annotate( "text", x = 4, y = 25, size = 8, label = text ) +
                                                                        ggtitle( sprintf("Run: %s", current_experiment) ) +
                                                                        theme_bw() + 
                                                                        theme( panel.grid.major = element_blank(),
                                                                               panel.grid.minor = element_blank(),
                                                                               axis.title = element_blank(),
                                                                               axis.text = element_bilank()
                                                                        ) )
            
            ref_plotname <- paste("plot_run_", values$run_index_map[[ values$Reference ]], sep="")
            values$alignedChromsPlot[[ref_plotname]] <- plotly::ggplotly( ggplot() +
                                                                            annotate( "text", x = 4, y = 25, size = 8, label = text ) +
                                                                            ggtitle( sprintf("Run: %s", values$Reference) ) +
                                                                            theme_bw() + 
                                                                            theme( panel.grid.major = element_blank(),
                                                                                   panel.grid.minor = element_blank(),
                                                                                   axis.title = element_blank(),
                                                                                   axis.text = element_bilank()
                                                                            ) )
            
            values$alignmentPathPlot[[path_plotname]] <- plotly::ggplotly( ggplot() +
                                                                        annotate( "text", x = 4, y = 25, size = 8, label = text ) +
                                                                        ggtitle( sprintf("Run: %s", current_experiment) ) +
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
  }
}