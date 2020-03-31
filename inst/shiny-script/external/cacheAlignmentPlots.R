cacheAlignmentPlots <- function( input, output, global, values, session ){
  MazamaCoreUtils::logger.setLevel("FATAL")
  for ( i in input$Experiment ) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlotly() will be the same across all instances, because
    # of when the expression is evaluated
    local({
      message("Generating and Save Plots\n")
      # Define Experiment_i
      current_experiment <- i
      
      # Get run Indec
      run_index <- values$run_index_map[[ current_experiment ]]
      plotname <- paste("plot_run_", run_index, sep="")
      cat(sprintf("Current Exp: %s of %s with run_idx and plotname %s : %s\n", current_experiment, length(input$Experiment), run_index, plotname ))
      
      tryCatch(
        expr = { 
          print("Getting Plots...")
          cat( sprintf( "Names in AlignOPObjs: %s\n", names(values$AlignObj_List) ) )
          ## Generate Plot
          if ( class(values$AlignObj_List[[current_experiment]])!= "character" ){
            alignedChromsPlot <- plotAlignedAnalytes(AlignObjOutput = values$AlignObj_List[[current_experiment]], DrawAlignR = T, annotatePeak = T, annotateOrgPeak = input$OriginalRTAnnotation, global = global, input = input)
            pt3 <- plotly::ggplotly( (alignedChromsPlot$peXpA), tooltip = c("x", "y", "text"), dynamicTicks = T) %>%
              layout(title = list(text = paste0(alignedChromsPlot$peXpA$labels$title,
                                                '<br>',
                                                '<sup>',
                                                gsub( ' \\| Precursor: \\d+ \\| Peptide: \\d+ \\| Charge: \\d+ | \\| ms2_m-score: .*' , ' ', gsub('\\\n', ' | ', alignedChromsPlot$peXpA$labels$subtitle)),
                                                '</sup>')))
            values$alignedChromsPlot[[plotname]] <- pt3
            ## Check and add Reference plot if not added yet
            if ( values$reference_plotted == FALSE ){
              ref_plotname <- paste("plot_run_", values$run_index_map[[ input$Reference ]], sep="")
              cat(sprintf("Storing Reference plot taken from Current Exp: %s of %s with run_idx and plotname %s : %s\n", current_experiment, length(input$Experiment), run_index, plotname ))
              pt1 <- plotly::ggplotly( (alignedChromsPlot$prefU), tooltip = c("x", "y", "text"), dynamicTicks = T) %>%
                layout(title = list(text = paste0(alignedChromsPlot$prefU$labels$title,
                                                  '<br>',
                                                  '<sup>',
                                                  gsub( ' \\| Precursor: \\d+ \\| Peptide: \\d+ \\| Charge: \\d+ | \\| ms2_m-score: .*' , ' ', gsub('\\\n', ' | ', alignedChromsPlot$prefU$labels$subtitle)),
                                                  '</sup>')))
              values$alignedChromsPlot[[ref_plotname]] <- pt1
              print( values$alignedChromsPlot[[ref_plotname]] )
              values$reference_plotted <- TRUE
            }
            alignmentPathPlot <- plotAlignmentPath( AlignObjOutput = values$AlignObj_List[[current_experiment]], title = sprintf("%s Aligned to %s", current_experiment, input$Reference) )
            pt3 <- plotly::ggplotly( (alignmentPathPlot), tooltip = c("all"), dynamicTicks = T)
            values$alignmentPathPlot[[plotname]] <- pt3
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
            
            ref_plotname <- paste("plot_run_", values$run_index_map[[ input$Reference ]], sep="")
            values$alignedChromsPlot[[ref_plotname]] <- plotly::ggplotly( ggplot() +
              annotate( "text", x = 4, y = 25, size = 8, label = text ) +
              ggtitle( sprintf("Run: %s", current_experiment) ) +
              theme_bw() + 
              theme( panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.title = element_blank(),
                     axis.text = element_bilank()
              ) )
            
            values$alignmentPathPlot[[plotname]] <- plotly::ggplotly( ggplot() +
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