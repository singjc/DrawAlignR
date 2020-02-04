server_help_description_text <- function( input, output, session ){
  ## popover help text for working directory/ individual file button switch toggle
  addPopover(session = session, 
             id = "WorkingDirectoryInput_help", 
             title = "Use a Working Directory?", 
             content = paste0("<p>You can use a general working directory that contains sub-folders osw, pqp, mzml and sqmass with respective files.</p><p>Or alternatively if you toggle the button off then you can select files individually using separate input buttons.</p>"), trigger = 'click') 
  ## popover help text for chromatogram file inpit
  addPopover(session = session, 
             id = "ChromatogramFile_help", 
             title = "Select Chromatogram File(s)", 
             content = paste0("<p>You can supply several chromatogram files using the *Chromatogram Path(s)* button. Accepted formats are: mzML and sqMass.</p><p> If you have an alternative format you would like to use, submit a feature request at Roestlab/DrawAlignR.</p>"), trigger = 'click')
  ## popover help text for chromatogram file inpit
  addPopover(session = session, id = "LibraryFile_help", 
             title = "Select a Library file", 
             content = paste0("<p>You can supply a Library File using the *PQP Path* button. Accepted formats are: pqp.</p><p> If you have an alternative format you would like to use, submit a feature request at Roestlab/DrawAlignR.</p>"), trigger = 'click') 
  ## popover help text for chromatogram file inpit
  addPopover(session = session, id = "OSWFile_help", 
             title = "Select a merged OSW results file", 
             content = paste0("<p>You can supply several chromatogram files using the *OSW Path button*. Accepted formats are: osw.</p><p> If you have an alternative format you would like to use, submit a feature request at Roestlab/DrawAlignR.</p>"), trigger = 'click') 
  
}
