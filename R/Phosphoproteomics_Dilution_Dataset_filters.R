filter_sequences <- c('ANSSPTTNIDHLK', 'ESTAEPDSLSR', 'KDSNTNIVLLK', 'NKESPTKAIVR')

file <- list.files("/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Synthetic_Dilution_Phosphoproteomics/pqp/", pattern = "*.pqp", full.names = T )
cat("Working on file:", file, "\n")
mstools::filterPQPdb(file, filter_sequences)

file <- list.files("/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Synthetic_Dilution_Phosphoproteomics/osw/", pattern = "*.osw", full.names = T )
cat("Working on file:", file, "\n")
mstools::filterOSWdb(file, filter_sequences)

files <- list.files("/media/justincsing/ExtraDrive1/Documents2/Roest_Lab/Github/DrawAlignR/inst/extdata/Synthetic_Dilution_Phosphoproteomics/sqmass/", pattern = "*.sqMass", full.names = T )
for ( file in files ){
  cat("Working on file:", file, "\n")
  mstools::filterSQMASSdb(file, filter_sequences)
}