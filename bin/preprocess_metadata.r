#!/usr/bin/env Rscript
# Preprocess ship metadata.
# Imputes missing data and derives new columns (e.g. Length, FlagNameBinned)
#
# usage:
#
# Rscript preprocess_metadata.r -i metadata-raw.csv -o metadata-imputed.csv --imputation_method 'rf'
#
library('optparse', warn.conflicts = F, quietly = T)

if(Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME') == ''){
  stop('R_CLIMATE_TRACE_SHIPPING_HOME not in environment variables. Please add "R_CLIMATE_TRACE_SHIPPING_HOME=/path/to/r/climate/trace/shipping/repo" to the file .Renviron in your home directory. Read more about the .Renviron file here: https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf')
}
CTHOME=Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME')

set.seed(123456)

# Define and get commandline options
option_list <- list(
  make_option(c("-i", "--input"),
              help="Input metadata CSV. Must have IMO numbers in first column. [required]"),
  make_option(c("-o", "--output"),
              help = "Output metadata CSV [required]"),
  make_option(c("--imputation_method"), default='rf',
              help="Imputation method: quick (median/mode) or rf (much more accurate but very slow) [default \"%default\"]"),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
              help="Verbose output [default %default]")
)
opt <- parse_args(OptionParser(option_list=option_list))

# check that all required options are provided
required.opts <- c('input','output')
if(any(!(required.opts %in% names(opt)))){
  stop(sprintf('One or more of these required parameters is missing: %s.\n\n', paste(required.opts,collapse=', ')))
}

# Now load other code; waited till here to perform
# input validations first
source(paste(CTHOME,'/lib/load.r',sep=''))
if(opt$imputation_method == 'quick') library('randomForest')
# Create output directory
dir.create(dirname(opt$output),showWarnings = FALSE)

# Run imputation and preprocessing
m.post <- preprocess.ship.metadata(opt$input,
                                   metadata.output.filepath = opt$output,
                                   imputation.method = opt$imputation_method)
