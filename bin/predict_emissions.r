#!/usr/bin/env Rscript
# Predict carbon emissions (kg) per nautical mile for new ships,
# using model trained on other ships.
#
# usage:
#
# Predict newships using model in output/final_model.rdata
# Adds a column "kg.CO2.per.nm.predicted" to the output csv
# Rscript predict_emissions.r -i newships.csv -m ship_metadata.csv -M output/final_model.rdata  -o newships-predicted.csv
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
              help="Input data CSV. Must have a column containing IMO Numbers. [required]"),
  make_option(c("-I", "--IMO_column"), default='IMO.Number',
              help="Name of column containing IMO Numbers [default \"%default\"]"),
  make_option(c("-m", "--metadata"), 
              help = "Ship metadata file, e.g. \"IHS complete Ship Data.csv,\" with columns IMO Numbers in column 1. [required]"),
  make_option(c("-R", "--raw_metadata"), action="store_true", default=FALSE,
              help = "Ship metadata file is raw, meaning requires imputing missing data, binning flagnames, etc. [default %default]"),
  make_option(c("-M", "--model_file"), 
              help = "Predictive model in .rdata format, output of train_and_evaluate_models.r. [required]"),
  make_option(c("--imputation_method"), default='rf',
              help="Imputation method: quick (median/mode) or rf (much more accurate but very slow) [default \"%default\"]"),
  make_option(c("-o", "--output_file"),
              help = "Output csv file [required]"),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
              help="Verbose output [default %default]")
)
opt <- parse_args(OptionParser(option_list=option_list))

# check that all required options are provided
required.opts <- c('input','metadata','model_file','output_file')
if(any(!(required.opts %in% names(opt)))){
  stop(sprintf('One or more of these required parameters is missing: %s.\n\n', paste(required.opts,collapse=', ')))
}

# Now load other code; waited till here to perform
# input validations first
source(paste(CTHOME,'/lib/training-general.r',sep=''))
source(paste(CTHOME,'/lib/load.r',sep=''))

# Create output directory if needed
dir.create(dirname(opt$output_file),showWarnings = FALSE)

if(opt$verbose) cat('Loading model from',opt$model_file,'\n')
load(opt$model_file)

# load new data
if(opt$verbose) cat('Loading ship data and metadata...\n')
newx <- load.generic.ship.data.and.metadata(opt$input,
                                            IMO.column=opt$IMO_column,
                                            metadata.filepath=opt$metadata,
                                            raw.metadata=opt$raw_metadata,
                                            remove.outliers=FALSE,
                                            verbose=opt$verbose)

if(opt$verbose) cat('Running prediction...\n')

print(system.time(results <- predict.from.saved.model(newx,
                                                   final.model.container,
                                                   verbose=c(0,1)[as.numeric(opt$verbose) + 1]
                                                   )
))


cat('Writing predictions to',opt$output_file,'...\n')
# load original input table, add predicted values to end
raw.x <- read.csv(opt$input)
# clean IMO column to ensure matching with R
# automated column cleanup
opt$IMO_column <- gsub(' ','.',opt$IMO_column)
opt$IMO_column <- gsub('\\.+','.',opt$IMO_column)
opt$IMO_column <- gsub('^\\.','',opt$IMO_column)
opt$IMO_column <- gsub('\\.$','',opt$IMO_column)

# augment x with prediction columns for model and linear model,
# percent deviation, imputed columns, numimputed columns
add.columns.ix <- grep('_imputed',colnames(newx))
print(colnames(raw.x))
for(add.column.ix in add.columns.ix){
  raw.x <- cbind(raw.x,rep(FALSE,nrow(raw.x)))
  colnames(raw.x)[ncol(raw.x)] <- colnames(newx)[add.column.ix]
  # add all TRUE/FALSE imputated value indicators to output table
  raw.x[match(newx$IMO.Number,raw.x[,opt$IMO_column]),ncol(raw.x)] <- newx[,add.column.ix]
  
  # Any rows not included in newx should be set to NA
  # because these were not in the metadata file
  raw.x[-match(newx$IMO.Number,raw.x[,opt$IMO_column]),ncol(raw.x)] <- NA
}
# add the number of imputed fields to output table
raw.x <- cbind(raw.x,'NumImputedFields'=rep(0,nrow(raw.x)))
raw.x[match(newx$IMO.Number,raw.x[,opt$IMO_column]),'NumImputedFields'] <- newx[,'NumImputedFields']
# Any rows not included in newx should be set to NA
# because these were not in the metadata file
raw.x[-match(newx$IMO.Number,raw.x[,opt$IMO_column]),'NumImputedFields'] <- NA

raw.x <- cbind(raw.x,
               predicted.kg.CO2.per.nm=rep(0,nrow(raw.x)),
               predicted.kg.CO2.per.nm.linear=rep(0,nrow(raw.x)),
               model.vs.linear.pct.deviation=rep(0,nrow(raw.x)))

# fill in predictions or NA if ship was missing from metadata
raw.x[match(newx$IMO.Number,raw.x[,opt$IMO_column]),'predicted.kg.CO2.per.nm'] <- round(results$yhat,5)
raw.x[!(raw.x[,opt$IMO_column] %in% newx$IMO.Number),'predicted.kg.CO2.per.nm'] <- NA
raw.x[match(newx$IMO.Number,raw.x[,opt$IMO_column]),'predicted.kg.CO2.per.nm.linear'] <- round(results$yhat.linear,5)
raw.x[!(raw.x[,opt$IMO_column] %in% newx$IMO.Number),'predicted.kg.CO2.per.nm'] <- NA
raw.x[match(newx$IMO.Number,raw.x[,opt$IMO_column]),'model.vs.linear.pct.deviation'] <- round((results$yhat - results$yhat.linear) / results$yhat.linear,5)
raw.x[!(raw.x[,opt$IMO_column] %in% newx$IMO.Number),'model.vs.linear.pct.deviation'] <- NA
write.csv(raw.x,opt$output_file, row.names=FALSE, quote=T)

