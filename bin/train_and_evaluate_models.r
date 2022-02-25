#!/usr/bin/env Rscript
# Tune, evaluate, and train one or more machine learning models
# to predict carbon emissions (kg) per nautical mile. Output
# includes performance metrics and the final predictive model(s).
#
# usage:
#
# Default is to tune, evaluate, and train random forests, extreme gradient boosting,
# groupwise linear model by ship type, and ridge regression,
# using 10-fold cross-validation (or out-of-bag prediction for random forests)
# to estimate generalization error.
# Default uses 10 repeats of the entire training process.
# 
# Rscript train_and_evaluate_models.r -i training_data.csv -m ship_metadata.csv -o output_directory
#
# Other examples:
#
# Tune, eval, and train only RF and linear using 3 repeats
# Rscript train_and_evaluate_models.r -i training_data.csv -m ship_metadata.csv -o output_directory --models "rf,linear" --repeats 3
#
# No evaluation/tuning, just train a final model on whole data set
# using default params
# Rscript train_and_evaluate_models.r -i training_data.csv -m ship_metadata.csv -o output_directory --models rf --skip-tuning
library('optparse', warn.conflicts = F, quietly = T)

if(Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME') == ''){
  stop('R_CLIMATE_TRACE_SHIPPING_HOME not in environment variables. Please add "R_CLIMATE_TRACE_SHIPPING_HOME=/path/to/r/climate/trace/shipping/repo" to the file .Renviron in your home directory. Read more about the .Renviron file here: https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf')
}
CTHOME=Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME')

set.seed(123456)

# Define and get commandline options
option_list <- list(
  make_option(c("-i", "--input"),
              help="Input training data CSV. Must have a column containing IMO Numbers. Currently assumes this is EU reporting data with additional columns Ship.type, distance.traveled.nm, average.speed.nm.hr, Annual.Total.time.spent.at.sea.hours, and Annual.average.CO.emissions.per.distance.kg.CO.n.mile [required]"),
  make_option(c("-I", "--IMO_columm"), default='IMO.Number',
              help="Name of column containing IMO Numbers [default \"%default\"]"),
  make_option(c("-m", "--metadata"), 
              help = "Ship metadata file, e.g. \"IHS complete Ship Data.csv,\" with columns IMO Numbers in column 1. [required]"),
  make_option(c("-o", "--outdir"),
              help = "Output directory [required]"),
  make_option(c("-M", "--models"), default='rf', 
              help="Comma-separated list of models. Options are rf, ridge, linear, xgb. [default %default]"),
  make_option(c("r", "--repeats"), default=1,
              help="Number of repeats of evaluation procedure [default %default]"),
  make_option(c("--skip_tuning"), action="store_true", default=FALSE,
              help="Skip model tuning and use hard-coded params, only estimate error (and optionally train final model) [default %default]"),
  make_option(c("--final_model"), action="store_true", default=FALSE,
              help="Train final model. Will be saved to <outdir>/final_model.rdata. Only works with one model at a time. [default %default]"),
  make_option(c("--imputation_method"), default='quick',
              help="Imputation method: quick (median/mode) or rf (much more accurate but very slow) [default \"%default\"]"),
  make_option(c("--save_preprocessed_data"), action="store_true", default=FALSE,
              help="Save preprocessed data file. This can be useful when you want to want to rerun training but don't want to rerun imputation of missing values. File will be saved to <outdir>/preprocessed.csv [default \"%default\"]"),
  make_option(c("--load_preprocessed_data"), action="store_true", default=FALSE,
              help="Load preprocessed data file. This means that the input data file needs no preprocessing or imputation and is ready for model training. [default \"%default\"]"),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
              help="Verbose output [default %default]")
)
opt <- parse_args(OptionParser(option_list=option_list))

# check models list for validity
models <- strsplit(opt$models,',')[[1]]
for(model in models){
  valid.models <- c('rf','ridge','linear','xgb')
  valid.model.string <- paste(valid.models,collapse=',')
  if(!(model %in% valid.models)){
    stop(sprintf('Error: requested model \"%s\" not found in valid models: %s \n', valid.model.string))
  }
}

# ensure that only one model is requested
# if final model also requested
if(opt$final_model && length(models) > 1){
  stop('Error: final model can only be output for one model at a time ')
}

# Now load other code; waited till here to perform
# input validations first
source(paste(CTHOME,'/lib/training-general.r',sep=''))
source(paste(CTHOME,'/lib/load.r',sep=''))


# Create output directory
dir.create(opt$outdir,showWarnings = FALSE)

# load data (can comment this out if loaded to save time)
if(opt$verbose) cat('Loading ship data and metadata...\n')
if(opt$load_preprocessed_data){
  x <- read.csv(opt$input)
} else {
  x <- load.EU.MRV.ship.data.and.metadata(ship.filepath=opt$input,
                                 metadata.filepath=opt$metadata,
                                 outdir=opt$outdir,
                                 imputation.method=opt$imputation_method,
                                 verbose=opt$verbose)
  if(opt$save_preprocessed_data){
    write.csv(x,paste(opt$outdir,'/preprocessed.csv',sep=''),quote=TRUE,row.names=FALSE)
  }
}
predictor.names <- c("shiptype.original","deadweight","grosstonnage","length","breadth","draught","shiptype2","powerkwmax","powerkwaux","speedmax","yearbuilt","flagname.binned","flagname.continent")

if(opt$skip_tuning){
  params <- list(rf=list(mtry=15, nodesize=8,ntree=2000),
              xgb=list(nrounds=2000, eta=.01, subsample=0.75, max_depth=5))
} else {
  params <- NULL
}

if(opt$verbose) cat('Running tuning and evaluation...\n')
res <- tune.and.evaluate.models(x[,predictor.names],
                                  x$kg.CO2.per.nm,
                                  nreps=opt$repeats,
                                  models=models,
                                  params=params,
                                  individual.ids=x$IMO.Number,
                                  linear.predictor=x$powerkwmax,
                                  linear.categories=x$shiptype.original,
                                  verbose=c(0,1)[as.numeric(opt$verbose) + 1],
                                  final.model = opt$final_model
  )

# save eval results
if(opt$verbose) cat('Saving eval results...\n')
# create outdir in case does not exist
dir.create(opt$outdir,showWarnings = FALSE)
# write results to .csv file
write.csv(res$rmses, paste(opt$outdir,'/rmse.csv',sep=''), row.names = TRUE, quote=F)
write.csv(res$maes, paste(opt$outdir,'/mae.csv',sep=''), row.names = TRUE, quote=F)

# save final model
if(opt$final_model){
  if(opt$verbose > 0) cat('Saving final model...\n')
  final.model.filepath <- paste(opt$outdir,'/final_model.rdata',sep='')
  final.model.container <- list(model.type=models[1],
                                train.x=x[,predictor.names],
                                final.model=res$final.model)
  save(final.model.container, file=final.model.filepath)
}

