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
  make_option(c("--skip_eval"), action="store_true", default=FALSE,
              help="Use hard-coded hyperparams and skip model tuning and eval entirely, only train final model. Implies skip_tuning. [default %default]"),
  make_option(c("--skip_final_model"), action="store_true", default=FALSE,
              help="Skip training final model. Otherwise, final model will be saved to <outdir>/final_model.rdata. Required when evaluating multiple models. [default %default]"),
  make_option(c("--imputation_method"), default='rf',
              help="Imputation method: quick (median/mode) or rf (much more accurate but very slow) [default \"%default\"]"),
  make_option(c("--save_preprocessed_data"), action="store_true", default=FALSE,
              help="Save preprocessed data file. This can be useful when you want to want to rerun training but don't want to rerun imputation of missing values. File will be saved to <outdir>/preprocessed.csv [default \"%default\"]"),
  make_option(c("--load_preprocessed_data"), action="store_true", default=FALSE,
              help="Load preprocessed data file. This means that the input data file needs no preprocessing or imputation and is ready for model training. [default \"%default\"]"),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
              help="Verbose output [default %default]")
)
opt <- parse_args(OptionParser(option_list=option_list))

# check that all required options are provided
required.opts <- c('input','metadata','outdir')
if(any(!(required.opts %in% names(opt)))){
  stop(sprintf('One or more of these required parameters is missing: %s.\n\n', paste(required.opts,collapse=', ')))
}


if(any(!(required.opts %in% names(opt)))){
  stop(sprintf('One or more of these required parameters is missing: %s.\n\n', paste(required.opts,collapse=', ')))
}

# check models list for validity
models <- strsplit(opt$models,',')[[1]]
for(model in models){
  valid.models <- c('rf','ridge','linear','xgb')
  valid.model.string <- paste(valid.models,collapse=',')
  if(!(model %in% valid.models)){
    stop(sprintf('Error: requested model \"%s\" not found in valid models: %s \n', valid.model.string))
  }
}

# check for conflict between --models and --skip_eval
if(opt$skip_eval && length(models) > 1){
  cat('\nStop: Cannot skip eval when evaluating multiple models.\n')  
}

# check for conflict between --models and --skip_eval
if(opt$skip_eval && opt$skip_final_model){
  cat('\nStop: Cannot skip eval and skip final model.\n')  
}

# check for conflict between --models and --skip_final_model
if(!opt$skip_final_model && length(models) > 1){
  cat('\nWarning: Cannot produce final model when evaluating multiple models. Setting "skip_final_model" to TRUE.\n')  
  opt$skip_final_model <- TRUE
}

# for convenience, store final_model as opposite of skip_final_model
opt$final_model <- !opt$skip_final_model

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

if(opt$skip_tuning || opt$skip_eval){
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
                                skip.eval = opt$skip_eval,
                                final.model = opt$final_model
)

# save eval results
if(!opt$skip_eval){
  if(opt$verbose) cat('Saving eval results...\n')
  # create outdir in case does not exist
  dir.create(opt$outdir,showWarnings = FALSE)
  # write results to .csv file
  write.csv(res$rmses, paste(opt$outdir,'/rmse.csv',sep=''), row.names = TRUE, quote=F)
  write.csv(res$maes, paste(opt$outdir,'/mae.csv',sep=''), row.names = TRUE, quote=F)
  
  # write summary of execution parameters and performance
  sink(paste(opt$outdir,'/summary.txt',sep=''))
  cat('Overall performance\n')
  cat('Model     MAE (mean +/- sd)   NRMSE (mean +/- sd)\n')
  for(i in 1:nrow(res$mae)){
    cat(sprintf('%-10s',rownames(res$mae)[i]))
    cat(sprintf('%0.4f +/- %0.4f  ',mean(res$mae[i,]), sd(res$mae[i,])))
    cat(sprintf('%0.4f +/- %0.4f',mean(res$rmse[i,]), sd(res$rmse[i,])))
    cat('\n')
  }
  cat('\n')
  cat('Best model hyperparameters\n')
  # print hyperparams in a table for each model
  for(model.name in names(res$best.params)){
    if(length(res$best.params[[model.name]]) > 0){
      cat(model.name,'\n',sep='')
      param.mat <- sapply(res$best.params[[model.name]], function(xx) unlist(xx))
      colnames(param.mat) <- sprintf('rep%03d',1:ncol(param.mat))
      print(param.mat)
      cat('\n')
    }
  }
  cat('\n')
  cat('Command parameters\n')
  print(opt)
  sink(NULL)
}

# save final model
if(opt$final_model){
  if(opt$verbose > 0) cat('Saving final model...\n')
  final.model.filepath <- paste(opt$outdir,'/final_model.rdata',sep='')
  final.model.container <- list(model.type=models[1],
                                train.x=x[,predictor.names],
                                final.model=res$final.model)
  save(final.model.container, file=final.model.filepath)
}

