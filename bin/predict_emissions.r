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
              help="Input training data CSV. Must have a column containing IMO Numbers. Currently assumes this is EU reporting data with additional columns Ship.type, distance.traveled.nm, average.speed.nm.hr, Annual.Total.time.spent.at.sea.hours, and Annual.average.CO.emissions.per.distance.kg.CO.n.mile [required]"),
  make_option(c("-I", "--IMO_columm"), default='IMO.Number',
              help="Name of column containing IMO Numbers [default \"%default\"]"),
  make_option(c("-m", "--metadata"), 
              help = "Ship metadata file, e.g. \"IHS complete Ship Data.csv,\" with columns IMO Numbers in column 1. [required]"),
  make_option(c("-M", "--model_file"), 
              help = "Predictive model in .rdata format, output of train_and_evaluate_models.r. [required]"),
  make_option(c("-o", "--output_file"),
              help = "Output csv file [required]"),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
              help="Verbose output [default %default]")
)
opt <- parse_args(OptionParser(option_list=option_list))


# Now load other code; waited till here to perform
# input validations first
source(paste(CTHOME,'/lib/training-general.r',sep=''))
source(paste(CTHOME,'/lib/load.r',sep=''))

# Create output directory
dir.create(dirname(opt$output_file),showWarnings = FALSE)

# load data (can comment this out if loaded to save time)
if(opt$verbose) cat('Loading ship data and metadata...\n')
x <- load.EU.MRV.ship.data.and.metadata(ship.filepath=opt$input,
                                 metadata.filepath=opt$metadata,
                                 outdir='.',
                                 imputation.method='quick')

drop.names <- ''
# Drop non-static features (those generated annually)
drop.names <- c(drop.names,c('distance.traveled','time.at.sea','average.speed'))

# Drop unhelpful predictors and the outcome variable, if present
predictor.names <- setdiff(colnames(x),c('IMO.Number','flagname','reporting.period','shiptype3','shiptype4','kg.CO2.per.nm',drop.names))


if(opt$verbose) cat('Loading model from',opt$model_file,'\n')
load(opt$model_file)

if(opt$verbose) cat('Running prediction...\n')

print(system.time(yhat <- predict.from.saved.model(x[,predictor.names],
                                                   final.model.container$final.model,
                                                   final.model.container$model.type,
                                                   verbose=c(0,1)[as.numeric(opt$verbose) + 1]
                                                   )
))

cat('Writing predictions to',opt$output_file,'...\n')
newx <- cbind(x,predicted.kg.CO2.per.nm=yhat)
write.csv(newx,opt$output_file, row.names=FALSE)
