library('mice', warn.conflicts = F, quietly = T)
# get path to source files from R environment
if(Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME') == ''){
  stop('R_CLIMATE_TRACE_SHIPPING_HOME not in environment variables. Please add "R_CLIMATE_TRACE_SHIPPING_HOME=/path/to/r/climate/trace/shipping/repo" to the file .Renviron in your home directory. Read more about the .Renviron file here: https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf')
}

CTHOME=Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME')

source(paste(CTHOME,'/lib/lib.r',sep=''))


# SET GLOBAL VARIABLE LOOKUP TABLE: flagname mapping to continent
{
FLAGNAMES.CONTINENT.MAP <- list()
FLAGNAMES.CONTINENT.MAP[["Albania"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Algeria"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Angola"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Anguilla"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Antigua & Barbuda"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Argentina"]] <- 'South America'
FLAGNAMES.CONTINENT.MAP[["Australia"]] <- 'Oceana'
FLAGNAMES.CONTINENT.MAP[["Azerbaijan"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Bahamas"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Bahrain"]] <- 'Middle East'
FLAGNAMES.CONTINENT.MAP[["Bangladesh"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Barbados"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Belgium"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Belize"]] <- 'Central America'
FLAGNAMES.CONTINENT.MAP[["Benin"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Bermuda"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Bolivia"]] <- 'Central America'
FLAGNAMES.CONTINENT.MAP[["Brazil"]] <- 'South America'
FLAGNAMES.CONTINENT.MAP[["Brunei"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Bulgaria"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Cameroon"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Canada"]] <- 'North America'
FLAGNAMES.CONTINENT.MAP[["Cape Verde"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Cayman Islands"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Chile"]] <- 'South America'
FLAGNAMES.CONTINENT.MAP[["China, People's Republic Of"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Chinese Taipei"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Colombia"]] <- 'South America'
FLAGNAMES.CONTINENT.MAP[["Comoros"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Congo (Democratic Republic)"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Congo"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Cook Islands"]] <- 'Oceana'
FLAGNAMES.CONTINENT.MAP[["Costa Rica"]] <- 'Central America'
FLAGNAMES.CONTINENT.MAP[["Cote D'ivoire"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Croatia"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Cuba"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Curacao"]] <- 'South America'
FLAGNAMES.CONTINENT.MAP[["Cyprus"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Denmark"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Djibouti"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Dominica"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Dominican Republic"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Ecuador"]] <- 'South America'
FLAGNAMES.CONTINENT.MAP[["Egypt"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["El Salvador"]] <- 'Central America'
FLAGNAMES.CONTINENT.MAP[["Equatorial Guinea"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Eritrea"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Estonia"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Ethiopia"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Faeroe Islands"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Falkland Islands"]] <- "South America"
FLAGNAMES.CONTINENT.MAP[["Fiji"]] <- "Oceana"
FLAGNAMES.CONTINENT.MAP[["Finland"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Flag Not Required"]] <- "Unknown"
FLAGNAMES.CONTINENT.MAP[["France"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["French Southern Territories"]] <- ""
FLAGNAMES.CONTINENT.MAP[["Gabon"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Gambia"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Georgia"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Germany"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Ghana"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Gibraltar"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Greece"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Grenada"]] <- "Carribean"
FLAGNAMES.CONTINENT.MAP[["Guatemala"]] <- "Central America"
FLAGNAMES.CONTINENT.MAP[["Guernsey"]] <- "Europe"
FLAGNAMES.CONTINENT.MAP[["Guinea"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Guinea-Bissau"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Guyana"]] <- "South America"
FLAGNAMES.CONTINENT.MAP[["Haiti"]] <- "Carribean"
FLAGNAMES.CONTINENT.MAP[["Honduras"]] <- 'Central America'
FLAGNAMES.CONTINENT.MAP[["Hong Kong, China"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Iceland"]] <- "Europe"
FLAGNAMES.CONTINENT.MAP[["India"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Indonesia"]] <- 'Oceana'
FLAGNAMES.CONTINENT.MAP[["Iran"]] <- 'Middle East'
FLAGNAMES.CONTINENT.MAP[["Iraq"]] <- "Middle East"
FLAGNAMES.CONTINENT.MAP[["Irish Republic"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Isle Of Man"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Israel"]] <- 'Middle East'
FLAGNAMES.CONTINENT.MAP[["Italy"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Jamaica"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Japan"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Jersey"]] <- "Europe"
FLAGNAMES.CONTINENT.MAP[["Jordan"]] <- 'Middle East'
FLAGNAMES.CONTINENT.MAP[["Kazakhstan"]] <- "Asia"
FLAGNAMES.CONTINENT.MAP[["Kenya"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Kiribati"]] <- "Oceana"
FLAGNAMES.CONTINENT.MAP[["Korea, North"]] <- "Asia"
FLAGNAMES.CONTINENT.MAP[["Korea, South"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Kuwait"]] <- 'Middle East'
FLAGNAMES.CONTINENT.MAP[["Laos"]] <- "Asia"
FLAGNAMES.CONTINENT.MAP[["Latvia"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Lebanon"]] <- 'Middle East'
FLAGNAMES.CONTINENT.MAP[["Liberia"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Libya"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Lithuania"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Luxembourg"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Madagascar"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Malaysia"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Maldives"]] <- "Asia"
FLAGNAMES.CONTINENT.MAP[["Malta"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Marshall Islands"]] <- 'Oceana'
FLAGNAMES.CONTINENT.MAP[["Mauritania"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Mauritius"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Mexico"]] <- 'Central America'
FLAGNAMES.CONTINENT.MAP[["Micronesia"]] <- "Oceana"
FLAGNAMES.CONTINENT.MAP[["Moldova"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Mongolia"]] <- "Asia"
FLAGNAMES.CONTINENT.MAP[["Montenegro"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Morocco"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Mozambique"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Myanmar"]] <- "Asia"
FLAGNAMES.CONTINENT.MAP[["Namibia"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Nauru"]] <- "Oceana"
FLAGNAMES.CONTINENT.MAP[["Netherlands"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["New Zealand"]] <- "Oceana"
FLAGNAMES.CONTINENT.MAP[["Nicaragua"]] <- "Central America"
FLAGNAMES.CONTINENT.MAP[["Nigeria"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Niue"]] <- 'Oceana'
FLAGNAMES.CONTINENT.MAP[["Norway"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Oman"]] <- "Middle East"
FLAGNAMES.CONTINENT.MAP[["Pakistan"]] <- 'Middle East'
FLAGNAMES.CONTINENT.MAP[["Palau"]] <- 'Oceana'
FLAGNAMES.CONTINENT.MAP[["Panama"]] <- 'Central America'
FLAGNAMES.CONTINENT.MAP[["Papua New Guinea"]] <- "Oceana"
FLAGNAMES.CONTINENT.MAP[["Paraguay"]] <- "South America"
FLAGNAMES.CONTINENT.MAP[["Peru"]] <- "South America"
FLAGNAMES.CONTINENT.MAP[["Philippines"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Poland"]] <- "Europe"
FLAGNAMES.CONTINENT.MAP[["Portugal"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Qatar"]] <- 'Middle East'
FLAGNAMES.CONTINENT.MAP[["Romania"]] <- "Europe"
FLAGNAMES.CONTINENT.MAP[["Russia"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Samoa"]] <- "Oceana"
FLAGNAMES.CONTINENT.MAP[["Sao Tome & Principe"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Saudi Arabia"]] <- 'Middle East'
FLAGNAMES.CONTINENT.MAP[["Senegal"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Seychelles"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Sierra Leone"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Singapore"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Sint Maarten"]] <- "Carribean"
FLAGNAMES.CONTINENT.MAP[["Slovenia"]] <- "Europe"
FLAGNAMES.CONTINENT.MAP[["Solomon Islands"]] <- "Oceana"
FLAGNAMES.CONTINENT.MAP[["South Africa"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Spain"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Sri Lanka"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["St Helena"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["St Kitts & Nevis"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["St Vincent & The Grenadines"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Sudan"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Suriname"]] <- "South America"
FLAGNAMES.CONTINENT.MAP[["Sweden"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Switzerland"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Syria"]] <- "Middle East"
FLAGNAMES.CONTINENT.MAP[["Tanzania (Sumatra)"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Tanzania (Zanzibar)"]] <- "Africa"
FLAGNAMES.CONTINENT.MAP[["Tanzania"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Thailand"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Togo"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Tonga"]] <- "Oceana"
FLAGNAMES.CONTINENT.MAP[["Trinidad & Tobago"]] <- "Carribean"
FLAGNAMES.CONTINENT.MAP[["Tunisia"]] <- 'Africa'
FLAGNAMES.CONTINENT.MAP[["Turkey"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["Turkmenistan"]] <- "Europe"
FLAGNAMES.CONTINENT.MAP[["Tuvalu"]] <- 'Oceana'
FLAGNAMES.CONTINENT.MAP[["Ukraine"]] <- "Europe"
FLAGNAMES.CONTINENT.MAP[["United Arab Emirates"]] <- "Middle East"
FLAGNAMES.CONTINENT.MAP[["United Kingdom"]] <- 'Europe'
FLAGNAMES.CONTINENT.MAP[["United States Of America"]] <- 'North America'
FLAGNAMES.CONTINENT.MAP[["Unknown"]] <- 'Unknown'
FLAGNAMES.CONTINENT.MAP[["Uruguay"]] <- "South America"
FLAGNAMES.CONTINENT.MAP[["Vanuatu"]] <- 'Oceana'
FLAGNAMES.CONTINENT.MAP[["Venezuela"]] <- "South America"
FLAGNAMES.CONTINENT.MAP[["Vietnam"]] <- 'Asia'
FLAGNAMES.CONTINENT.MAP[["Virgin Islands, British"]] <- 'Carribean'
FLAGNAMES.CONTINENT.MAP[["Yemen"]] <- "Middle East"
# convert map to a named vector
tmp <- character(length(FLAGNAMES.CONTINENT.MAP))
names(tmp) <- names(FLAGNAMES.CONTINENT.MAP)
for(i in 1:length(FLAGNAMES.CONTINENT.MAP)){
  tmp[i] <- FLAGNAMES.CONTINENT.MAP[[i]]
}
FLAGNAMES.CONTINENT.MAP <- tmp
}

"load.generic.ship.data.and.metadata" <- function(ship.filepath,
                                                  metadata.filepath=paste(CTHOME,'/data/IHS-imputed-rf.csv',sep=''),
                                                  IMO.column='IMO.Number',
                                                  raw.metadata=FALSE, # TRUE if metadata file has not been preprocessed
                                                  keep.extra.columns=FALSE,
                                                  remove.outliers=TRUE, # disable for predictions on new data
                                                  verbose=TRUE
){
  x <- read.csv(ship.filepath)
  
  # replace spaces with "." in the provided IMO.column string,
  # to match the auto-replacement in read.csv
  # also remove double .. and leading/trailing .
  raw.IMO.column <- IMO.column
  IMO.column <- gsub(' ','.',IMO.column)
  IMO.column <- gsub('\\.+','.',IMO.column)
  IMO.column <- gsub('^\\.','',IMO.column)
  IMO.column <- gsub('\\.$','',IMO.column)
  
  if(!(IMO.column %in% colnames(x))){
    if(IMO.column != raw.IMO.column) {
      replacement.string <- sprintf('("%s" after replacing spaces/non-alphanumeric with ".") ',IMO.column)
    } else {
      replacement.string <- ''
    }
    stop(sprintf('\nProvided IMO column header "%s" %snot found in input file headers: %s.\n\n',
                 raw.IMO.column,
                 replacement.string,
                 paste(colnames(x),collapse=', ')))
  }
  
  # rename the provided IMO number column to 'IMO.Number'
  colnames(x)[colnames(x) == IMO.column] <- 'IMO.Number'

  # keep only IMO Number column
  # keep full x in case adding extra columns back in later
  full.x <- x
  x <- full.x[,'IMO.Number',drop=F]
  
  # Either load raw metadata and preprocess it, 
  # or load the pre-preprocessed metadata from file
  if(raw.metadata){
    map <- preprocess.ship.metadata(metadata.filepath)
  } else {
    map <- load.preprocessed.ship.metadata(metadata.filepath)
  }

  x <- add.ship.metadata(x, map)
  
  # add back in other columns, overwriting the metadata if same column name is present
  if(keep.extra.columns){
    for(column.name in colnames(full.x)[colnames(full.x) != "IMO.Number"]){
      print(column.name)
      if(column.name %in% colnames(x)){
        # This column was already added to x from metadata; overwrite but print warning
        cat('WARNING: metadata column',column.name,'Present in input file and metadata file. Overwriting metadata values with values in input file.\n')
        x[,column.name] <- full.x[match(x$IMO.Number,full.x$IMO.Number),column.name]
      } else {
        x <- cbind(x,full.x[match(x$IMO.Number,full.x$IMO.Number),column.name])
        colnames(x)[ncol(x)] <- column.name
      }
    }
  }
  return(x)
}

"load.EU.MRV.ship.data.and.metadata" <- function(ship.filepath=paste(CTHOME,'/data/EU MRV data 18-19-20.csv',sep=''),
                                                 metadata.filepath=paste(CTHOME,'/data/IHS-imputed-rf.csv',sep=''),
                                                 raw.metadata=FALSE,
                                                 outdir='output', # outdir for preprocessing visualizations
                                                 do.plots=FALSE, # plot feature distributions, outliers, interactions
                                                 preprocess=TRUE,
                                                 remove.outliers=TRUE, # disable for predictions on new data
                                                 verbose=TRUE
){
  x <- load.EU.MRV.ship.data(ship.filepath)

  # Either load raw metadata and preprocess it, 
  # or load the pre-preprocessed metadata from file
  if(raw.metadata){
    map <- preprocess.ship.metadata(metadata.filepath)
  } else {
    map <- load.preprocessed.ship.metadata(metadata.filepath)
  }
  
  x <- add.ship.metadata(x, map)
  
  if(preprocess) x <- preprocess.EU.ship.data(x,
                                           outdir = outdir,
                                           do.plots = do.plots,
                                           remove.outliers=remove.outliers,
                                           verbose=verbose)
  return(x)
}

"load.raw.ship.metadata" <- function(filepath=paste(CTHOME,'/data/IHS complete Ship Data.csv',sep=''),
                                 check_valid=TRUE
){
  m <- read.csv(filepath,row.names=1)
  if(check_valid){
    expected.columns <- c('Deadweight','FlagName','GrossTonnage','LengthOverallLOA','LengthRegistered','Breadth','Draught','ShiptypeLevel2','ShiptypeLevel3','ShiptypeLevel4','Powerkwmax','TotalPowerOfAuxiliaryEngines','Speedmax', 'Speed','YearOfBuild')
    if(any(!(expected.columns %in% colnames(m)))){
      missing.columns.string <- paste(setdiff(expected.columns, colnames(m)),collapse=', ',sep='')
      stop(sprintf('Error: the following required columns were not found in the ship metadata table: %s',missing.columns.string))
    }
  }
  return(m)
}

"load.preprocessed.ship.metadata" <- function(filepath=paste(CTHOME,'/data/IHS-imputed-rf.csv',sep=''),
                                     check_valid=TRUE
){
  m <- read.csv(filepath,row.names=1)
  if(check_valid){
    expected.columns <- c('Deadweight','FlagNameBin','FlagNameContinent','GrossTonnage','Length','Breadth','Draught','ShiptypeEU','ShiptypeLevel2','Powerkwmax','Powerkwaux','Speed','YearOfBuild')
    if(any(!(expected.columns %in% colnames(m)))){
      missing.columns.string <- paste(setdiff(expected.columns, colnames(m)),collapse=', ',sep='')
      stop(sprintf('Error: the following required columns were not found in the ship metadata table: %s',missing.columns.string))
    }
  }
  return(m)
}


# specifically loads EU MRV ship data file
"load.EU.MRV.ship.data" <- function(filepath=paste(CTHOME,'/data/EU MRV data 18-19-20.csv',sep=''),
                                    verbose=1){
  # read CSV
  x <- read.csv(filepath)
  if(verbose > 0) cat('Raw data:',nrow(x),'unique entries\n')
  
  # to deal with special characters, replace all non-alphanumerics
  # with ".", then replace multiple "." with single "."
  # Also drop "." from start or end for aesthetics
  colnames(x) <- gsub('[^A-Za-z0-9]','.',colnames(x))
  colnames(x) <- gsub('\\.+','.',colnames(x))
  colnames(x) <- gsub('^\\.','',colnames(x))
  colnames(x) <- gsub('\\.$','',colnames(x))
  
  columns.to.keep <- c("IMO.Number",
                       "Reporting.Period",
                       "Ship.type",
                       "distance.traveled.nm",
                       "average.speed.nm.hr",
                       "Annual.Total.time.spent.at.sea.hours",
                       "Annual.average.CO.emissions.per.distance.kg.CO.n.mile")
  
  # ensure all desired columns are present
  if(any(!(columns.to.keep %in% colnames(x)))){
    missing.columns <- paste(setdiff(columns.to.keep,colnames(x)),collapse=',')
    stop(sprintf('Error: columns %s missing from ship data file.', missing.columns))
  }
  
  # keep a few original columns, but rename them
  x <- x[,columns.to.keep]
  colnames(x) <- c("IMO.Number","reporting.period","shiptype.original","distance.traveled","average.speed","time.at.sea","kg.CO2.per.nm")
  
  return(x)
}


# takes a data frame with a column "IMO.Number"
# and looks up metadata from metadata table.
# The metadata table "map" must me the output of "preprocess.ship.data"
# or "load.preprocessed.ship.data". (Can't be raw ship data)
#
# Ships entirely missing from metadata table are dropped
#
# x must have a column named "IMO.Number"
#
"add.ship.metadata" <- function(x, map,
                                predictor.names=c('Deadweight','FlagNameBin','FlagNameContinent','GrossTonnage','Length','Breadth','Draught','ShiptypeEU','ShiptypeLevel2','Powerkwmax','Powerkwaux', 'Speed','YearOfBuild'),
                                add.imputation.indicators=TRUE,
                                verbose=1){
  # Drop ships missing from metadata file
  # convert IMO number to string so that it can be used for indexing
  x$IMO.Number <- as.character(x$IMO.Number)
  drop.ix <- which(!(x$IMO.Number %in% rownames(map)))
  if(length(drop.ix) > 0){
    x <- x[-drop.ix,,drop=F]
    cat('Warning: dropped',length(drop.ix),'entries for ships not present in metadata file.\n')
  }
  x <- cbind(x,map[x$IMO.Number,predictor.names])
  if(add.imputation.indicators){
    add.columns <- grep('_imputed',colnames(map))
    x <- cbind(x,map[x$IMO.Number,add.columns])
    x <- cbind(x,NumImputedFields=map[x$IMO.Number,'NumImputedFields'])
  }
  return(x)
}

# drops outliers and ships with invalid or NA values,
# imputes data where requested
# input is a raw ship data frame with all metadata, 
# output is a preprocessed ship data frame
"preprocess.EU.ship.data" <- function(x,
                                   positive.vars
                                   = c('kg.CO2.per.nm', # list of known cont. vars
                                       'average.speed',
                                       'distance.traveled',
                                       'time.at.sea',
                                       'Deadweight',
                                       'GrossTonnage',
                                       'Length',
                                       'Breadth',
                                       'Draught',
                                       'Powerkwmax',
                                       'Powerkwaux',
                                       'Speed',
                                       'YearOfBuild'),
                                   remove.outliers = TRUE, # disable for predictions on new data
                                   use.log.for.outlier.detection 
                                   = c('kg.CO2.per.nm',
                                       'Deadweight',
                                       'GrossTonnage',
                                       'Breadth',
                                       'Powerkwmax',
                                       'Powerkwaux',
                                       'Speed',
                                       'distance.traveled',
                                       'average.speed'),
                                   ignore.outlier.detection.columns
                                   = c('YearOfBuild'),
                                   min.time.at.sea = 7*24, # min in hours (default 1 week)
                                   min.distance.traveled = 1000, # default 1k km
                                   outlier.IQR.threshold = 3, # IQR multiplier m for boxplot outlier test
                                   outdir = "output", # outdir for feature distribution plots
                                   do.plots = TRUE,
                                   verbose=0
)
{
  # drop NA, outliers, etc. in numeric vars
  n.before <- nrow(x)
  
  # Copy EU ship type into ShiptypeEU field
  x$ShiptypeEU <- x$shiptype.original

  # ensure eu-specific vars are numeric
  x[,"distance.traveled"] <- suppressWarnings(as.numeric(x[,"distance.traveled"]))
  x[,"average.speed"] <- suppressWarnings(as.numeric(x[,"average.speed"]))
  x[,"time.at.sea"] <- suppressWarnings(as.numeric(x[,"time.at.sea"]))
  x[,"kg.CO2.per.nm"] <- suppressWarnings(as.numeric(x[,"kg.CO2.per.nm"]))

  # Drop NA/known bad values
  for(i in 1:length(positive.vars)){
    positive.var <- positive.vars[i]
    if(positive.var %in% colnames(x)){
      # only test if this var is in columns of x
      # drop ships with NA value for this continuous var
      x <- droplevels(x[!is.na(x[,positive.var]),])
      # drop ships with zero values 
      x <- droplevels(x[x[,positive.var] > 0,])
    }
  }
  
  if("time.at.sea" %in% colnames(x)){
    x <- droplevels(x[x$time.at.sea <= 366*24,]) # time at sea can't be more hours than a year
    x <- droplevels(x[x$time.at.sea >= min.time.at.sea,]) # time at sea must be at least a week for reliable data
  } 
  if("distance.traveled" %in% colnames(x)){
    x <- droplevels(x[x$distance.traveled >= min.distance.traveled,]) # distance traveled must be at least 1000 nm
  }
  if(verbose > 0) cat('Dropped',n.before-nrow(x),'ships with N/A, non-positive, or inadmissible value for continuous variables (Time at sea < 1 week or > 1 year, Distance traveled < 1000km).\n')
  
  if(remove.outliers){
    # drop outliers 
    # use boxplot rule modified to m*IQR above/below 75/25%
    # the m instead of e.g. 1.5*IQR is from visual inspection of the log-transformed data
    # outliers for some variables detected in log space due to highly skewed data
    outlier.ix <- rep(FALSE,nrow(x))
    for(i in 1:length(positive.vars)){
      positive.var <- positive.vars[i]
      if(!(positive.var %in% ignore.outlier.detection.columns) &&
         positive.var %in% colnames(x)){
        if(positive.var %in% use.log.for.outlier.detection){
          outlier.ix <- outlier.ix | is.outlier(log10(x[,positive.var]),outlier.IQR.threshold)
        } else{
          outlier.ix <- outlier.ix | is.outlier(x[,positive.var],outlier.IQR.threshold)
        }
      }
    }
  }  
  if(do.plots){
    plot.feature.distributions(x, positive.vars, use.log=use.log.for.outlier.detection,
                               outlier.IQR.threshold = outlier.IQR.threshold,
                               outdir=outdir)
  }
  
  if(remove.outliers){
    # Now drop the outliers detected
    x <- droplevels(x[!outlier.ix,])
    cat('Dropped',sum(outlier.ix),sprintf('ships (%.3f%%)',100*sum(outlier.ix)/(nrow(x) + sum(outlier.ix))), 'with outlier values for continuous variables.\n')
  }  
  return(x)
}


# takes a raw ship metadata file and preprocesses it
# this means imputing missing data, 
# renaming some long column names, 
# and adding any derived columns/features (like flagname.continent)
# if outputfile given, writes new CSV to outputfile.
# 
# Ideally, do this once for ship metadata file and save to CSV.
# Then in future use the cleaned, filled table
preprocess.ship.metadata <- function(metadata.filepath=paste(CTHOME,'/data/IHS complete Ship Data.csv',sep=''),
                                     metadata.output.filepath=NULL,
                                     positive.vars # list of known positive vars, same list will be imputed if missing
                                     = c('Deadweight',
                                         'GrossTonnage',
                                         'Length',
                                         'Breadth',
                                         'Draught',
                                         'Powerkwmax',
                                         'Powerkwaux',
                                         'Speed'),
                                     predictor.names
                                     = c('Deadweight',
                                         'FlagNameBin',
                                         'FlagNameContinent',
                                         'GrossTonnage',
                                         'Length',
                                         'Breadth',
                                         'Draught',
                                         'ShiptypeEU',
                                         'ShiptypeLevel2',
                                         'Powerkwmax',
                                         'Powerkwaux',
                                         'Speed',
                                         'YearOfBuild'),
                                     imputation.method=c('kNN','rf','quick','none')[2],
                                     outlier.IQR.threshold = 3, # IQR multiplier m for boxplot outlier test
                                     verbose=0){
  
  # Create output directory if needed
  if(!is.null(metadata.output.filepath)) dir.create(dirname(metadata.output.filepath),showWarnings = FALSE)
  
  # Read input table
  if(verbose > 0) cat('Reading metadata table...\n')
  map <- read.csv(metadata.filepath,row.names=1)
  if(verbose > 0) cat('Read',nrow(map),'rows.\n')
  
  # rename very long column name
  colnames(map)[colnames(map) == 'TotalPowerOfAuxiliaryEngines'] <- 'Powerkwaux'  
  
  # Smart fill-ins of certain values based on expert knowledge
  # get Length as the max of two provided lengths since sometimes missing data
  map$Length <- apply(map[,c('LengthOverallLOA','LengthRegistered')],1,function(xx) max(xx,na.rm=TRUE))

  # if shiptype.eu is not present, assign closest matches
  # This was a mapping from EU reported data that may have been a 
  # useful predictor.
  if(!('ShiptypeEU' %in% colnames(map))){
    shiptype.eu <- character(nrow(map))
    shiptype.eu[map$ShiptypeLevel4 %in% c('Bulk Carrier','Cement Carrier','Ore Carrier','Pipe Burying Vessel','Self Discharging Bulk Carrier')] <- 'Bulk carrier'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Chemical Tanker','Chemical/Oil Products Tanker')] <- 'Chemical tanker'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Bulk/Oil Carrier')] <- 'Combination carrier'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Container Ship')] <- 'Container ship'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Container/Ro-Ro Cargo Ship')] <- 'Container/ro-ro cargo ship'
    shiptype.eu[map$ShiptypeLevel4 %in% c('LPG Tanker')] <- 'Gas carrier'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Aggregates Carrier','Deck Cargo Ship','General Cargo Ship','Palletised Cargo Ship')] <- 'General cargo ship'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Bunkering Tanker','FPSO (Floating, Production, Storage, Offloading)','LNG Tanker')] <- 'LNG carrier'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Bitumen Tanker','Crude Oil Tanker','Oil Products Tanker')] <- 'Oil tanker'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Passenger (Cruise) Ship')] <- 'Passenger ship'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Fruit Juice Tanker','Refrigerated Cargo Ship')] <- 'Refrigerated cargo carrier'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Passenger/General Cargo Ship','Passenger/Ro-Ro Cargo Ship')] <- 'Ro-pax ship'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Ro-Ro Cargo Ship')] <- 'Ro-ro ship'
    shiptype.eu[map$ShiptypeLevel4 %in% c('Vehicles Carrier')] <- 'Vehicle carrier'
    shiptype.eu[map$ShiptypeLevel4 == ""] <- "Other ship types" # assign all left over to other ship types
    shiptype.eu[shiptype.eu == ''] <- "Other ship types"
    map$ShiptypeEU <- shiptype.eu

    # Note: the following commented code was used to generate the best mapping,
    # based on 2018-2020 EU reporting data,
    # which was then hard-coded above.
    # 
    # # tally how many ships map from each EU shiptype (x) to each
    # # metadata shiptype level 4 (map)
    # shiptype.mat <- matrix(0,nrow=length(unique(x$shiptype.eu)),ncol=length(unique(x$shiptype4)))
    # rownames(shiptype.mat) <- sort(unique(x$shiptype.eu))
    # colnames(shiptype.mat) <- sort(unique(x$shiptype4))
    # for(shiptype.i in rownames(shiptype.mat)){
    #   for(shiptype.j in colnames(shiptype.mat)){
    #     shiptype.mat[shiptype.i,shiptype.j] <- sum(x$shiptype.eu==shiptype.i & x$shiptype4 == shiptype.j)
    #   }
    # }
    # 
    # # print the code to hard-code this mapping
    # mappings <- list()
    # for(shiptype.j in colnames(shiptype.mat)){
    #   shiptype.i <- rownames(shiptype.mat)[which.max(shiptype.mat[,shiptype.j])]
    #   if(!(shiptype.i %in% names(mappings))) mappings[[shiptype.i]] <- character(0)
    #   mappings[[shiptype.i]] <- c(mappings[[shiptype.i]],shiptype.j)
    # }
    # for(shiptype.i in sort(names(mappings))){
    #   cat('shiptype.eu[x$shiptype4 %in% c(')
    #   cat(paste("'",paste(mappings[[shiptype.i]],collapse="','"),"'",sep=''))
    #   cat(paste(')] <- ',"'",shiptype.i,"'\n",sep=''))
    # }
  }
  
  # convert numeric-intended columns to numeric
  map$Powerkwmax <- as.numeric(map$Powerkwmax)
  map$Powerkwaux <- as.numeric(map$Powerkwaux)
  
  # deduplicate and bin flag names
  # Fix duplicate flagnames
  if(any(map$FlagName == 'Portugal (Mar)')) map$FlagName[map$FlagName == 'Portugal (Mar)'] <- 'Portugal'
  if(any(map$FlagName == 'Denmark (Dis)')) map$FlagName[map$FlagName == 'Denmark (Dis)'] <- 'Denmark'
  if(any(map$FlagName == 'Spain (Csr)')) map$FlagName[map$FlagName == 'Spain (Csr)'] <- 'Spain'
  if(any(map$FlagName == 'Norway (Nis)')) map$FlagName[map$FlagName == 'Norway (Nis)'] <- 'Norway'
  if(any(map$FlagName == 'Faeroes (Fas)')) map$FlagName[map$FlagName == 'Faeroes (Fas)'] <- 'Faeroe Islands'
  if(any(map$FlagName == 'France (Fis)')) map$FlagName[map$FlagName == 'France (Fis)'] <- 'France'
  if(any(map$FlagName == 'Fiji False')) map$FlagName[map$FlagName == 'Fiji False'] <- 'Fiji'
  if(any(map$FlagName == 'Maldives False')) map$FlagName[map$FlagName == 'Maldives False'] <- 'Maldives'
  if(any(map$FlagName == 'Micronesia False')) map$FlagName[map$FlagName == 'Micronesia False'] <- 'Micronesia'
  if(any(map$FlagName == 'Mongolia False')) map$FlagName[map$FlagName == 'Mongolia False'] <- 'Mongolia'
  if(any(map$FlagName == 'Samoa False')) map$FlagName[map$FlagName == 'Samoa False'] <- 'Samoa'
  if(any(map$FlagName == 'Belize False')) map$FlagName[map$FlagName == 'Belize False'] <- 'Belize'
  if(any(map$FlagName == 'Bolivia False')) map$FlagName[map$FlagName == 'Bolivia False'] <- 'Bolivia'
  if(any(map$FlagName == 'Congo (Democratic Republic) False')) map$FlagName[map$FlagName == 'Congo (Democratic Republic) False'] <- 'Congo (Democratic Republic)'
  
  # drop extra levels of flags if flagname is a factor
  if(class(map$FlagName) == "factor") map$FlagName <- droplevels(map$FlagName) 
  
  # keep 15 largest categories for flagname, bin the rest
  # 15 was chosen with intuition only, to add some info to model
  # but not too much flexibility/complexity
  map$FlagNameBin <- as.character(map$FlagName)
  map$FlagNameBin[!(map$FlagName %in% names(table(map$FlagName)[order(table(map$FlagName),decreasing=TRUE)[1:15]]))] <- "Other"
  map$FlagNameBin <- factor(map$FlagNameBin)
  
  if(verbose > 0) cat('Adding FlagNameContinent values to metadata...\n')
  map$FlagNameContinent <- character(nrow(map))
  ix <- match(map$FlagName,names(FLAGNAMES.CONTINENT.MAP))
  map$FlagNameContinent <- FLAGNAMES.CONTINENT.MAP[ix]
  map$FlagNameContinent <- as.factor(map$FlagNameContinent)
  
  string2factor.list <- c('ShiptypeEU','FlagName','ShiptypeLevel2','ShiptypeLevel3','ShiptypeLevel4')
  for(column.name in string2factor.list) map[,column.name] <- factor(map[,column.name])
  
  # These variables are all supposed to be positive (above zero)
  # Set any zero values to NA
  for(i in 1:length(positive.vars)){
    var <- positive.vars[i]
    na.ix <- is.na(map[,var])
    if(any(map[!na.ix,var] == 0)) map[!na.ix,var][map[!na.ix,var] == 0] <- NA
  }
  

  map$ImputedValues <- character(nrow(map))
  map$NumImputedValues <- numeric(nrow(map))

  # get list of values that will be imputed for each sample
  if(verbose > 0) cat('Adding fields to indicate which fields were imputed for each ship...\n')
  is_imputed <- is.na(map[,positive.vars])
  colnames(is_imputed) <- sprintf('%s_imputed',positive.vars)
  map <- cbind(map,is_imputed)
  map$NumImputedFields <- apply(map[,colnames(is_imputed)],1,sum)
  
  # impute missing values e.g. powerkwaux  (auxiliary power)
  if(any(is.na(map)) && imputation.method != 'none'){
    if(verbose > 0) cat('NAs detected in input table...\n')
    if(imputation.method == 'quick'){
      if(verbose > 0) cat('Filling NAs with median/mode...\n')
      # quick means use median/mode of non-missing values
      for(var in positive.vars){
        # set to numeric because na.roughfix doesn't like "int" types
        map[,var] <- as.numeric(map[,var]) 
      }
      xfix.median <- na.roughfix(map[,positive.vars])
      for(var.name in positive.vars){
        if(any(is.na(map[,var.name]))){
          map[,var.name] <- xfix.median[,var.name]
        }
      }
    } else if(imputation.method == 'rf'){
      nreps <- 20 # chose these values using results from  lib/dev/test_imputation_methods.r 
      maxit <- 2
      if(verbose > 0) cat('Running "rf" imputation of missing values, this can be slow...\n')
      # run MICE package imputation 20 times
      # note: this is slow for large data sets
                   
      yfix <- mice(map[,predictor.names],m=nreps,maxit=maxit,method='rf') # 10 iterations, then average them
      for(var.name in positive.vars){
        if(var.name %in% names(yfix$imp) && any(is.na(map[,var.name]))){
          # note: the reason we need to take first nrow(x) rows of imputations
          # is in case the lookup table was appended
          na.ix <- which(is.na(map[,var.name]))
          map[na.ix,var.name] <- rowMeans(yfix$imp[[var.name]][1:length(na.ix),,drop=F])
        }
      }
    } else if(imputation.method == 'kNN'){
      # use a subset of all data as training examples, 
      # otherwise pairwise distance matrix is too large
      # use ships with the most complete data for training
      n.training.examples <- 5000
      train.ix <- sample(order(map$NumImputedValues),n.training.examples)
      test.ix <- which(map$NumImputedValues > 0)
      
      # impute data for up to 1000 samples at a time
      # this ensures that kNN distance matrix doesn't get too large
      if(verbose > 0) cat('Imputing missing data using kNN for',length(test.ix),'samples.\n')
      count <- 1
      batch.size <- 1000
      while(count < length(test.ix)){
        if(verbose > 0) cat(count,'')
        test.ix.subset <- test.ix[count:min(length(test.ix),count + batch.size - 1)]
        newmap <- kNN(map[c(test.ix.subset,train.ix),,drop=F],positive.vars)
        map[test.ix.subset,positive.vars] <- newmap[1:length(test.ix.subset),positive.vars]
        count <- count + batch.size
      }
      if(verbose > 0) cat(count,'\n')
    }
  }
  
  # Put predictor columns at beginning, and "imputed" columns at the end
  new.column.order <- 
  map <- map[]
  
  # Add rownames/IMO numbers as the first column
  map <- cbind(rownames(map),map)
  colnames(map)[1] <- 'IMO.Number'
  if(!is.null(metadata.output.filepath)) write.csv(map,file=metadata.output.filepath,quote=TRUE,row.names=FALSE)
  return(map)
}



# plot distributions of features with outliers labeled
# x is the full ship data frame
# outlier.ix is TRUE/FALSE outlier for each column
"plot.feature.distributions" <- function(x,
                                         var.names,
                                         use.log, # tells whether to use log for outlier detection
                                         outlier.IQR.threshold = 3,
                                         outdir='output'){
  
  # plot distributions before dropping outliers
  dir.create(outdir,showWarnings = FALSE) 
  
  pdf(paste(outdir,'/feature-distributions-before-cleaning.pdf',sep=''),width=12,height=9)
  par(mfrow=c(3,4),mar=c(5,4,4,2))
  for(i in 1:length(var.names)){
    continuous.var <- var.names[i]
    if(continuous.var %in% use.log){
      outlier.ix.i <- is.outlier(log10(x[,continuous.var]),outlier.IQR.threshold)
      h <- hist(log10(x[,continuous.var]),br=30,main=sprintf('log10(%s)\n(%d outliers)',continuous.var,sum(outlier.ix.i)),cex.main=1,xlab='',las=2,cex.lab=.75)
      for(ix in which(outlier.ix.i)) lines(rep(log10(x[ix,continuous.var]),2),c(0,max(h$counts)),col='#99111177',new=FALSE)
    } else{
      outlier.ix.i <- is.outlier(x[,continuous.var],outlier.IQR.threshold)
      h <- hist(x[,continuous.var],br=30,main=sprintf('%s\n(%d outliers)',continuous.var,sum(outlier.ix.i)),cex.main=1,xlab='',las=2,cex.lab=.75)
      for(ix in which(outlier.ix.i)) lines(rep(x[ix,continuous.var],2),c(0,max(h$counts)),col='#99111177',new=FALSE)
    }
  }
  dev.off()
  
}

# pairwise scatterplots to viz interactions
# use sample of data to speed this up
"plot.feature.interactions" <- function(x, vars, outdir='output'){
  # plot distributions before dropping outliers
  dir.create(outdir,showWarnings = FALSE) 
  
  pdf(paste(outdir,'/pairwise.scatterplots.pdf',sep=''),width=9,height=9)
  pairs(x[sample(nrow(x),1000),continuous.vars],
        col=NA,bg='#2200aa22',pch=21,las=2,cex.lab=2,cex.axis=.75,
        lower.panel=NULL,
        labels=substring(continuous.vars,1,12))
  dev.off()
}




