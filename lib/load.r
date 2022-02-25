library('mice', warn.conflicts = F, quietly = T)
# get path to source files from R environment
if(Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME') == ''){
  stop('R_CLIMATE_TRACE_SHIPPING_HOME not in environment variables. Please add "R_CLIMATE_TRACE_SHIPPING_HOME=/path/to/r/climate/trace/shipping/repo" to the file .Renviron in your home directory. Read more about the .Renviron file here: https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf')
}

CTHOME=Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME')

source(paste(CTHOME,'/lib/lib.r',sep=''))

"load.generic.ship.data.and.metadata" <- function(ship.filepath,
                                                 metadata.filepath=paste(CTHOME,'/data/IHS complete Ship Data.csv',sep=''),
                                                 IMO.column='IMO.Number',
                                                 preprocess=TRUE,
                                                 remove.outliers=TRUE, # disable for predictions on new data
                                                 imputation.method=c('rf','quick','none')[1], # imputation method for missing values, if preprocessing
                                                 imputation.lookup.table=NULL, # table of known data to use for imputation
                                                 verbose=TRUE
){
  x <- read.csv(ship.filepath)

  # replace spaces with "." in the provided IMO.column string,
  # to match the auto-replacement in read.csv
  IMO.column <- gsub(' ','.',IMO.column)
  
  # rename the provided IMO number column to 'IMO.Number'
  colnames(x)[colnames(x) == IMO.column] <- 'IMO.Number'
  # keep only IMO Number column
  x <- x[,'IMO.Number',drop=F]
  
  map <- load.ship.metadata(metadata.filepath)
  x <- add.ship.metadata(x, map)
  if(preprocess) x <- preprocess.ship.data(x,
                                           do.plots = FALSE,
                                           remove.outliers=remove.outliers,
                                           imputation.method=imputation.method,
                                           imputation.lookup.table=imputation.lookup.table,
                                           verbose=verbose)
  return(x)
}

"load.EU.MRV.ship.data.and.metadata" <- function(ship.filepath=paste(CTHOME,'/data/EU MRV data 18-19-20.csv',sep=''),
                                          metadata.filepath=paste(CTHOME,'/data/IHS complete Ship Data.csv',sep=''),
                                          outdir='output', # outdir for preprocessing visualizations
                                          do.plots=FALSE, # plot feature distributions, outliers, interactions
                                          preprocess=TRUE,
                                          remove.outliers=TRUE, # disable for predictions on new data
                                          imputation.method=c('rf','quick','none')[1], # imputation method for missing values, if preprocessing
                                          imputation.lookup.table=NULL, # table of known data to use for imputation
                                          verbose=TRUE
                                        ){
  x <- load.EU.MRV.ship.data(ship.filepath)
  map <- load.ship.metadata(metadata.filepath)
  x <- add.ship.metadata(x, map)
  if(preprocess) x <- preprocess.ship.data(x,
                                           outdir = outdir,
                                           do.plots = do.plots,
                                           remove.outliers=remove.outliers,
                                           imputation.method=imputation.method,
                                           imputation.lookup.table=imputation.lookup.table,
                                           verbose=verbose)
  return(x)
}

"load.ship.metadata" <- function(filepath=paste(CTHOME,'/data/IHS complete Ship Data.csv',sep=''),
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
# adds these to the original table as new columns,
# or NA where missing
# Ships entirely missing from metadata table are dropped
#
# x must have a column named "IMO.Number"
# TO DO: allow choice of whether to retain ships with no metadata
#
"add.ship.metadata" <- function(x, map,
                                verbose=1){
  # Drop ships missing from metadata file
  # convert IMO number to string so that it can be used for indexing
  x$IMO.Number <- as.character(x$IMO.Number)
  drop.ix <- which(!(x$IMO.Number %in% rownames(map)))
  if(length(drop.ix) > 0){
    x <- x[-drop.ix,,drop=F]
    cat('Warning: dropped',length(drop.ix),'entries for ships not present in metadata file.\n')
  }
  # add columns from metadata file
  x$deadweight <- map[x$IMO.Number,'Deadweight']
  x$flagname <- map[x$IMO.Number,'FlagName']
  x$grosstonnage <- map[x$IMO.Number,'GrossTonnage']
  x$length <- apply(map[x$IMO.Number,c('LengthOverallLOA','LengthRegistered')],1,max)
  x$breadth <- map[x$IMO.Number,'Breadth']
  x$draught <- map[x$IMO.Number,'Draught']
  x$shiptype2 <- map[x$IMO.Number,'ShiptypeLevel2']
  x$shiptype3 <- map[x$IMO.Number,'ShiptypeLevel3']
  x$shiptype4 <- map[x$IMO.Number,'ShiptypeLevel4']
  x$powerkwmax <- map[x$IMO.Number,'Powerkwmax']
  x$powerkwaux <- map[x$IMO.Number,'TotalPowerOfAuxiliaryEngines']
  x$speedmax <- map[x$IMO.Number,'Speedmax']
  # need to fill zeros in speedmax using speed:
  # use the average ratio of speed to speedmax (when not equal)
  speed <- map[x$IMO.Number,'Speed']
  ix <- which(speed > 0 & x$speedmax > 0 & speed != x$speedmax)
  ratio <- mean(speed[ix] / x$speedmax[ix])
  x$speedmax[x$speedmax == 0] <- speed[x$speedmax == 0] /ratio # the average factor tends to be close to 0.91
  x$yearbuilt <- map[x$IMO.Number,'YearOfBuild']
  
  # if shiptype.original is not present, assign closest matches
  if(!('shiptype.original' %in% colnames(x))){
    shiptype.original <- character(nrow(x))
    shiptype.original[x$shiptype4 %in% c('Bulk Carrier','Cement Carrier','Ore Carrier','Pipe Burying Vessel','Self Discharging Bulk Carrier')] <- 'Bulk carrier'
    shiptype.original[x$shiptype4 %in% c('Chemical Tanker','Chemical/Oil Products Tanker')] <- 'Chemical tanker'
    shiptype.original[x$shiptype4 %in% c('Bulk/Oil Carrier')] <- 'Combination carrier'
    shiptype.original[x$shiptype4 %in% c('Container Ship')] <- 'Container ship'
    shiptype.original[x$shiptype4 %in% c('Container/Ro-Ro Cargo Ship')] <- 'Container/ro-ro cargo ship'
    shiptype.original[x$shiptype4 %in% c('LPG Tanker')] <- 'Gas carrier'
    shiptype.original[x$shiptype4 %in% c('Aggregates Carrier','Deck Cargo Ship','General Cargo Ship','Palletised Cargo Ship')] <- 'General cargo ship'
    shiptype.original[x$shiptype4 %in% c('Bunkering Tanker','FPSO (Floating, Production, Storage, Offloading)','LNG Tanker')] <- 'LNG carrier'
    shiptype.original[x$shiptype4 %in% c('Bitumen Tanker','Crude Oil Tanker','Oil Products Tanker')] <- 'Oil tanker'
    shiptype.original[x$shiptype4 %in% c('Passenger (Cruise) Ship')] <- 'Passenger ship'
    shiptype.original[x$shiptype4 %in% c('Fruit Juice Tanker','Refrigerated Cargo Ship')] <- 'Refrigerated cargo carrier'
    shiptype.original[x$shiptype4 %in% c('Passenger/General Cargo Ship','Passenger/Ro-Ro Cargo Ship')] <- 'Ro-pax ship'
    shiptype.original[x$shiptype4 %in% c('Ro-Ro Cargo Ship')] <- 'Ro-ro ship'
    shiptype.original[x$shiptype4 %in% c('Vehicles Carrier')] <- 'Vehicle carrier'
    shiptype.original[x$shiptype4 == ""] <- "Other ship types" # assign all left over to other ship types
    x$shiptype.original <- shiptype.original
    
    # Note: the following commented code was used to generate the best mapping,
    # based on 2018-2020 EU reporting data,
    # which was then hard-coded above.
    # 
    # # tally how many ships map from each EU shiptype (x) to each
    # # metadata shiptype level 4 (map)
    # shiptype.mat <- matrix(0,nrow=length(unique(x$shiptype.original)),ncol=length(unique(x$shiptype4)))
    # rownames(shiptype.mat) <- sort(unique(x$shiptype.original))
    # colnames(shiptype.mat) <- sort(unique(x$shiptype4))
    # for(shiptype.i in rownames(shiptype.mat)){
    #   for(shiptype.j in colnames(shiptype.mat)){
    #     shiptype.mat[shiptype.i,shiptype.j] <- sum(x$shiptype.original==shiptype.i & x$shiptype4 == shiptype.j)
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
    #   cat('shiptype.original[x$shiptype4 %in% c(')
    #   cat(paste("'",paste(mappings[[shiptype.i]],collapse="','"),"'",sep=''))
    #   cat(paste(')] <- ',"'",shiptype.i,"'\n",sep=''))
    # }
  }
  
  
  # convert numeric-intended columns to numeric
  if('average.speed' %in% colnames(x)) x$average.speed <- suppressWarnings(as.numeric(as.character(x$average.speed)))
  if('distance.traveled' %in% colnames(x)) x$distance.traveled <- suppressWarnings(as.numeric(as.character(x$distance.traveled)))
  if('time.at.sea' %in% colnames(x)) x$time.at.sea <- suppressWarnings(as.numeric(as.character(x$time.at.sea)))
  if('kg.CO2.per.nm' %in% colnames(x)) x$kg.CO2.per.nm <- suppressWarnings(as.numeric(as.character(x$kg.CO2.per.nm)))
  x$powerkwmax <- as.numeric(x$powerkwmax)
  x$powerkwaux <- as.numeric(x$powerkwaux)
  
  # deduplicate and bin flag names
  # Fix duplicate flagnames
  if(any(x$flagname == 'Portugal (Mar)')) x$flagname[x$flagname == 'Portugal (Mar)'] <- 'Portugal'
  if(any(x$flagname == 'Denmark (Dis)')) x$flagname[x$flagname == 'Denmark (Dis)'] <- 'Denmark'
  if(any(x$flagname == 'Spain (Csr)')) x$flagname[x$flagname == 'Spain (Csr)'] <- 'Spain'
  if(any(x$flagname == 'Norway (Nis)')) x$flagname[x$flagname == 'Norway (Nis)'] <- 'Norway'
  if(any(x$flagname == 'Faeroes (Fas)')) x$flagname[x$flagname == 'Faeroes (Fas)'] <- 'Faeroe Islands'
  if(any(x$flagname == 'France (Fis)')) x$flagname[x$flagname == 'France (Fis)'] <- 'France'
  if(any(x$flagname == 'Tanzania (Zanzibar)')) x$flagname[x$flagname == 'Tanzania (Zanzibar)'] <- 'Tanzania'
  
  # drop extra levels of flags if flagname is a factor
  if(class(x$flagname) == "factor") x$flagname <- droplevels(x$flagname) 
  
  # keep 15 largest categories for flagname, bin the rest
  # 15 was chosen with intuition only, to add some info to model
  # but not too much flexibility/complexity
  x$flagname.binned <- as.character(x$flagname)
  x$flagname.binned[!(x$flagname %in% names(table(x$flagname)[order(table(x$flagname),decreasing=TRUE)[1:15]]))] <- "Other"
  x$flagname.binned <- factor(x$flagname.binned)
  
  # make a "continent" vector
  flagnames.continent.map <- list()
  flagnames.continent.map[['Algeria']] <- 'Africa'
  flagnames.continent.map[['Antigua & Barbuda']] <- 'Carribean'
  flagnames.continent.map[['Azerbaijan']] <- 'Asia'
  flagnames.continent.map[['Bahamas']] <- 'Carribean'
  flagnames.continent.map[['Bahrain']] <- 'Middle East'
  flagnames.continent.map[['Bangladesh']] <- 'Asia'
  flagnames.continent.map[['Barbados']] <- 'Carribean'
  flagnames.continent.map[['Belgium']] <- 'Europe'
  flagnames.continent.map[['Belize']] <- 'Central America'
  flagnames.continent.map[['Bermuda']] <- 'Carribean'
  flagnames.continent.map[['Brazil']] <- 'South America'
  flagnames.continent.map[['Bulgaria']] <- 'Europe'
  flagnames.continent.map[['Cameroon']] <- 'Africa'
  flagnames.continent.map[['Canada']] <- 'North America'
  flagnames.continent.map[['Cayman Islands']] <- 'Carribean'
  flagnames.continent.map[['Chile']] <- 'South America'
  flagnames.continent.map[["China, People's Republic Of"]] <- 'Asia'
  flagnames.continent.map[['Chinese Taipei']] <- 'Asia'
  flagnames.continent.map[['Comoros']] <- 'Africa'
  flagnames.continent.map[['Cook Islands']] <- 'Oceana'
  flagnames.continent.map[['Croatia']] <- 'Europe'
  flagnames.continent.map[['Cuba']] <- 'Carribean'
  flagnames.continent.map[['Curacao']] <- 'South America'
  flagnames.continent.map[['Cyprus']] <- 'Europe'
  flagnames.continent.map[['Denmark']] <- 'Europe'
  flagnames.continent.map[['Djibouti']] <- 'Africa'
  flagnames.continent.map[['Dominica']] <- 'Carribean'
  flagnames.continent.map[['Ecuador']] <- 'South America'
  flagnames.continent.map[['Egypt']] <- 'Africa'
  flagnames.continent.map[['Estonia']] <- 'Europe'
  flagnames.continent.map[['Faeroe Islands']] <- 'Europe'
  flagnames.continent.map[['Finland']] <- 'Europe'
  flagnames.continent.map[['France']] <- 'Europe'
  flagnames.continent.map[['Gabon']] <- 'Africa'
  flagnames.continent.map[['Georgia']] <- 'Europe'
  flagnames.continent.map[['Germany']] <- 'Europe'
  flagnames.continent.map[['Gibraltar']] <- 'Europe'
  flagnames.continent.map[['Greece']] <- 'Europe'
  flagnames.continent.map[['Honduras']] <- 'Central America'
  flagnames.continent.map[['Hong Kong, China']] <- 'Asia'
  flagnames.continent.map[['India']] <- 'Asia'
  flagnames.continent.map[['Indonesia']] <- 'Oceana'
  flagnames.continent.map[['Iran']] <- 'Middle East'
  flagnames.continent.map[['Irish Republic']] <- 'Europe'
  flagnames.continent.map[['Isle Of Man']] <- 'Europe'
  flagnames.continent.map[['Israel']] <- 'Middle East'
  flagnames.continent.map[['Italy']] <- 'Europe'
  flagnames.continent.map[['Jamaica']] <- 'Carribean'
  flagnames.continent.map[['Japan']] <- 'Asia'
  flagnames.continent.map[['Jordan']] <- 'Middle East'
  flagnames.continent.map[['Korea, South']] <- 'Asia'
  flagnames.continent.map[['Kuwait']] <- 'Middle East'
  flagnames.continent.map[['Latvia']] <- 'Europe'
  flagnames.continent.map[['Lebanon']] <- 'Middle East'
  flagnames.continent.map[['Liberia']] <- 'Africa'
  flagnames.continent.map[['Libya']] <- 'Africa'
  flagnames.continent.map[['Lithuania']] <- 'Europe'
  flagnames.continent.map[['Luxembourg']] <- 'Europe'
  flagnames.continent.map[['Malaysia']] <- 'Asia'
  flagnames.continent.map[['Malta']] <- 'Europe'
  flagnames.continent.map[['Marshall Islands']] <- 'Oceana'
  flagnames.continent.map[['Mauritius']] <- 'Africa'
  flagnames.continent.map[['Mexico']] <- 'Central America'
  flagnames.continent.map[['Moldova']] <- 'Europe'
  flagnames.continent.map[['Montenegro']] <- 'Europe'
  flagnames.continent.map[['Morocco']] <- 'Africa'
  flagnames.continent.map[['Netherlands']] <- 'Europe'
  flagnames.continent.map[['Nigeria']] <- 'Africa'
  flagnames.continent.map[['Niue']] <- 'Oceana'
  flagnames.continent.map[['Norway']] <- 'Europe'
  flagnames.continent.map[['Pakistan']] <- 'Middle East'
  flagnames.continent.map[['Palau']] <- 'Oceana'
  flagnames.continent.map[['Panama']] <- 'Central America'
  flagnames.continent.map[['Philippines']] <- 'Asia'
  flagnames.continent.map[['Portugal']] <- 'Europe'
  flagnames.continent.map[['Qatar']] <- 'Middle East'
  flagnames.continent.map[['Russia']] <- 'Asia'
  flagnames.continent.map[['Saudi Arabia']] <- 'Middle East'
  flagnames.continent.map[['Seychelles']] <- 'Africa'
  flagnames.continent.map[['Sierra Leone']] <- 'Africa'
  flagnames.continent.map[['Singapore']] <- 'Asia'
  flagnames.continent.map[['South Africa']] <- 'Africa'
  flagnames.continent.map[['Spain']] <- 'Europe'
  flagnames.continent.map[['Sri Lanka']] <- 'Asia'
  flagnames.continent.map[['St Kitts & Nevis']] <- 'Carribean'
  flagnames.continent.map[['St Vincent & The Grenadines']] <- 'Carribean'
  flagnames.continent.map[['Sweden']] <- 'Europe'
  flagnames.continent.map[['Switzerland']] <- 'Europe'
  flagnames.continent.map[['Tanzania']] <- 'Africa'
  flagnames.continent.map[['Thailand']] <- 'Asia'
  flagnames.continent.map[['Togo']] <- 'Africa'
  flagnames.continent.map[['Tunisia']] <- 'Africa'
  flagnames.continent.map[['Turkey']] <- 'Europe'
  flagnames.continent.map[['Tuvalu']] <- 'Oceana'
  flagnames.continent.map[['United Kingdom']] <- 'Europe'
  flagnames.continent.map[['United States Of America']] <- 'North America'
  flagnames.continent.map[['Unknown']] <- 'Unknown'
  flagnames.continent.map[['Vanuatu']] <- 'Oceana'
  flagnames.continent.map[['Vietnam']] <- 'Asia'
  flagnames.continent.map[['Virgin Islands, British']] <- 'Carribean'
  
  x$flagname.continent <- character(nrow(x))
  for(i in 1:nrow(x)){
    if(x$flagname[i] %in% names(flagnames.continent.map)){
      x$flagname.continent[i] <- flagnames.continent.map[[x$flagname[i]]]
    } else {
      if(verbose > 0) cat(paste('Warning, country ',x$flagname[i],' not in continent mapping list\n',sep=''))
      x$flagname.continent[i] <- 'Unknown'
    }
  } 
  x$flagname.continent <- as.factor(x$flagname.continent)

  string2factor.list <- c('shiptype.original','flagname','shiptype2','shiptype3','shiptype4')
  for(column.name in string2factor.list) x[,column.name] <- factor(x[,column.name])
  
  return(x)
}

# drops outliers and ships with invalid or NA values,
# imputes data where requested
# input is a raw ship data frame with all metadata, 
# output is a preprocessed ship data frame
"preprocess.ship.data" <- function(x,
                                   continuous.vars 
                                     = c('kg.CO2.per.nm', # list of known cont. vars
                                         'average.speed',
                                         'distance.traveled',
                                         'time.at.sea',
                                         'deadweight',
                                         'grosstonnage',
                                         'length',
                                         'breadth',
                                         'draught',
                                         'powerkwmax',
                                         'powerkwaux',
                                         'speedmax',
                                         'yearbuilt'),
                                   remove.outliers = TRUE, # disable for predictions on new data
                                   use.log.for.outlier.detection 
                                     = c('kg.CO2.per.nm',
                                         'deadweight',
                                         'grosstonnage',
                                         'breadth',
                                         'powerkwmax',
                                         'speedmax',
                                         'distance.traveled',
                                         'average.speed'),
                                   ignore.outlier.detection.columns
                                     = c('yearbuilt',
                                         'powerkwaux'),
                                   impute.missing.values
                                     = c('powerkwaux'),
                                   imputation.method=c('rf','quick','none')[1],
                                   imputation.lookup.table=NULL, # table of known data to use for imputation
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

  # Drop NA/known bad values
  for(i in 1:length(continuous.vars)){
    continuous.var <- continuous.vars[i]
    if(continuous.var %in% colnames(x)){
      # only test if this var is in columns of x
      if(!(continuous.var %in% impute.missing.values)){
        # don't drop empty values if we are supposed to impute them for this var
        # drop ships with NA value for this continuous var
        x <- droplevels(x[!is.na(x[,continuous.var]),])
        # drop ships
        x <- droplevels(x[x[,continuous.var] > 0,])
      }
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
    for(i in 1:length(continuous.vars)){
      continuous.var <- continuous.vars[i]
      if(!(continuous.var %in% ignore.outlier.detection.columns) &&
         continuous.var %in% colnames(x)){
        if(continuous.var %in% use.log.for.outlier.detection){
          outlier.ix <- outlier.ix | is.outlier(log10(x[,continuous.var]),outlier.IQR.threshold)
        } else{
          outlier.ix <- outlier.ix | is.outlier(x[,continuous.var],outlier.IQR.threshold)
        }
      }
    }
  }  
  if(do.plots){
    plot.feature.distributions(x, continuous.vars, use.log=use.log.for.outlier.detection,
                                             outlier.IQR.threshold = outlier.IQR.threshold,
                                             outdir=outdir)
  }
  
  if(remove.outliers){
    # Now drop the outliers detected
    x <- droplevels(x[!outlier.ix,])
    cat('Dropped',sum(outlier.ix),sprintf('ships (%.3f%%)',100*sum(outlier.ix)/(nrow(x) + sum(outlier.ix))), 'with outlier values for continuous variables.\n')
  }  

  # impute missing values e.g. powerkwaux  (auxiliary power)
  if(length(impute.missing.values) > 0 && any(is.na(x)) && imputation.method != 'none'){
    if(verbose > 0) cat('NAs detected in input table...\n')
    if(imputation.method == 'quick' && is.null(imputation.lookup.table)){
      if(verbose > 0) cat('Filling NAs with median/mode...\n')
      # quick means use median/mode of non-missing values
      xfix.median <- na.roughfix(x[,-1])
      for(var.name in impute.missing.values){
        if(any(is.na(x[,var.name]))){
          x[,var.name] <- xfix.median[,var.name]
        }
      }
    } else if(imputation.method == 'quick' && !is.null(imputation.lookup.table)){
      if(verbose > 0) cat('Filling NAs with quick method from lookup table...\n')
      # lookup means impute using median/mode of 
      # variable of same name in provided lookup table
      for(var.name in impute.missing.values){
        if(any(is.na(x[,var.name]))){
          if(class(x[,var.name]) %in% c('string','factor')){
            # use mode for string/factor
            table.i <- table(imputation.lookup.table[,var.name])
            fill.val <- names(table.i)[which.max(table.i)]
          } else {
            fill.val <- median(imputation.lookup.table[,var.name])
          }
          if(verbose > 0) cat('Fill value for column',var.name,'is',fill.val,'\n')
          x[is.na(x[,var.name]),var.name] <- fill.val
        }
      }
    } else if(imputation.method == 'rf'){
      nreps <- 20 # chose these values using results from  lib/dev/test_imputation_methods.r 
      maxit <- 1 
      if(verbose > 0) cat('Running "rf" imputation of missing values, this can be slow...\n')
      # run MICE package imputation 10 times
      # note: this is slow for large data sets
      if(!is.null(imputation.lookup.table)){
        # if imputation lookup table was provided, 
        # use it to augment the imputation
        # find intersection of column names between the two tables
        common.columns <- intersect(colnames(x), colnames(imputation.lookup.table))
        tmpx <- rbind(x[,common.columns],imputation.lookup.table[,common.columns])
        tmpx <- tmpx[,colnames(tmpx) != "IMO.Number"]
        yfix <- mice(tmpx,m=nreps,maxit=maxit,method='rf') # 10 iterations, then average them
      } else {
        yfix <- mice(x[,colnames(x) != "IMO.Number"],m=nreps,maxit=maxit,method='rf') # 10 iterations, then average them
      }
      
      for(var.name in impute.missing.values){
        if(var.name %in% names(yfix$imp) && any(is.na(x[,var.name]))){
          # note: the reason we need to take first nrow(x) rows of imputations
          # is in case the lookup table was appended
          na.ix <- which(is.na(x[,var.name]))
          x[na.ix,var.name] <- rowMeans(yfix$imp[[var.name]][1:length(na.ix),,drop=F])
        }
      }
    }
  }
  return(x)
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




