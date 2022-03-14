source("~/Dropbox/research/climate-trace-shipping/lib/load.r")
source("~/Dropbox/research/climate-trace-shipping/lib/rf.r")

######### 
# Run with 1 missing feature per sample
#########
nreps <- 3 # each rep takes hours

# for each numeric predictor, set 3000 random rows to NA
cat('Choosing random indices...\n')
numeric.vars <- c('Deadweight','GrossTonnage','Length','Breadth','Draught','Powerkwmax','Powerkwaux','Speed')
mae.1.missing <- matrix(0,nrow=length(numeric.vars),ncol=nreps)
rownames(mae.1.missing) <- numeric.vars
colnames(mae.1.missing) <- sprintf('rep%02d',1:nreps)

for(rep.i in 1:nreps){
  cat('\n\nREP',rep.i,'\n\n')
  cat('Loading raw ship metadata...\n')
  m <- load.raw.ship.metadata()
  cat('Loading EU ship data...\n')
  x.eu <- load.EU.MRV.ship.data()
  
  x.eu$IMO.Number <- as.character(x.eu$IMO.Number)
  x.eu <- x.eu[x.eu$IMO.Number %in% rownames(m),]
  eu.ix <- match(x.eu$IMO.Number, rownames(m))
  
  ix.fake.missing <- list()
  random.ix <- sample(nrow(x.eu))
  count <- 1
  batch.size <- 3000
  # set to NA and store those indices
  for(numeric.var in numeric.vars){
    random.ix.i <- random.ix[count:(count + batch.size - 1)]
    ix.fake.missing[[numeric.var]] <- random.ix.i
    imo.numbers <- x.eu$IMO.Number[random.ix.i]
    if(numeric.var == 'Length'){
      m[imo.numbers,c("LengthOverallLOA","LengthRegistered")] <- 0 # will be treated as missing
    } else if (numeric.var == 'Speed'){
      m[imo.numbers,c("Speed","Speedmax","Speedservice")] <- NA
    } else if (numeric.var == 'Powerkwaux'){
      m[imo.numbers,c("TotalPowerOfAuxiliaryEngines")] <- NA
    } else {
      m[imo.numbers,numeric.var] <- NA
    }
    count <- count + batch.size
  }
  
  cat('Saving raw metadata with fake missing data...\n')
  write.csv(m, file='metadata.fake.missing.data.csv',quote=TRUE,row.names=TRUE)
  
  # impute
  cat('Running imputation...\n')
  m.post <- preprocess.ship.metadata('metadata.fake.missing.data.csv',
                                     metadata.output.filepath = 'metadata.fake.missing.data-impute-rf.csv',
                                     imputation.method = 'rf')
  
  # add data to EU table
  cat('Adding imputed metadata to EU data...\n')
  x.eu.imputed <- load.EU.MRV.ship.data.and.metadata(metadata.filepath='metadata.fake.missing.data-impute-rf.csv')
  
  # train RF and get OOB predictions
  predictor.names <- c('Deadweight','FlagNameBin','FlagNameContinent','GrossTonnage','Length','Breadth','Draught','ShiptypeEU','ShiptypeLevel2','Powerkwmax','Powerkwaux','Speed','YearOfBuild')
  xoh <- model.matrix(~ ., data=x.eu.imputed[,predictor.names])[,-1]
  y <- x.eu.imputed$kg.CO2.per.nm
  cat('Running rf...\n')
  my.rf <- my.rf.tune(xoh,y,params=list(mtry=15, nodesize=8,ntree=1000))
  yhat <- my.rf$yhat
  
  cat('Reporting performance...\n')
  for(numeric.var in numeric.vars){
    random.ix.i <- ix.fake.missing[[numeric.var]]
    imo.numbers <- x.eu$IMO.Number[random.ix.i]
    new.ix <- match(imo.numbers, x.eu.imputed$IMO.Number)
    new.ix <- new.ix[!is.na(new.ix)]
    mae <- mean(abs(yhat[new.ix] - y[new.ix])/y[new.ix],na.rm=TRUE)
    mae.1.missing[which(numeric.vars == numeric.var),rep.i] <- mae
    cat(numeric.var,mae,'\n')
  }
}

######### 
# Run with 2, 3, 4, 5 missing features per sample
#########
n.missing <- c(2,3,4,5,6)
nreps <- 3 # each rep takes hours

# for roughly 1/3 of samples (10k), set n.missing[j] random features to NA
cat('Choosing random indices...\n')
numeric.vars <- c('Deadweight','GrossTonnage','Length','Breadth','Draught','Powerkwmax','Powerkwaux','Speed')
mae.n.missing <- list()
  
for(rep.i in 1:nreps){
  for(j in (1:length(n.missing))[1:3]){
    if(rep.i == 1){
      mae.n.missing[[j]] <- numeric(nreps)
      names(mae.n.missing)[j] <- sprintf('%d_missing',n.missing[j])
    }
    cat('\n\nMissing',n.missing[j],'REP',rep.i,'\n\n')
    cat('Loading raw ship metadata...\n')
    m <- load.raw.ship.metadata()
    cat('Loading EU ship data...\n')
    x.eu <- load.EU.MRV.ship.data()
    x.eu$IMO.Number <- as.character(x.eu$IMO.Number)
    x.eu <- x.eu[x.eu$IMO.Number %in% rownames(m),]
    eu.ix <- match(x.eu$IMO.Number, rownames(m))
    
    ix.fake.missing <- list()
    random.ix <- sample(nrow(x.eu))
    batch.size <- 10000
    # get indices in m ahead of time
    random.ix.in.m <- match(x.eu$IMO.Number[random.ix[1:batch.size]],rownames(m))
    
    cat('Adding missing data...')
    for(i in 1:batch.size){
      if(i %% 1000 == 0) cat(i,'')
      # sample two vars to make NA
      random.ix.i.in.m <- random.ix.in.m[i]
      numeric.vars.i <- sample(numeric.vars,n.missing[j])
      for(k in 1:length(numeric.vars.i)){
        numeric.var <- numeric.vars.i[k]
        if(numeric.var == 'Length'){
          m[random.ix.i.in.m,c("LengthOverallLOA","LengthRegistered")] <- 0 # will be treated as missing
        } else if (numeric.var == 'Speed'){
          m[random.ix.i.in.m,c("Speed","Speedmax","Speedservice")] <- NA
        } else if (numeric.var == 'Powerkwaux'){
          m[random.ix.i.in.m,c("TotalPowerOfAuxiliaryEngines")] <- NA
        } else {
          m[random.ix.i.in.m,numeric.var] <- NA
        }
      }
    }
    cat('\n')
    cat('Saving raw metadata with fake missing data...\n')
    write.csv(m, file='metadata.fake.missing.data.csv',quote=TRUE,row.names=TRUE)
    
    # impute
    cat('Running imputation...\n')
    m.post <- preprocess.ship.metadata('metadata.fake.missing.data.csv',
                                       metadata.output.filepath = 'metadata.fake.missing.data-impute-rf.csv',
                                       imputation.method = 'rf')
    
    # add data to EU table
    cat('Adding imputed metadata to EU data...\n')
    x.eu.imputed <- load.EU.MRV.ship.data.and.metadata(metadata.filepath='metadata.fake.missing.data-impute-rf.csv')
    
    # train RF and get OOB predictions
    predictor.names <- c('Deadweight','FlagNameBin','FlagNameContinent','GrossTonnage','Length','Breadth','Draught','ShiptypeEU','ShiptypeLevel2','Powerkwmax','Powerkwaux','Speed','YearOfBuild')
    xoh <- model.matrix(~ ., data=x.eu.imputed[,predictor.names])[,-1]
    y <- x.eu.imputed$kg.CO2.per.nm
    cat('Running rf...\n')
    my.rf <- my.rf.tune(xoh,y,params=list(mtry=15, nodesize=8,ntree=1000))
    yhat <- my.rf$yhat
    
    cat('Reporting performance...\n')
    imo.numbers <- x.eu$IMO.Number[random.ix]
    new.ix <- match(imo.numbers, x.eu.imputed$IMO.Number)
    new.ix <- new.ix[!is.na(new.ix)]
    mae <- mean(abs(yhat[new.ix] - y[new.ix])/y[new.ix],na.rm=TRUE)
    mae.n.missing[[j]][rep.i] <- mae
    cat(n.missing[j],'missing, rep',rep.i,':',mae,'\n')
  }
}

stop('hi\n')

#####
# Now run with no fake missing data; only impute naturally missing powerkwaux
# this is because the OOB estimates from RF might differ slightly from the holdout data
#####

cat('\n\nRunning with no fake missing data\n',rep.i,'\n\n')
# load standard EU table
cat('Loading standard imputed metadata for EU data...\n')
x.eu.imputed <- load.EU.MRV.ship.data.and.metadata(metadata.filepath='../../data/IHS-imputed-rf.csv')

mae.none.missing <- numeric(nreps)
for(rep.i in 1:nreps){
  cat('\n\nREP',rep.i,'\n\n')
  
  # train RF and get OOB predictions
  predictor.names <- c('Deadweight','FlagNameBin','FlagNameContinent','GrossTonnage','Length','Breadth','Draught','ShiptypeEU','ShiptypeLevel2','Powerkwmax','Powerkwaux','Speed','YearOfBuild')
  xoh <- model.matrix(~ ., data=x.eu.imputed[,predictor.names])[,-1]
  y <- x.eu.imputed$kg.CO2.per.nm
  cat('Running rf...\n')
  my.rf <- my.rf.tune(xoh,y,params=list(mtry=15, nodesize=8,ntree=1000))
  yhat <- my.rf$yhat
  mae <- mean(abs(yhat - y)/y,na.rm=TRUE)
  mae.none.missing[rep.i] <- mae
  cat('None missing, rep',rep.i,':',mae,'\n')
}

save(mae.none.missing, mae.1.missing, mae.n.missing, file='results.rdata')