source("~/Dropbox/research/climate-trace-shipping/lib/load.r")
source("~/Dropbox/research/climate-trace-shipping/lib/rf.r")

m <- load.raw.ship.metadata()
x.eu <- load.EU.MRV.ship.data()
x.eu$IMO.Number <- as.character(x.eu$IMO.Number)
x.eu <- x.eu[x.eu$IMO.Number %in% rownames(m),]
eu.ix <- match(x.eu$IMO.Number, rownames(m))

# for each numeric predictor, set 1000 random rows to NA
numeric.vars <- c('Deadweight','GrossTonnage','Length','Breadth','Draught','Powerkwmax','Powerkwaux','Speed')
ix.fake.missing <- list()
random.ix <- sample(nrow(x.eu))
count <- 1
batch.size <- 3000
# set to NA and store those indices
for(numeric.var in numeric.vars){
  random.ix.i <- random.ix[count:(count + batch.size - 1)]
  ix.fake.missing[[numeric.var]] <- random.ix.i
  imo.numbers <- x.eu$IMO.Number[random.ix.i]
  m[imo.numbers,numeric.var] <- NA
  count <- count + batch.size
}

write.csv(m, file='metadata.fake.missing.data.csv',quote=TRUE,row.names=TRUE)

# impute
m.post <- preprocess.ship.metadata('metadata.fake.missing.data.csv',
                                   metadata.output.filepath = 'metadata.fake.missing.data-impute-rf.csv',
                                   imputation.method = 'rf')

# add data to EU table
x.eu.imputed <- load.EU.MRV.ship.data.and.metadata(metadata.filepath='metadata.fake.missing.data-impute-rf.csv')

# train RF and get OOB predictions
predictor.names <- c('Deadweight','FlagNameBin','FlagNameContinent','GrossTonnage','Length','Breadth','Draught','ShiptypeEU','ShiptypeLevel2','Powerkwmax','Powerkwaux','Speed','YearOfBuild')
xoh <- model.matrix(~ ., data=x.eu.imputed[,predictor.names])[,-1]
y <- x.eu.imputed$kg.CO2.per.nm
my.rf <- my.rf.tune(xoh,y,params=list(mtry=15, nodesize=8,ntree=1000))
yhat <- my.rf$yhat

for(numeric.var in numeric.vars){
  random.ix.i <- ix.fake.missing[[numeric.var]]
  imo.numbers <- x.eu$IMO.Number[random.ix.i]
  new.ix <- match(imo.numbers, x.eu.imputed$IMO.Number)
  new.ix <- new.ix[!is.na(new.ix)]
  mae <- mean(abs(yhat[new.ix] - y[new.ix])/y[new.ix])
  cat(numeric.var,mae,'\n')
}
