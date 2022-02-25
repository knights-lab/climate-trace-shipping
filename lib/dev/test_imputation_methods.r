library('RColorBrewer')
library('mice', warn.conflicts = F, quietly = T)

# get path to source files from R environment
if(Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME') == ''){
  stop('R_CLIMATE_TRACE_SHIPPING_HOME not in environment variables. Please add "R_CLIMATE_TRACE_SHIPPING_HOME=/path/to/r/climate/trace/shipping/repo" to the file .Renviron in your home directory. Read more about the .Renviron file here: https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf')
}
CTHOME=Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME')
source(paste(CTHOME,'/lib/load.r',sep=''))

# x <- load.generic.ship.data.and.metadata(paste(CTHOME,'/data/EU MRV data 18-19-20.csv',sep=''),imputation.method='none')
# # drop NA rows
# x <- x[!is.na(x$powerkwaux),]
# y <- x$powerkwaux # save true values

nimp <- 3 # n imputations
maxit.opts <- c(1,2,5,10,20) # max iterations

cormat <- matrix(0,nrow=nimp, ncol=length(maxit.opts)) # matrix of correlation performance
rownames(cormat) <- sprintf('nimp%03d',1:nimp)
colnames(cormat) <- sprintf('maxiter%03d',maxit.opts)

nreps <- 1 # n train/test splits
for(i in 1:nreps){
  # fraction training data
  train.frac <- 2/3
  train.ix <- sample(1:nrow(x),round(train.frac*nrow(x)))
  
  for(m in 1:length(maxit.opts)){
    maxit.i <- maxit.opts[m]
    
    # artificially set test samples' powerkwaux to NA
    x[-train.ix,'powerkwaux'] <- NA
    yfix <- mice(x[,colnames(x) != "IMO.Number"],m=nimp,maxit=maxit.i,method='rf') 
    na.ix <- which(is.na(x[,'powerkwaux']))
    
    for(j in 1:nimp){
      cat(j,'')
      yhat <- y # will hold training and fixed values
      # get rowmeans using 100 random subsets of size j of the imputation columns
      for(k in 1:100){
        col.ix <- sample(nimp, j)
        yhat[na.ix] <- rowMeans(yfix$imp[['powerkwaux']][,col.ix,drop=F])
        
        cormat[j,m] <- cormat[j,m,drop=F] + cor(y[na.ix], yhat[na.ix])
      }
      cormat[j,m] <- cormat[j,m,drop=F] / 100
    }
    cat('\n')
  }
}

# plot 
cols <- colorRampPalette(c('gray','blue'))(ncol(cormat))
plot(0,0,type='n',xlim=c(0,nimp),ylim=range(cormat))
for(i in 1:ncol(cormat)) lines(1:nimp, cormat[,i,drop=F],col=cols[i],lwd=2)
legend('topleft',colnames(cormat),col=cols[1:ncol(cormat)],lty=1,lwd=2)
