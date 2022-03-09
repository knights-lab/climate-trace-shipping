library('xgboost', warn.conflicts = F, quietly = T)

# Runs xbg CV to tune params
# returns hyperparameter table, RMSEs, Pearson correlations,
# and final model trained on all data using lowest RMSE
"my.xgb.cv.tune" <- function(x, y,nfolds=5,
                        # max_depth = c(3, 4, 5, 6),
                        # eta = c(.05, .1, .2, .3),
                        # subsample = c(0.25,0.5,.75,1),
                        # nrounds=c(100,200,500,1000),
                        max_depth = c(3, 4),
                        eta = c(.05, .1),
                        subsample = c(0.25,0.5),
                        nrounds=c(10,20),
                        params=NULL,
                        verbose=0) {
  # only load xgboost if needed for xgb
  # this is for faster runtime
  # and fewer dependencies for basic use case
  library('xgboost', warn.conflicts = F, quietly = T)
  
  # https://www.r-bloggers.com/2020/11/r-xgboost-regression/
  #create hyperparameter grid
  # override values with params list if provided
  if(!is.null(params)){
    if('max_depth' %in% names(params)) max_depth <- params$max_depth
    if('eta' %in% names(params)) eta <- params$eta
    if('subsample' %in% names(params)) subsample <- params$subsample
    if('nrounds' %in% names(params)) nrounds <- params$nrounds
  }
  
  hyper.grid <- expand.grid(max_depth = max_depth,
                            eta = eta,
                            subsample = subsample,
                            nrounds = nrounds)
  
  cors <- numeric(nrow(hyper.grid))
  rmses <- numeric(nrow(hyper.grid))
  fold.ix <- sample(rep(1:nfolds,length=nrow(x)))
  yhat <- y
  
  # if only one value of each hyper parameter provided,
  # Skip tuning and just train once on all data
  if(nrow(hyper.grid) == 1){
    best.ix <- 1
    # Train once on whole set using best params
    m.xbg <- xgboost(data = x, 
                     label = y,
                     objective = "reg:squarederror", 
                     nrounds=hyper.grid[1,'nrounds'],
                     eta=hyper.grid[1,'eta'],
                     subsample=hyper.grid[1,'subsample'],
                     max_depth=hyper.grid[1,'max_depth'],
                     verbose = FALSE)
    yhat <- predict(m.xbg,x)
  } else {
    if(verbose > 0) cat('Of', nrow(hyper.grid), 'iterations: ')
    for(i in 1:nrow(hyper.grid)){
      if(verbose > 0) cat(i,'')
      for(k in 1:nfolds){
        in.fold <- fold.ix == k
        max_depth <- 
          eta <- hyper.grid[i,'eta']
        subsample <- hyper.grid[i,'subsample']
        nrounds <- hyper.grid[i,'nrounds']
        
        m.xbg <- xgboost(data = x[in.fold,], 
                         label = y[in.fold],
                         objective = "reg:squarederror", 
                         nrounds=hyper.grid[i,'nrounds'],
                         eta=hyper.grid[i,'eta'],
                         subsample=hyper.grid[i,'subsample'],
                         max_depth=hyper.grid[i,'max_depth'],
                         verbose = FALSE)
        
        yhat[!in.fold] <- predict(m.xbg, x[!in.fold,])
      }
      cors[i] <- cor(yhat,y)
      rmses[i] <- sqrt(mean((yhat - y)^2))
    }
    if(verbose > 0) cat('\n')
    best.ix <- which.min(rmses)
    
    # Train once on whole set using best params
    m.xbg <- xgboost(data = x, 
                     label = y,
                     objective = "reg:squarederror", 
                     nrounds=hyper.grid[best.ix,'nrounds'],
                     eta=hyper.grid[best.ix,'eta'],
                     subsample=hyper.grid[best.ix,'subsample'],
                     max_depth=hyper.grid[best.ix,'max_depth'],
                     verbose = FALSE)
  }
  best.nrounds   <- hyper.grid$nrounds[  best.ix]
  best.eta       <- hyper.grid$eta[      best.ix]
  best.subsample <- hyper.grid$subsample[best.ix]
  best.max_depth <- hyper.grid$max_depth[best.ix]
  
  yhat.fitted <- predict(m.xbg,x)
  rmse.fitted <- sqrt(mean((yhat.fitted - y)^2))
  mape.fitted <- 100*mean(abs(yhat.fitted - y)/y)
  mape.cv <- 100*mean(abs(yhat - y)/y)
  if(nfolds==1) rmses = rmse.fitted
  
  return(list(hyper.grid=hyper.grid,
              cors=cors,
              rmses=rmses,
              rmse.cv=min(rmses),
              rmse=rmse.fitted,
              final.model=m.xbg,
              y=y,
              yhat.cv=yhat,
              yhat=yhat.fitted,
              mape.cv=mape.cv,
              mape=mape.fitted,
              best.params=list(nrounds=best.nrounds,
                               eta=best.eta,
                               subsample=best.subsample,
                               max_depth=best.max_depth)
              )
         )
}
