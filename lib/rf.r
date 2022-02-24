suppressMessages(suppressWarnings(library('randomForest', warn.conflicts = F, quietly = T)))

# Runs rf CV to tune params
# returns hyperparameter table, RMSEs, Pearson correlations,
# and final model trained on all data using lowest RMSE
# tunes with OOB
# tunes ntree within each model by subsampling trees
# assumes regression; classification not implemented
# params can be a named list of params like nodesize, etc.,
# if so, these will override the default values for the provided params
"my.rf.tune" <- function(x, y,
                        mtry=round(ncol(x)/3),
                        nodesize=3:10,
                        ntree=500,
                        params=NULL,
                        verbose=0) {
  
  # https://www.r-bloggers.com/2020/11/r-xgboost-regression/
  # create hyperparameter grid
  # override values with params list if provided
  if(!is.null(params)){
    if('nodesize' %in% names(params)) nodesize <- params$nodesize
    if('mtry' %in% names(params)) mtry <- params$mtry
    if('ntree' %in% names(params)) ntree <- params$ntree
  }
  hyper.grid <- expand.grid(mtry=mtry,
                       nodesize=nodesize)
  rmses <- numeric(nrow(hyper.grid))
  
  print(hyper.grid)
  # validation
  if(length(ntree) > 1) stop('Error: rf ntree can only take one value.')

    # more than one hyperparameter set, need to tune
  if(nrow(hyper.grid) > 1) { 
    
    # keep all predictions
    yhat <- list()
    for(i in 1:nrow(hyper.grid)){
      mtry <- hyper.grid$mtry[i]
      nodesize <- hyper.grid$nodesize[i]
      if(verbose > 0) cat('mtry =',mtry,'nodesize =',nodesize,'')
      
      m.rf <- randomForest(x, y,
                           mtry=mtry,
                           nodesize=nodesize,
                           ntree=ntree)
      yhat[[i]] <- m.rf$predicted
      rmses[i] <- sqrt(m.rf$mse[ntree])
      if(verbose > 0) cat(rmses[i],'\n')
    }
    if(verbose > 0) cat('\n')
    best.ix <- which.min(rmses)
    best.rmse <- min(rmses)
    best.mtry <- hyper.grid$mtry[best.ix]
    best.nodesize <- hyper.grid$nodesize[best.ix]
    
    # train one last time
    final.model <-     m.rf <- randomForest(x, y,
                                            mtry=best.mtry,
                                            nodesize=best.nodesize,
                                            ntree=ntree)
  } else {
    best.mtry <- mtry
    best.nodesize <- nodesize
    best.ix <- 1
    # train one model
    final.model <-     m.rf <- randomForest(x, y,
                                            mtry=best.mtry,
                                            nodesize=best.nodesize,
                                            ntree=ntree)
    yhat <- m.rf$predicted
    yhat <- list(yhat=yhat)
    rmses <- sqrt(m.rf$mse[ntree])
    best.rmse <- rmses
    if(verbose > 0) cat(rmses,'\n')
  }

  return(list(rmses=rmses,
              hyper.grid=hyper.grid,
              best.rmse=best.rmse,
              best.params=list(mtry=best.mtry,
                               nodesize=best.nodesize,
                               ntree=ntree),
              final.model=final.model,
              y=y,
              yhat=yhat[[best.ix]]
              ))
}
