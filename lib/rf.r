library('randomForest')

# Runs rf CV to tune params
# returns hyperparameter table, RMSEs, Pearson correlations,
# and final model trained on all data using lowest RMSE
# tunes with OOB
# tunes ntree within each model by subsampling trees
# assumes regression; classification not implemented
"my.rf.tune" <- function(x, y,
                        mtry=ncol(x)/3,
                        nodesize=3:10,
                        maxnodes=NULL,
                        ntree=500) {
  
  # https://www.r-bloggers.com/2020/11/r-xgboost-regression/
  #create hyperparameter grid
  hyper.grid <- expand.grid(mtry=mtry,
                       nodesize=nodesize,
                       maxnodes=if(is.null(maxnodes)) NA else maxnodes)
  rmses <- numeric(nrow(hyper.grid))
  if(length(rmses) > 1) { # more than one hyperparameter set, need to tune
    
    # keep all predictions
    yhat <- list()
    cat('ntree =',ntree,'')
    for(i in 1:nrow(hyper.grid)){
      mtry <- hyper.grid$mtry[i]
      nodesize <- hyper.grid$nodesize[i]
      maxnodes <- if(is.na(hyper.grid$maxnodes[i])) NULL else hyper.grid$maxnodes[i]
      cat('mtry =',mtry,'nodesize =',nodesize,'maxnodes =',maxnodes,'')
      m.rf <- randomForest(x, y,
                           mtry=mtry,
                           nodesize=nodesize,
                           maxnodes=maxnodes,
                           ntree=ntree)
      yhat[[i]] <- m.rf$predicted
      rmses[i] <- sqrt(m.rf$mse[ntree])
      cat(rmses[i],'\n')
    }
    cat('\n')
    best.ix <- which.min(rmses)
    best.rmse <- min(rmses)
    best.mtry <- hyper.grid$mtry[best.ix]
    best.nodesize <- hyper.grid$nodesize[best.ix]
    best.maxnodes <- hyper.grid$maxnodes[best.ix]
    # train one last time
    final.model <-     m.rf <- randomForest(x, y,
                                            mtry=best.mtry,
                                            nodesize=best.nodesize,
                                            maxnodes=best.maxnodes,
                                            ntree=ntree)
  } else {
    best.mtry <- mtry
    best.nodesize <- nodesize
    best.maxnodes <- maxnodes
    best.ix <- 1
    # train one model
    final.model <-     m.rf <- randomForest(x, y,
                                            mtry=best.mtry,
                                            nodesize=best.nodesize,
                                            maxnodes=best.maxnodes,
                                            ntree=ntree)
    yhat <- m.rf$predicted
    yhat <- list(yhat=yhat)
    rmses <- sqrt(m.rf$mse[ntree])
    best.rmse <- rmses
    cat(rmses,'\n')
  }

  cat('Best mtry =',best.mtry,"nodesize =",best.nodesize,"maxnodes =",best.maxnodes,"ntree =",ntree,'\n')
  
  return(list(rmses=rmses,
              hyper.grid=hyper.grid,
              best.rmse=best.rmse,
              best.mtry=best.mtry,
              best.nodesize=best.nodesize,
              best.maxnodes=best.maxnodes,
              ntree=ntree,
              final.model=final.model,
              y=y,
              yhat=yhat[[best.ix]]
              ))
}
