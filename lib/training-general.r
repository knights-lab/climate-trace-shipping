# get path to source files from R environment
if(Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME') == ''){
  stop('R_CLIMATE_TRACE_SHIPPING_HOME not in environment variables. Please add "R_CLIMATE_TRACE_SHIPPING_HOME=/path/to/r/climate/trace/shipping/repo" to the file .Renviron in your home directory. Read more about the .Renviron file here: https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf')
}

CTHOME=Sys.getenv('R_CLIMATE_TRACE_SHIPPING_HOME')

library("optparse", warn.conflicts = F, quietly = T)
library('xgboost', warn.conflicts = F, quietly = T)
library('caret', warn.conflicts = F, quietly = T)
library('data.table', warn.conflicts = F, quietly = T)
source(paste(CTHOME,'/lib/rf.r',sep=''))
source(paste(CTHOME,'/lib/xgb.r',sep=''))
source(paste(CTHOME,'/lib/linear.r',sep=''))
source(paste(CTHOME,'/lib/ridge.r',sep=''))


# repeat entire train/test n times on random splits of data
# report performance across all splits
# do this for all requested models.
# x is a data frame with the desired predictive features
# y is the continuous outcome
# individual.ids is a optional vector of IDs of individual
#   ships. If provided, all entries for a given ship will 
#   either be in the test set or training set on each split,
#   to avoid information leak.
# linear.categories is an optional factor or character vector of 
#   category labels. If provided, linear regression will be 
#   performed _within_ each category.
# if params list is provided, it must contain values for the default params;
#   each param can be a vector of values, e.g. nodesize=5:10
#
# Return value is a list of results and chosen model params across the reps, 
# in this format:
# nreps
# train.fraction
# params.tried = list of lists of model params requested
# best.params = list indexed by model of lists of the final params chosen in that rep
# maes = mean absolute percent errors (rows are models, cols are reps)
# rmses = root mean squared errors (rows are models, cols are reps)
# y = true labels
# yhat = list indexed by model of lists of predicted values
# train.ix = lists of vectors of train indices
"tune.and.evaluate.models" <- function(x,y,
                                       nreps=5, # number of train/test splits
                                       test.fraction=1/3, # fraction in test sets
                                       models=c('rf','xgb','linear','ridge'),
                                       params=NULL, # by default, allow methods to choose their parameters to search
                                       individual.ids=NULL,
                                       linear.predictor=NULL, # predictor variable for linear models
                                       linear.categories=NULL,
                                       final.model=TRUE, # Train a final model on all data
                                       verbose=1
)
{

  # initialize return variables
  maes <- matrix(0,nrow=length(models),ncol=nreps, dimnames=list(models, sprintf('rep%d',1:nreps)))
  rmses <- matrix(0,nrow=length(models),ncol=nreps, dimnames=list(models, sprintf('rep%d',1:nreps)))
  best.params <- list()
  yhat = list()
  train.ix = list()

  for(rep.i in 1:nreps){
    if(verbose > 0) cat('\nREP',rep.i,'of',nreps,'\n\n')

    # Initialize train/test split for this rep
    train.ix[[rep.i]] <- sample(nrow(x),round((1-test.fraction)*nrow(x)))

    # if individual.ids provided,
    # and if a ship appears more than once,
    # then ensure each ship is in either train or test
    if(!is.null(individual.ids)){
      if(max(table(individual.ids)) > 1){
        if(verbose > 0) cat('Certain ships appear more than once, grouping each ship in train or test.\n')
        ship.ids <- sample(unique(individual.ids)) # get random ordering of ship ids
        train.ship.ids.ix <- sample(length(ship.ids),round((1-test.fraction)*length(ship.ids)))
        train.ix[[rep.i]] <- which(individual.ids %in% ship.ids[train.ship.ids.ix])
      }
    }
    
    train.ix.i <- train.ix[[rep.i]] # for convenience

    # Get a one-hot encoding of the data
    xoh <- model.matrix(~ ., data=x)[,-1]

    for(i in 1:length(models)){
      if(verbose > 0) cat(sprintf('Model %s\n',models[i]))
      if(rep.i == 1) {
        # if first rep, initialize empty lists for results from this model
        best.params[[models[i]]] <- list()
        yhat[[models[i]]] <- list()
      }

      # TUNE model
      if(models[i] == 'xgb'){
        res <- my.xgb.cv.tune(xoh[train.ix.i,,drop=F],y[train.ix.i],params=params[['xgb']],verbose=verbose)
        yhat.test <- predict(res$final.model,xoh[-train.ix.i,])
        best.params[['xgb']][[rep.i]] <- res$best.params
      } else if (models[i] == 'rf'){
        res <- my.rf.tune(xoh[train.ix.i,,drop=F],y[train.ix.i],params=params[['rf']],verbose=verbose)
        yhat.test <- predict(res$final.model,xoh[-train.ix.i,])
        best.params[['rf']][[rep.i]] <- res$best.params
        
      } else if (models[i] == 'linear' && !is.null(linear.predictor) && !is.null(linear.categories)){
        res.lm <- lm.by.category(linear.predictor[train.ix.i], y[train.ix.i], linear.categories[train.ix.i])
        yhat.test <- predict.lm.by.category(res.lm$final.model,linear.predictor[-train.ix.i], linear.categories[-train.ix.i])
      } else if (models[i] == 'ridge'){
        # only continuous predictors for ridge regression
        ridge.predictors <- sapply(x,class) %in% c('numeric','integer')
        xoh.ridge <- model.matrix(~ ., data=x[,ridge.predictors,drop=F])[,-1]
        res.ridge <- train.ridge.by.category(xoh.ridge[train.ix.i,,drop=F],y[train.ix.i],droplevels(as.factor(linear.categories[train.ix.i])),verbose=verbose)
        yhat.test <- predict.ridge.by.category(res.ridge$final.model, xoh.ridge[-train.ix.i,,drop=F],droplevels(as.factor(linear.categories[-train.ix.i])))
      }

      # EVALUATE performance
      y.test <- y[-train.ix.i]
      rmses[models[i],rep.i] <- sqrt(mean((y.test - yhat.test)^2))/mean(y.test)
      maes[models[i],rep.i] <- 100*mean(abs(y.test-yhat.test)/y.test)
          
      # store yhat
      yhat[[models[i]]][[rep.i]] <- yhat.test

      if(verbose > 0) cat('NRMSE =',rmses[i,rep.i],'MAE =',maes[i,rep.i],'\n')
      
      if(FALSE){
        # TO DO: add optional visualizations of feature importance
        # and obs vs expected scatterplots; example code below.
        # importance calculations
        importance_matrix <- xgb.importance(model = res.xgb$final.model)
        importance_matrix <- importance_matrix[1:min(10,max(min(nrow(importance_matrix),10),which(importance_matrix$Gain > 0.001))),drop=F]
        feature.names <- importance_matrix$Feature
        importance_matrix <- as.matrix(importance_matrix[,-1])
        rownames(importance_matrix) <- feature.names
        print(cbind(res.xgb$rmses,res.xgb$hyper.grid)[order(res.xgb$rmses),])

        pdf(sprintf('output/xgb-importance-%s.pdf',task),width=5, height=5)
        par(mar=c(5,10,2,2))
        barplot(importance_matrix[nrow(importance_matrix):1,'Gain'],
                names.arg=rownames(importance_matrix)[nrow(importance_matrix):1],
                horiz=TRUE,
                las=2,xlab = "XBG tree gain",
                cex.axis=.7,
                cex.names=.7,
                main="XBG Feature Importance",
                cex.main=1)
        dev.off()
        
        importance_matrix <- res.rf$final.model$importance
        importance_matrix <- importance_matrix[order(importance_matrix[,'IncNodePurity'],decreasing=TRUE),,drop=F][1:(min(10,nrow(importance_matrix))),,drop=F]
        
        pdf(sprintf('output/rf-importance-%s.pdf',task),width=5, height=5)
        par(mar=c(5,10,2,2))
        barplot(importance_matrix[nrow(importance_matrix):1,1],
                names.arg=rownames(importance_matrix)[nrow(importance_matrix):1],
                horiz=TRUE,
                las=2,xlab = "RF Increase in Node Purity",
                cex.axis=.7,
                cex.names=.7,
                main="RF Feature Importance",
                cex.main=1)
        dev.off()
        
        # plot scatterplot
        pdf(sprintf('output/xgb-%s.pdf',task),width=5, height=5)
        axis.lims <- range(c(y.test,yhat.test))
        plot(y.test,yhat.test,pch=21,col=NA,bg='#00000011',main=sprintf('XGB Test \n(NRMSE %0.3f, MAE %0.1f)',nrmse, mae),las=2,cex.axis=.75,xlim=axis.lims,ylim=axis.lims,xlab='Reported kg CO2/nm',ylab='Predicted kg CO2/nm'); abline(0,1)
        dev.off()
        
        pdf(sprintf('output/xgb-mae-by-shiptype-%s.pdf',task), width=7,height=5)
        par(mar=c(7.1, 4.1, 4.1, 2.1))
        abs.pct.err <- abs(y.test-yhat.test)/y.test
        boxplot(100*abs.pct.err ~ x[rowix,,drop=F][-train.ix.i,'shiptype3'],las=2,cex.axis=.5, ylab='Absolute percent error')
        dev.off()
        
        # plot per-category linear fits
        pdf(sprintf('output/lm.scatterplots.by.shiptype-%s.pdf',task),width=9,height=7)
        plot.lm.by.category(x$deadweight[rowix], x$kg.CO2.per.nm[rowix], x$shiptype.original[rowix])
        dev.off()
        
      }
      
    }
  }
  if(verbose > 0) cat('Generating final model...\n')
  
  if(final.model){
    # if requested, train final model on all data using best params
    # Only works with one model at a time
    if(length(models) > 1) stop('Error: final model can only be requested for one model type at a time')

    if(model %in% c('rf','xgb')){
      # get this model's params from all reps in a matrix
      params.mat <- matrix(unlist(best.params[[models[1]]]), ncol=nreps)
      # choose the median value of each param
      # round to nearest integer if none of the values have decimal components
      final.params <- apply(params.mat, 1, function(xx) if(any(round(xx) != xx)) mean(xx) else round(median(xx)))
      final.params <- as.list(final.params)
      names(final.params) <- names(best.params[[models[1]]][[1]])
    }
    if(models[1] == 'rf'){
      final.model <- my.rf.tune(xoh,y,params=final.params,verbose=verbose)$final.model
    } else if(models[1] == 'xgb'){
      final.model <- my.xgb.cv.tune(xoh,y,params=final.params,verbose=verbose)$final.model
    } else if(models[1] == 'linear' && !is.null(linear.predictor) && !is.null(linear.categories)){
      final.model <- lm.by.category(linear.predictor, y, linear.categories)$final.model
    } else if (models[i] == 'ridge'){
      # only continuous predictors for ridge regression
      ridge.predictors <- sapply(x,class) %in% c('numeric','integer')
      xoh.ridge <- model.matrix(~ ., data=x[,ridge.predictors,drop=F])[,-1]
      final.model <- train.ridge.by.category(xoh.ridge,y,linear.categories,verbose=verbose)$final.model
    }
  } else{
    final.model <- NULL
  }
  
  return(list(nreps=nreps,
              test.fraction=test.fraction,
              params.tried = params,
              best.params = best.params,
              maes = maes,
              rmses = rmses,
              y = y,
              yhat = yhat,
              train.ix = train.ix,
              final.model = final.model))
}



# Predict emissions using saved model
# 
# return value is a vector of predicted kg CO2/nm
"predict.from.saved.model" <- function(newx,
                                       final.model.container,
                                       verbose=1
)
{

  train.x <- final.model.container$train.x
  model <- final.model.container$final.model
  model.type <- final.model.container$model.type
  
  # Get a one-hot encoding of the data
  # use the original training data to ensure proper formatting of 
  # new model matrix
  predictor.names <- colnames(train.x)
  combined.x <- rbind(train.x, newx[,colnames(train.x)])
  xoh <- model.matrix(~ ., data=combined.x)[,-1,drop=F]
  
  # remove original data from one-hot encoding, 
  # leaving only new data
  xoh <- xoh[-(1:nrow(train.x)),,drop=F]

  if(model.type %in% c('xgb','rf')){
    yhat <- predict(model,xoh)
  } else if (model.type == 'linear' && !is.null(linear.predictor) && !is.null(linear.categories)){
    # NOT IMPLEMENTED
    # need to encode linear predictor name etc. in model
    stop('Prediction from linear-by-category model not implemented.')
    yhat <- predict.lm.by.category(model,linear.predictor, linear.categories)
  } else if (model.type == 'ridge'){
    # NOT IMPLEMENTED
    # need to encode linear category variable in model
    stop('Prediction from ridge-regression-by-category model not implemented.')
    ridge.predictors <- sapply(x,class) %in% c('numeric','integer')
    yhat <- predict.ridge.by.category(model,xoh.ridge,linear.categories)
  }
  return(yhat)  
}


