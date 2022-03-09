# ridge regression within levels of a factor
# result has "final.model" which contains a list "models" of 
# ridge models indexed by category levels and a string "category" name
"train.ridge.by.category" <- function(x,y,category, verbose=0){
  # moved 'caret' loading here because it has been difficult to 
  # install on some systems
  library('caret', warn.conflicts = F, quietly = T)
  
  results <- list()
  final.model <- list()
  final.model$models <- list()
  yhat <- y
  
  # preprocess data to center and scale
  preprocessor <- preProcess(x, method = c("center", "scale"))
  final.model$preprocessor <- preprocessor
  x <- predict(preprocessor,x)
  
  for(level in levels(category)){
    if(verbose > 0) cat(level,'')
    ix <- category == level
    if(sum(ix) >= 10){
      final.model$models[[level]] <- train(x[ix,],y[ix],method='ridge',
                                           tuneLength=4,trControl=trainControl(method='cv',number=5)
                                           )
      yhat[ix] <- predict(final.model$models[[level]],x[ix,])
    } else {
      yhat[ix] <- NA
    }
  }

  # train a general model for unrecognized levels
  if(verbose > 0) cat('general')
  final.model$general.model <- train(x,y,method='ridge',
                                       tuneLength=4,trControl=trainControl(method='cv',number=5)
                                     )
  
  if(verbose > 0) cat('\n')
  results$final.model <- final.model
  results$rmse <- sqrt(mean((yhat-y)^2))
  results$mape <- 100*mean(abs(yhat-y)/y)
  results$y <- y
  results$yhat <- yhat
  return(results)
}

# lm within levels of a factor
# "final.model" which contains a list "models" of 
# ridge models indexed by category levels
"predict.ridge.by.category" <- function(final.model, newx, category){
  yhat <- numeric(nrow(newx))
  # preprocess data to scale and center
  newx <- predict(final.model$preprocessor,newx)
  for(level in levels(category)){
    ix <- category == level
    if(level %in% names(final.model$models)){
      model.i <- final.model$models[[level]] # get model for this category
    } else {
      model.i <- final.model$general.model # get model for this category
    }
    yhat[ix] <- predict(model.i,newx[ix,,drop=F])
  }
  return(yhat)
}

