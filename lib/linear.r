# lm within levels of a factor
# result has "final.model" which contains a list "models" of 
# lm models indexed by category levels and a string "category" name
"lm.by.category" <- function(x,y,category){
  results <- list()
  final.model <- list()
  final.model$models <- list()
  category <- droplevels(as.factor(as.character(category)))
  yhat <- y
  for(level in levels(category)){
    ix <- category == level
    if(sum(ix) >= 3){
      final.model$models[[level]] <- lm(y[ix] ~ x[ix])
      yhat[ix] <- final.model$models[[level]]$fitted.values
    }
  }
  # make one general model for predicting unrecognized categories
  final.model$general.model <- lm(y ~ x)
  
  results$final.model <- final.model
  results$rmse <- sqrt(mean((yhat-y)^2))
  results$mape <- 100*mean(abs(yhat-y)/y)
  results$y <- y
  results$yhat <- yhat
  return(results)
}

# lm within levels of a factor
# "final.model" which contains a list "models" of 
# lm models indexed by category levels
"predict.lm.by.category" <- function(final.model, newx, category){
  yhat <- numeric(length(newx))
  category <- droplevels(as.factor(as.character(category)))
  for(level in levels(category)){
    ix <- category == level
    if(level %in% names(final.model$models)){
      # this level is in the list of models
      model.i <- final.model$models[[level]] # get model for this category
    }
    else{
      # this level is not in the list of models
      model.i <- final.model$general.model # get general model
    }
    intercept <- model.i$coefficients[1]
    slope <- model.i$coefficients[2]
    yhat[ix] <- slope * newx[ix] + intercept
  }
  return(yhat)
}

"plot.lm.by.category" <- function(x,y,category,x.name='Deadweight Tons',y.name='KG CO2/NM'){
  par(mfrow=c(3,5),mar=c(4,4,5,.4))
  yhat <- y
  category <- droplevels(as.factor(as.character(category)))
  for(level in levels(category)){
    ix <- category == level
    if(sum(ix) >= 5){
      linearmodel <- lm(y[ix] ~ x[ix])
      yhat[ix] <- linearmodel$fitted.values
      rmse <- sqrt(mean((yhat[ix]-y[ix])^2))
      alpha.char <- '22'
      if(sum(ix) < 100 ) alpha.char <- '55'
      plot(x[ix],y[ix],
           pch=21,col=NA,
           bg=sprintf('#000000%s',alpha.char),
           xlab=x.name,
           ylab=y.name,
           main=sprintf('%s \n(RMSE %0.3f)',level, rmse),
           cex.main=1.1,
           cex.lab=0.75)
      abline(linearmodel)
    }
  }
}
