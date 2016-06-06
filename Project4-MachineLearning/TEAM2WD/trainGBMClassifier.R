
trainGBMClassifier <-function(dfTrain, gbm_params, weights_opt, predictors, plot_on=FALSE){
  
  gc(reset=TRUE)
  
  # deal with weights for gbm
  if(weights_opt=='raw'){
    weights <- dfTrain$Weight
    weights <- weights/sum(weights)
  }else if(weights_opt=='balance'){
    # re-scale weights to have balanced weights
    weights <- dfTrain$Weight
    ind_pos <- which(dfTrain$Label_binary==1)
    ind_neg <- which(dfTrain$Label_binary==0)
    sum_wpos <- sum(weights[ind_pos])
    sum_wneg <- sum(weights[ind_neg])
    weights[ind_pos] <- 0.5 * (weights[ind_pos]/sum_wpos)
    weights[ind_neg] <- 0.5 * (weights[ind_neg]/sum_wneg)
  }else if(weights_opt=='none'){
    weights <- rep(1, dim(dfTrain)[1])
    weights <- weights/sum(weights)
  }
  
  # train gbm
  model <- gbm(Label_binary ~ .,
               data = dfTrain[, c(predictors, 'Label_binary')],
               distribution = gbm_params$distribution,
               weights = weights,
               n.trees = gbm_params$n.trees,
               shrinkage = gbm_params$shrinkage,
               interaction.depth = gbm_params$interaction.depth,
               n.minobsinnode = gbm_params$n.minobsinnode,
               train.fraction = gbm_params$train.fraction,
               bag.fraction = gbm_params$bag.fraction,
               cv.folds = gbm_params$cv.folds,
               class.stratify.cv = gbm_params$class.stratify.cv,
               verbose = gbm_params$verbose,
               n.cores = gbm_params$n.cores,
               keep.data = gbm_params$keep.data)
  
  # plot the error
  if(plot_on ==TRUE){
    if(gbm_params$cv.folds>1){
      best.iter <- gbm.perf(model, method="cv")
      min.cv.error <- min(model$cv.error)
      abline(h=min.cv.error, col='blue', lwd=2, lty=2)
    }    
  }
  return(model)
}