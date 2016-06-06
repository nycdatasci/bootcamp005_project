####### The AMS function defined according to the evaluation page on the website
AMS <- function(real,pred,weight)
{
  pred_s_ind = which(pred=="s")                          # Index of s in prediction
  real_s_ind = which(real=="s")                          # Index of s in actual
  real_b_ind = which(real=="b")                          # Index of b in actual
  s = sum(weight[intersect(pred_s_ind,real_s_ind)])      # True positive rate
  b = sum(weight[intersect(pred_s_ind,real_b_ind)])      # False positive rate
  b_tau = 10                                             # Regulator weight
  ans = sqrt(2*((s+b+b_tau)*log(1+s/(b+b_tau))-s))
  return(ans)
}


####### Using AMS function defined above as a metrics function for caret
####### Check the details here: http://topepo.github.io/caret/training.html#metrics
AMS_summary <- function(data, lev = NULL, model = NULL){
  out = (AMS(data$obs, data$pred, data$weights))
  names(out) <- "AMS"
  return(out)
}

##################################################################################
# define some helper and reporting functions
# calulcate area under the curve of numeric vectors x,y
# length(x)==length(y)
# y>=0, 0<=x<=1 and x increasing

plotROC <- function(title,outcol,predcol) {
  pred <- prediction(predcol,outcol)
  perf <- performance(pred,'tpr','fpr')
  auc <- as.numeric(performance(pred,'auc')@y.values)
  pf <- data.frame(
    FalsePositiveRate=perf@x.values[[1]],
    TruePositiveRate=perf@y.values[[1]])
  plot=ggplot() +
    geom_ribbon(data=pf,aes(x=FalsePositiveRate,ymax=TruePositiveRate,ymin=0),
                fill='blue',alpha=0.3) +
    geom_point(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
    geom_line(aes(x=c(0,1),y=c(0,1))) + coord_fixed() +
    ggtitle(paste(title,'\nAUC:',format(auc,digits=2)))
  list(pf=pf,plot=plot)
}


deviance <- function(truth,pred,epsilon=0) {
  pred = pmax(pred, epsilon)
  pred = pmin(pred, 1-epsilon)
  S = 0.0 # assumed log-likelihood of saturated model
  -2*(sum(ifelse(truth,log(pred),log(1-pred)))-S)
}


reportStats <- function(d,test,modelName,title,epsilon) {
  dSub <- d[d$isTest==test,,drop=FALSE]
  tab <- table(truth=dSub[,yColumn],pred=dSub[,modelName]>0.5)
  accuracy <- (tab[1,1] + tab[2,2])/sum(tab)
  note = ifelse(test,'test','train')
  print(paste('\t',note,'AMS',modelName,format(AMS,digits=2)))
  print(paste('\t',note,'accuracy',modelName,format(accuracy,digits=2)))
  residual.deviance <- deviance(dSub[,yColumn],dSub[,modelName],epsilon)
  #print(paste('\tresidual.deviance',residual.deviance))
  null.deviance <- deviance(dSub[,yColumn],mean(dSub[,yColumn]),epsilon)
  #print(paste('\tnull.deviance',null.deviance))
  print(paste("\tmodel explained a",
              format((1-residual.deviance/null.deviance),digits=2),
              "fraction of the variation on",note))  
}

###################################

func_compute_AUC <- function(labels, scores, plotROC=FALSE){
  if(plotROC==TRUE){
    par(mfrow=c(1,1))
    auc <- colAUC(scores, labels, plotROC=TRUE, alg='ROC')
    auc <- as.numeric(auc)
  }else{
    x1 = scores[labels==1]; n1 = length(x1)
    x2 = scores[labels==0]; n2 = length(x2)
    r = rank(c(x1, x2))
    auc = (sum(r[1:n1]) - n1*(n1+1)/2)/n1/n2
  }
  cat('AUC: ', auc, '\n', sep='')
  return(auc)
}

func_compute_AMS_cutoffs <- function(y_true_label, y_pred_prob, weight, cutoffs){
  #### This function computes (AMS) for a single cutoff
  func_compute_AMS_single_cutoff <- function(y_true_label, y_pred_prob, weight, cutoff){
    RankOrder <- rank(y_pred_prob, ties.method='random')
    top <- as.integer(floor(cutoff * length(y_pred_prob)))
    thresh <- y_pred_prob[which(RankOrder==top)]
    y_pred_label <- ifelse(y_pred_prob>thresh, 's', 'b')
    r <- func_compute_s_b(y_true_label, y_pred_label, weight)
    s <- r[[1]]
    b <- r[[2]]
    ams <- func_compute_AMS(s, b)
    return(ams)
  }
  
  ## We now compute f1-score for varying cutoffs
  AMS <- sapply(cutoffs, function(cutoff)
    func_compute_AMS_single_cutoff(y_true_label, y_pred_prob, weight, cutoff))
  return(AMS)
}
