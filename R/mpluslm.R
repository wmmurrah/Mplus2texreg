# Constructor function to create mpluslm object
mpluslm <- function(target) {
  require(MplusAutomation)
  mod <- readModels(target=target,recursive=FALSE)
  estimator <- mod$summaries$Estimator
  n <- mod$summaries$Observations
  k<- (length(unique(mod$parameters$unstandardized$param))-1)
  par.tn <- mod$summaries$Parameters
  coef.names <- mod$parameters$unstandardized$param[1:k]
  coef <- mod$parameters$unstandardized$est[1:k]
  se <- mod$parameters$unstandardized$se[1:k]
  pvalues <- mod$parameters$unstandardized$pval[1:k]
  gof.names <- character()
  rs <- (1 - mod$parameters$stdyx.standardized$est[par.tn])
  adj <- (rs - (k/(n-1)))
  ll <- mod$summaries$LL
  llu <- mod$summaries$UnrestrictedLL
  aic <- mod$summaries$AIC
  bic <- mod$summaries$BIC
  new("mpluslm", names=coef.names, coef=coef, se=se, pval=pvalues, rsq=rs,adjrs=adj, n=n)
}
