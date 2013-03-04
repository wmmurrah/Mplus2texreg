# extract.mpluslm
extract.mpluslm <- function(model) {
  tr <- createTexreg(
    coef.names=model@names, 
    coef=model@coef, 
    se=model@se, 
    pvalues=model@pval, 
    gof.names=c("Num obs.", "R^2","Adj. R^2"), 
    gof=c(model@n, model@rsq, model@adjrs), 
    gof.decimal=c(FALSE, TRUE, TRUE)
  )
  return(tr)
}