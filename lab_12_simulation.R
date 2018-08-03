generate_data = function(n, p){
  n-by-p = matrix(rnorm(n * p, 0, 1), n, p)
  r = vector(rnorm(n, 0, 1))
  return(list(covariates = n-by-p, responses = r))
}

model_select = function(covariates, responses, cutoff){
  regression = lm(responses ~ covariates)
  new.cov = covariates[which(coef(summary(regression))[-1, 4] <= cutoff)]
  if (length(new.cov) == 0) {return(vector(length = 0))}
  else{
    regression.new = lm(responses ~ new.cov)
    return(coef(summary(regression.new))[, 4])
  }
}