generate_data = function(n, p){
  n-by-p = matrix(rnorm(n * p, 0, 1), n, p)
  r = vector(rnorm(n, 0, 1))
  return(list(covariates = n-by-p, responses = r))
}