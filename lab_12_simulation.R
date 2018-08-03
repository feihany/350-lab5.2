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

run_simulation = function(n_trials = 1000, n, p, cutoff = 0.05){
  p_values = replicate(n_trials, {model_select(generate_data(n, p)$covariates, 
                                               generate_data(n, p)$responses, 
                                               cutoff = cutoff)})
  
  jpeg(file = paste0("hist_pvalues", n, "_", p, ".jpeg"))
  hist(unlist(p_values))
  dev.off()
}



n = c(100, 1000, 10000)
p = c(10, 20, 50)
n_trials = 1000
cutoff = 0.05

comb = expand.grid(n, p)
names(comb) = c("n", "p")
par.mfrow = c(3, 3)
for(i in 1:nrow(comb)){
  run_simulation(n_trials, comb[i, 1], comb[i, 2], cutoff = 0.05)
}