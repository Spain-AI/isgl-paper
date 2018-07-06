# Author: JC Laria

train_size=90
validate_size=60
num_features=480
true_num_groups=1
expert_num_groups=60
num_runs=50

write.table(expert_num_groups, file="results/expert_num_groups",
            row.names = F, col.names = F)

generate.data = function(nobs, num_features, true_num_groups){

  true_betas = rep(c(1:5, rep(0, num_features/true_num_groups-5)), times = true_num_groups)

  X = matrix(rnorm(nobs*num_features), nrow=nobs)

  # se genera el predictor lineal
  y = X%*%true_betas
  p = exp(y)/(1+exp(y))
  y = rbinom(length(p),1, p)

  return(
    list(x=X, y=y)
  )
}

for( run in 1:num_runs ){
  data.train = generate.data(train_size, num_features, true_num_groups)
  data.validate = generate.data(validate_size, num_features, true_num_groups)
  data.test = generate.data(200, num_features, true_num_groups)

  save(data.train, data.validate, file = paste0("data/train_run", run,".RData"))
  save(data.test, file = paste0("data/test_run", run, ".RData"))
}
