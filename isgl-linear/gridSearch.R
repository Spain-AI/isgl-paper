# Author: JC Laria 
# Grid Search and Random Search SGL
#==================================

gridSearch = function(data.train, data.validate, group.length, type="linear",
           lambda1.grid = 2^seq(-8, 1, length.out = 30),
           lambda2.grid = 2^seq(-8, 1, length.out = 30)){
  
  lambdas = expand.grid(lambda1.grid, lambda2.grid)
  
  best_cost = Inf
  best_fit = NULL
  for (point in 1:nrow(lambdas)) {
      sgl.fit = sglfast::solve_inner_problem(data.train, group.length,
                                             lambdas = lambdas[point,],
                                             type=type,
                                             simple = T)
      if (type=="linear") {
        y.pred = sgl.fit$intercept + data.validate$x%*%sgl.fit$beta
        cost = mean((y.pred-data.validate$y)^2)
      }else{
        y.pred = sgl.fit$intercept + data.validate$x%*%sgl.fit$beta
        y.pred = ((1 + exp( -y.pred ))^(-1) > 0.5 )+0
        cost = mean(y.pred!=data.validate$y)
      }
      
      if (cost < best_cost) {
        best_fit = sgl.fit
        best_cost = cost
      }
  }
  return(best_fit)
}

randomSearch = function(data.train, data.validate, group.length, type="linear",
             npoints = 900 ){
  
  r = 2^seq(-8, 1, length.out = npoints)
  theta = runif(npoints, 0, pi/2)  
  lambdas = data.frame( lambda1 = r*cos(theta), lambda2 = r*sin(theta))
  
  best_cost = Inf
  best_fit = NULL
  for (point in 1:nrow(lambdas)) {
    sgl.fit = sglfast::solve_inner_problem(data.train, group.length,
                                           lambdas = lambdas[point,],
                                           type=type,
                                           simple = T)
    if (type=="linear") {
      y.pred = sgl.fit$intercept + data.validate$x%*%sgl.fit$beta
      cost = mean((y.pred-data.validate$y)^2)
    }else{
      y.pred = sgl.fit$intercept + data.validate$x%*%sgl.fit$beta
      y.pred = ((1 + exp( -y.pred ))^(-1) > 0.5 )+0
      cost = mean(y.pred!=data.validate$y)
    }
    
    if (cost < best_cost) {
      best_fit = sgl.fit
      best_cost = cost
    }
  }
  return(best_fit)
}

call_GS = function(expert_num_groups, run, type="linear"){
  X = as.matrix(read.table(paste0("data/X_train-run",run)))
  y = as.matrix(read.table(paste0("data/y_train-run",run)))
  data.train = list(x=X, y=y)
  
  X = as.matrix(read.table(paste0("data/X_validate-run",run)))
  y = as.matrix(read.table(paste0("data/y_validate-run",run)))
  data.validate = list(x=X, y=y)

  group.length = rep(ncol(X)/expert_num_groups, expert_num_groups)
  
  
  time = system.time(fit <- gridSearch(data.train, 
                                       data.validate, 
                                       group.length, type))
  
  save(time, fit, file = paste0("results/GS/fit",run,".RData"))
}

call_RS = function(expert_num_groups, run, type="linear"){
  X = as.matrix(read.table(paste0("data/X_train-run",run)))
  y = as.matrix(read.table(paste0("data/y_train-run",run)))
  data.train = list(x=X, y=y)
  
  X = as.matrix(read.table(paste0("data/X_validate-run",run)))
  y = as.matrix(read.table(paste0("data/y_validate-run",run)))
  data.validate = list(x=X, y=y)
  
  group.length = rep(ncol(X)/expert_num_groups, expert_num_groups)
  
  
  time = system.time(fit <- randomSearch(data.train, 
                                       data.validate, 
                                       group.length, type))
  
  save(time, fit, file = paste0("results/RS/fit",run,".RData"))
}

# X = matrix(rnorm(200), nrow = 10)
# y = X[,1:5]%*%(1:5) + rnorm(10, 0, 0.45)
# data.train = list(x=X, y=y)
# group.length = rep(4,5)
# X = matrix(rnorm(200), nrow = 10)
# y = X[,1:5]%*%(1:5) + rnorm(10, 0, 0.45)
# data.validate = list(x=X, y=y)
# type = "linear"
