# Author: JC Laria 
true_num_groups = 3
expert_num_groups = as.integer(read.table("results/expert_num_groups"))
algorithms = c("HC", "HC0", "iSGL", "iSGL0", "NM", "GS", "RS")

num_runs = length(dir("data/"))/6

runtime = matrix(0, nrow = num_runs, ncol = length(algorithms))
colnames(runtime) = algorithms
validate_error = test_error = beta_error = beta_fpr = beta_fnr =
group_fpr = group_fnr = runtime


for (run in 0:(num_runs-1)) {
  
  X = as.matrix(data.table::fread(input = paste0("data/X_train-run",run)))
  y = as.matrix(read.table(paste0("data/y_train-run",run)))
  data.train = list(x=X, y=y)
  
  X = as.matrix(data.table::fread(input = paste0("data/X_validate-run",run)))
  y = as.matrix(read.table(paste0("data/y_validate-run",run)))
  data.validate = list(x=X, y=y)
  
  X = as.matrix(data.table::fread(input=paste0("data/X_test-run",run)))
  y = as.matrix(read.table(paste0("data/y_test-run",run)))
  data.test = list(x=X, y=y)
  
  for(algo in algorithms){
    switch (algo,
      iSGL = {
        path = paste0("results/",algo,"/")
        load(paste0(path, "fit", run, ".RData"))
        runtime[run+1, algo] = time["elapsed"] 
        
        y.pred = sglfast::predict.isgl(isgl.fit, data.validate$x)
        validate_error[run+1, algo] = mean((y.pred-data.validate$y)^2)
        
        y.pred = sglfast::predict.isgl(isgl.fit, data.test$x)
        test_error[run+1, algo] = mean((y.pred-data.test$y)^2)
        
        beta = isgl.fit$beta
      },
      iSGL0 = {
        path = paste0("results/",algo,"/")
        load(paste0(path, "fit", run, ".RData"))
        runtime[run+1, algo] = time["elapsed"] 
        
        y.pred = sglfast::predict.isgl(isgl.fit, data.validate$x)
        validate_error[run+1, algo] = mean((y.pred-data.validate$y)^2)
        
        y.pred = sglfast::predict.isgl(isgl.fit, data.test$x)
        test_error[run+1, algo] = mean((y.pred-data.test$y)^2)
        
        beta = isgl.fit$beta
      },
      GS = {
        path = paste0("results/",algo,"/")
        load(paste0(path, "fit", run, ".RData"))
        runtime[run+1, algo] = time["elapsed"] 
        
        y.pred = data.validate$x%*%fit$beta + fit$intercept
        validate_error[run+1, algo] = mean((y.pred-data.validate$y)^2)
        
        y.pred = data.test$x%*%fit$beta + fit$intercept
        test_error[run+1, algo] = mean((y.pred-data.test$y)^2)
        
        beta = fit$beta
      },
      RS = {
        path = paste0("results/",algo,"/")
        load(paste0(path, "fit", run, ".RData"))
        runtime[run+1, algo] = time["elapsed"] 
        
        y.pred = data.validate$x%*%fit$beta + fit$intercept
        validate_error[run+1, algo] = mean((y.pred-data.validate$y)^2)
        
        y.pred = data.test$x%*%fit$beta + fit$intercept
        test_error[run+1, algo] = mean((y.pred-data.test$y)^2)
        
        beta = fit$beta
      },
      HC = {
        path = paste0("results/",algo,"/")
        time = as.numeric(read.table(paste0(path, "time", run)))
        runtime[run+1, algo] = time
        
        beta = as.numeric(read.table(paste0(path, "beta", run))[[1]])
        
        y.pred = data.validate$x%*%beta
        validate_error[run+1, algo] = mean((y.pred-data.validate$y)^2)
        
        y.pred = data.test$x%*%beta
        test_error[run+1, algo] = mean((y.pred-data.test$y)^2)
      },
      HC0 = {
        path = paste0("results/",algo,"/")
        time = as.numeric(read.table(paste0(path, "time", run)))
        runtime[run+1, algo] = time
        
        beta = as.numeric(read.table(paste0(path, "beta", run))[[1]])
        
        y.pred = data.validate$x%*%beta
        validate_error[run+1, algo] = mean((y.pred-data.validate$y)^2)
        
        y.pred = data.test$x%*%beta
        test_error[run+1, algo] = mean((y.pred-data.test$y)^2)
      },
      NM = {
        path = paste0("results/",algo,"/")
        time = as.numeric(read.table(paste0(path, "time", run)))
        runtime[run+1, algo] = time
        
        beta = as.numeric(read.table(paste0(path, "beta", run))[[1]])
        
        y.pred = data.validate$x%*%beta
        validate_error[run+1, algo] = mean((y.pred-data.validate$y)^2)
        
        y.pred = data.test$x%*%beta
        test_error[run+1, algo] = mean((y.pred-data.test$y)^2)
      }
    )
    true_beta = rep(c(1:5, rep(0, ncol(data.train$x)/true_num_groups-5)), true_num_groups)
    beta_error[run + 1, algo] = sum((beta - true_beta)^2)
    
    tol = 1e-8
    beta_fpr[run+1, algo] = sum((abs(beta)>tol)*(true_beta==0))/sum(true_beta==0)
    beta_fnr[run+1, algo] = sum((abs(beta)<tol)*(true_beta!=0))/sum(true_beta!=0)
    
    gsize = length(true_beta)/expert_num_groups
    true_groups = sapply(0:(expert_num_groups-1), 
                         function(j){any(abs(true_beta[(j*gsize+1):((j+1)*gsize)])>tol)})
    s_groups = sapply(0:(expert_num_groups-1), 
                      function(j){any(abs(beta[(j*gsize+1):((j+1)*gsize)])>tol)})
    group_fpr[run+1, algo] = sum((!true_groups)*s_groups)/sum(!true_groups)
    group_fnr[run+1, algo] = sum(true_groups*(!s_groups))/sum(true_groups)
  }
}

save.image("results/results.RData")


for(algo in algorithms){
  nlam = paste0(algo," & ", switch (algo,
    HC = expert_num_groups+1,
    HC0 = 2,
    GS = 2,
    RS = 2,
    NM = 2,
    iSGL = expert_num_groups + 2,
    iSGL0 = 2
  ))
  
  write(
    paste0(nlam, " & ",
          round(mean(validate_error[,algo]), digits = 2), 
          #" (", round(sd(validate_error[,algo])/sqrt(num_runs), digits = 2), ") &",
          " &",
          round(mean(test_error[,algo]), digits = 2), 
          " &",
          round(mean(beta_error[,algo]), digits = 2), 
          " &",
          round(mean(beta_fpr[,algo])*100, digits = 2), 
          "\\% &",
          round(mean(beta_fnr[,algo])*100, digits = 2), 
          "\\% &",
          round(mean(group_fpr[,algo])*100, digits = 2), 
          "\\% &",
          round(mean(group_fnr[,algo])*100, digits = 2), 
          "\\% &",
          round(mean(runtime[,algo]/60), digits = 2), 
          " \\\\"
          )
    , "")
}

t.test(validate_error[,"HC"], validate_error[,"iSGL"], paired = T, 
       conf.level = 0.95, alternative = "less")
t.test(test_error[,"iSGL"], test_error[,"HC"], paired = T, 
       conf.level = 0.95, alternative = "less")
t.test(beta_error[,"iSGL"], beta_error[,"HC"], paired = T, 
       conf.level = 0.95, alternative = "less")
t.test(beta_fpr[,"iSGL"], beta_fpr[,"HC"], paired = T, 
       conf.level = 0.95, alternative = "less")
t.test(beta_fnr[,"iSGL"], beta_fnr[,"iSGL0"], paired = T, 
       conf.level = 0.95, alternative = "less")
t.test(group_fpr[,"iSGL"], group_fpr[,"HC"], paired = T, 
       conf.level = 0.95, alternative = "less")
t.test(group_fnr[,"iSGL"], group_fnr[,"RS"], paired = T, 
       conf.level = 0.95, alternative = "less")
