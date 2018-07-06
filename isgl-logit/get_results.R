# Author: JC Laria
true_num_groups = 1
expert_num_groups = as.integer(read.table("results/expert_num_groups"))
algorithms = c("iSGL", "iSGL0")

#algo = algorithms[4]

num_runs = length(dir("data/"))/2

runtime = matrix(0, nrow = num_runs, ncol = length(algorithms))
colnames(runtime) = algorithms
validate_error = test_error = beta_error = beta_fpr = beta_fnr =
  group_fpr = group_fnr = runtime


for (run in 0:(num_runs-1)) {
  load(paste0("data/train_run", run+1,'.RData'))
  load(paste0("data/test_run", run+1,'.RData'))

  for(algo in algorithms){
    switch (algo,
            iSGL = {
              path = paste0("results/",algo,"/")
              load(paste0(path, "fit", run+1, ".RData"))
              runtime[run+1, algo] = time["elapsed"]

              y.pred = (sglfast::predict.isgl(isgl.fit, data.validate$x)>0.5)+0
              validate_error[run+1, algo] = mean(y.pred == data.validate$y)

              y.pred = (sglfast::predict.isgl(isgl.fit, data.test$x)>0.5)+0
              test_error[run+1, algo] = mean(y.pred == data.test$y)

              beta = isgl.fit$beta
            },
            iSGL0 = {
              path = paste0("results/",algo,"/")
              load(paste0(path, "fit", run+1, ".RData"))
              runtime[run+1, algo] = time["elapsed"]

              y.pred = (sglfast::predict.isgl(isgl.fit, data.validate$x)>0.5)+0
              validate_error[run+1, algo] = mean(y.pred == data.validate$y)

              y.pred = (sglfast::predict.isgl(isgl.fit, data.test$x)>0.5)+0
              test_error[run+1, algo] = mean(y.pred == data.test$y)

              beta = isgl.fit$beta
            }
    )
    true_beta = rep(c(1:5, rep(0, ncol(data.train$x)/true_num_groups-5)),
                    times = true_num_groups)
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

t.test(validate_error[,"iSGL"], validate_error[,"iSGL0"], paired = T,
       conf.level = 0.95, alternative = "less")
