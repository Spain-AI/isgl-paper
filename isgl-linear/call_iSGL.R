# Author: JC Laria 
# the iterative SGL
#====================

call_iSGL = function(expert_num_groups, run, type = "linear"){
  X = as.matrix(read.table(paste0("data/X_train-run",run)))
  y = as.matrix(read.table(paste0("data/y_train-run",run)))
  data.train = list(x=X, y=y)
  
  X = as.matrix(read.table(paste0("data/X_validate-run",run)))
  y = as.matrix(read.table(paste0("data/y_validate-run",run)))
  data.validate = list(x=X, y=y)
  
  group.length = rep(ncol(X)/expert_num_groups, expert_num_groups)
  
  time = system.time( isgl.fit <- 
                        sglfast::isgl(data.train, 
                                      data.validate, 
                                      group.length = group.length, 
                                      type = type, standardize = F) )
  save(time, isgl.fit, file = paste0("results/iSGL/fit",run,".RData"))
}

call_iSGL0 = function(expert_num_groups, run, type = "linear"){
  X = as.matrix(read.table(paste0("data/X_train-run",run)))
  y = as.matrix(read.table(paste0("data/y_train-run",run)))
  data.train = list(x=X, y=y)
  
  X = as.matrix(read.table(paste0("data/X_validate-run",run)))
  y = as.matrix(read.table(paste0("data/y_validate-run",run)))
  data.validate = list(x=X, y=y)
  
  group.length = rep(ncol(X)/expert_num_groups, expert_num_groups)
  
  time = system.time( isgl.fit <- 
                        sglfast::isgl_simple(data.train, 
                                      data.validate, 
                                      group.length = group.length, 
                                      type = type, standardize = F) )
  save(time, isgl.fit, file = paste0("results/iSGL0/fit",run,".RData"))
}