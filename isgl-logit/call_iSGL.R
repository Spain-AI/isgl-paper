# Author: JC Laria 
# the iterative SGL
#====================

call_iSGL = function(expert_num_groups, run, type = "linear"){
  load(paste0("data/train_run", run,'.RData'))
  
  group.length = rep(ncol(data.train$x)/expert_num_groups, expert_num_groups)
  
  time = system.time( isgl.fit <- 
                        sglfast::isgl(data.train, 
                                      data.validate, 
                                      group.length = group.length, 
                                      type = type, standardize = F) )
  save(time, isgl.fit, file = paste0("results/iSGL/fit",run,".RData"))
}

call_iSGL0 = function(expert_num_groups, run, type = "linear"){
  load(paste0("data/train_run", run,'.RData'))
  
  group.length = rep(ncol(data.train$x)/expert_num_groups, expert_num_groups)
  
  time = system.time( isgl.fit <- 
                        sglfast::isgl_simple(data.train, 
                                             data.validate, 
                                             group.length = group.length, 
                                             type = type, standardize = F) )
  save(time, isgl.fit, file = paste0("results/iSGL0/fit",run,".RData"))
}