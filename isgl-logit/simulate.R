# Author: JC Laria 

expert_num_groups = as.integer(read.table("results/expert_num_groups"))

# Required functions

wlog = function(text,...){
  cat(paste0(date(),"\t", text,...,"\n"), file="log.txt", append = T)
}

source("call_iSGL.R")

# Required parallel libraries
library(foreach)
library(Rmpi)
library(doMPI)

cl <- startMPIcluster()
registerDoMPI(cl)

writeLines(c(""), "log.txt")
wlog("Welcome to the simulations")

files = dir("data/")
num_runs = length(files)/2
wlog("The number of runs is ", num_runs , ". If am wrong, please stop this script and re-run make_data.sh")

algorithms = c("iSGL", "iSGL0")

foreach(algo=algorithms)%:%foreach(run=1:(num_runs))%dopar%{
  switch (algo,
          iSGL = {
            call_iSGL(expert_num_groups, run, type = "logit")
          },
          iSGL0 = {
            call_iSGL0(expert_num_groups, run, type = "logit")
          }
  )
  wlog(algo,"\t",run, " completed!")
}

wlog("tHE eNd")
closeCluster(cl)
mpi.quit()