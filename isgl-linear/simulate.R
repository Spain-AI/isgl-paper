# Author: JC Laria 

expert_num_groups = as.integer(read.table("results/expert_num_groups"))

# Required functions

wlog = function(text,...){
  cat(paste0(date(),"\t", text,...,"\n"), file="log.txt", append = T)
}

source("call_iSGL.R")
source("gridSearch.R")

# Required parallel libraries
library(foreach)
library(Rmpi)
library(doMPI)

cl <- startMPIcluster()
registerDoMPI(cl)

writeLines(c(""), "log.txt")
wlog("Welcome to the simulations")

files = dir("data/")
num_runs = length(files)/6
wlog("The number of runs is ", num_runs , ". If am wrong, please stop this script and re-run make_data.sh")

algorithms = c("HC", "HC0", "iSGL", "iSGL0", "NM", "GS", "RS")

foreach(algo=algorithms)%:%foreach(run=0:(num_runs-1))%dopar%{
  switch (algo,
    HC = {
      system(paste("~/miniconda2/bin/python call_HC.py",expert_num_groups,run,"full"))
    },
    HC0 = {
      system(paste("~/miniconda2/bin/python call_HC.py",expert_num_groups,run,"simple"))
    },
    NM = {
      system(paste("~/miniconda2/bin/python call_NM.py",expert_num_groups, run))
    },
    GS = {
      call_GS(expert_num_groups, run)
    },
    RS = {
      call_RS(expert_num_groups, run)
    },
    iSGL = {
      call_iSGL(expert_num_groups, run)
    },
    iSGL0 = {
      call_iSGL0(expert_num_groups, run)
    }
  )
  wlog(algo,"\t",run, " completed!")
}

wlog("tHE eNd")
closeCluster(cl)
mpi.quit()