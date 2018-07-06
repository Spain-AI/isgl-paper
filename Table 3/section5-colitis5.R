wlog = function(text,...){
  cat(paste0(date(),"\t", text,...,"\n"), file="log.txt", append = T)
}

# Required parallel libraries
library(foreach)
library(Rmpi)
library(doMPI)

cl <- startMPIcluster()
registerDoMPI(cl)

writeLines(c(""), "log.txt")
wlog("Welcome to the real data simulations")


if(!file.exists("../data/colitis.RData")){
  source("../data/colitis.R")
}

load("../data/colitis.RData")
library(ROCR)

set.seed(2018)

nrun = 20
num.genes = rep(0, nrun)
num.genesets = num.genes
auc = num.genes
optimal.cutpoint = num.genes
ccr.validation = num.genes
ccr.test = num.genes



result = foreach(run = 1:nrun, .combine = "rbind")%dopar%{
  wlog("Starting run ", run)
  training.ind = sample(1:nrow(X), 80)
  train.data <- list(x = X[training.ind,], y = y[training.ind])
  test.data <- list(x = X[-training.ind,], y = y[-training.ind])
  
  validation.idx = sample(80,31)
  validate.data = list(x = train.data$x[validation.idx,], y = train.data$y[validation.idx])
  
  train.data = list( x = train.data$x[-validation.idx,], y = train.data$y[-validation.idx] )
  
  isgl = sglfast::isgl(data.train = train.data,
                       data.validate = validate.data,
                       index = membership.index, type = "logit", standardize = T)
  
  # validation risk
  validate.pred.isgl = sglfast::predict.isgl(isgl, validate.data$x)
  pred = prediction(validate.pred.isgl, validate.data$y)
  perf1 <- performance(pred, measure = "acc")
  (posicion.max <- sapply(perf1@y.values, which.max))
  (punto.corte <- sapply(perf1@x.values, "[", posicion.max))
  AUC <- performance(pred, "auc")
  
  
  
  # test error
  test.pred.isgl = sglfast::predict.isgl(isgl, test.data$x)
  correct.class.isgl = (test.pred.isgl > punto.corte) * test.data$y + (test.pred.isgl < punto.corte)* (1-test.data$y)
  c.isgl<- apply(correct.class.isgl,2,mean)
  
  # beta
  nonzero.isgl = sum(isgl$beta!=0)
  nonzero.groups = (isgl$beta!=0)*membership.index
  nonzero.groups = unique(nonzero.groups[nonzero.groups!=0])
  
  wlog("run\t",run, "\tccr_T\t", c.isgl)
  
  c(
  num.genes = nonzero.isgl,
  num.genesets = length(nonzero.groups),
  auc = AUC@y.values[[1]],
  optimal.cutpoint = punto.corte,
  ccr.validation=max(perf1@y.values[[1]]),
  ccr.test=c.isgl
  )
}

save(result, file = "result.RData")

wlog("tHE eNd")
closeCluster(cl)
mpi.quit()

# mean(num.genes)
# sd(num.genes)/sqrt(nrun)
# 
# mean(num.genesets)
# sd(num.genesets)/sqrt(nrun)
# 
# mean(auc)
# sd(auc)/sqrt(nrun)
# 
# mean(optimal.cutpoint)
# sd(optimal.cutpoint)/sqrt(nrun)
# 
# mean(ccr.validation)
# sd(ccr.validation)/sqrt(nrun)
# 
# mean(ccr.test)
# sd(ccr.test)/sqrt(nrun)
