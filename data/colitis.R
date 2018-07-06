### Source code to get the Colitis dataset
## Original script from Simon et al. (2013) "A sparse-group lasso"

#source("http://www.bioconductor.org/biocLite.R")
#biocLite("GEOquery")

library(ggplot2)
library(Biobase)
library(GEOquery)
library(GSA)

### This is the code used to analyze the Cancer dataset in the manuscript
gds1615 <- getGEO('GDS1615', destdir = "../data/readingSoft/")

## This preprocesses it

eset <- GDS2eSet(gds1615, do.log2 = TRUE)

## This constructs our design matrix and reponse

y <- (as.numeric(pData(eset)$disease.state) == 2)
X <- t(exprs(eset))

## This grabs the gene identifiers

Gene.Identifiers <- Table(gds1615)[,2]

## The following code creates the group index using the C1 genesets

filename="../data/C1.gmt"
junk1=GSA.read.gmt(filename)

index <- rep(0,length(Gene.Identifiers))
for(i in 1:277){
  indi <- match(junk1$genesets[[i]],Gene.Identifiers)
  index[indi] <- i
}

Gene.set.info <- junk1  
dim(X)
length(y)
ind.include <- which(index != 0)
genenames <- Gene.Identifiers[ind.include]
X <- X[,ind.include]
membership.index <- rep(0,ncol(X))
for(i in 1:277){ 
  for(j in 1:length(Gene.set.info$genesets[[i]])){
    change.ind <- match(Gene.set.info$genesets[[i]][j],genenames)
    if(!is.na(change.ind)){
      if(membership.index[change.ind] == 0){
        membership.index[change.ind] <- i
      }
    }
  }
}

save(X, y, membership.index, file="../data/colitis.RData")
