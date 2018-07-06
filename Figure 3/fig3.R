if(!file.exists("../data/colitis.RData")){
  source("../data/colitis.R")
}


optimal.cutpoint = function(y_pred, y_true){
  pred = ROCR::prediction(y_pred, y_true)
  perf1 <- performance(pred, measure = "acc")
  (posicion.max <- sapply(perf1@y.values, which.max))
  (punto.corte <- sapply(perf1@x.values, "[", posicion.max))
  return(punto.corte)
}

set.seed(0)

load("../data/colitis.RData")

ord = sample(length(y))
X = X[ord,]
y = y[ord] + 0

data.train = list(x = X[1:49,], y = y[1:49])
data.validate = list(x = X[50:80, ], y = y[50:80])
data.test = list(x = X[81:127,], y = y[81:127])

tol = 1e-8

# iSGL ===============================================================================

isgl.fit = sglfast::isgl(data.train, data.validate, membership.index, 
                                type="logit", standardize = T)

y_pred = sglfast::predict.isgl(isgl.fit, data.validate$x)

cutpoint_isgl = optimal.cutpoint(y_pred, data.validate$y)
ccrV_isgl = mean(((y_pred>cutpoint_isgl)+0) == data.validate$y)

y_pred = sglfast::predict.isgl(isgl.fit, data.test$x)
ccrT_isgl = mean(((y_pred>cutpoint_isgl)+0) == data.test$y)

num.variables_isgl = sum(abs(isgl.fit$beta)>tol)

# lasso =========================================================================

library(glmnet)

fit.lasso = glmnet(data.train$x, data.train$y, family = "binomial")
y_pred = predict(fit.lasso, data.validate$x, type = "response")
cutpoint_lasso = apply(y_pred, 2, function(x){optimal.cutpoint(x, data.validate$y)})
ccrV_lasso = sapply(1:100, function(i){mean(((y_pred[,i]>cutpoint_lasso[i])+0)==data.validate$y)})

y_pred = predict(fit.lasso, data.test$x, type = "response")
ccrT_lasso = sapply(1:100, function(i){mean(((y_pred[,i]>cutpoint_lasso[i])+0)==data.test$y)})

num.variables_lasso = apply(fit.lasso$beta, 2, function(x){sum(abs(x)>tol)})

#================================================================================

#SGL 0.05 =======================================================================

fit.SGL = SGL::SGL(data.train, membership.index, alpha = 0.05, standardize = T, nlam = 100)
y_pred = SGL::predictSGL(fit.SGL, data.validate$x)
cutpoint_sgl = apply(y_pred, 2, function(x){optimal.cutpoint(x, data.validate$y)})
ccrV_sgl = sapply(1:100, function(i){mean(((y_pred[,i]>cutpoint_sgl[i])+0)==data.validate$y)})
y_pred = SGL::predictSGL(fit.SGL, data.test$x)
ccrT_sgl = sapply(1:100, function(i){mean(((y_pred[,i]>cutpoint_sgl[i])+0)==data.test$y)})

num.variables_sgl = apply(fit.SGL$beta, 2, function(x){sum(abs(x)>tol)})
#================================================================================

#Group lasso ====================================================================

fit.gg = SGL::SGL(data.train, membership.index, alpha = 0, standardize = T, nlam = 100)
y_pred = SGL::predictSGL(fit.gg, data.validate$x)
cutpoint_gg = apply(y_pred, 2, function(x){optimal.cutpoint(x, data.validate$y)})
ccrV_gg = sapply(1:100, function(i){mean(((y_pred[,i]>cutpoint_gg[i])+0)==data.validate$y)})
y_pred = SGL::predictSGL(fit.gg, data.test$x)
ccrT_gg = sapply(1:100, function(i){mean(((y_pred[,i]>cutpoint_gg[i])+0)==data.test$y)})

num.variables_gg = apply(fit.gg$beta, 2, function(x){sum(abs(x)>tol)})
#================================================================================

#=======  PLOT ==================================================================

library(ggplot2)


# validate ======================
c.class <- c( ccrV_gg,
              ccrV_lasso,
              ccrV_sgl,
              ccrV_isgl)

Method <- c(rep("GL",100),rep("Lasso", 100),rep("SGL",100), rep("iSGL",1))
nonzero <- c( num.variables_gg,
              num.variables_lasso,
              num.variables_sgl,
              num.variables_isgl)

cutoff <- data.frame(yintercept=ccrV_isgl)

dd <- data.frame(Method = Method, x = nonzero, y = c.class)
p1 = ggplot(data = dd, aes(x = x, y = y, group = Method, shape = Method)) +
  geom_line(aes(linetype=Method,col=Method)) +
  geom_point(aes(col=Method, size = Method))+
  scale_size_manual(values = c(1,3,1,1))+
  scale_y_continuous("Validation CCR") + 
  scale_x_continuous(expression(num.variables)) +
  scale_color_brewer(palette="Set1")+theme_bw()+
  geom_hline(aes(yintercept=yintercept, linetype="iSGL", color = "iSGL"), 
             data=cutoff, show.legend = F)

# test ============================

c.class <- c( ccrT_gg,
              ccrT_lasso,
              ccrT_sgl,
              ccrT_isgl)

Method <- c(rep("GL",100),rep("Lasso", 100),rep("SGL",100), rep("iSGL",1))
nonzero <- c( num.variables_gg,
              num.variables_lasso,
              num.variables_sgl,
              num.variables_isgl)

cutoff <- data.frame(yintercept=ccrT_isgl)

dd <- data.frame(Method = Method, x = nonzero, y = c.class)
p2 = ggplot(data = dd, aes(x = x, y = y, group = Method, shape = Method)) +
  geom_line(aes(linetype=Method,col=Method)) +
  geom_point(aes(col=Method, size = Method))+
  scale_size_manual(values = c(1,3,1,1))+
  scale_y_continuous("Test CCR") + 
  scale_x_continuous(expression(num.variables)) +
  scale_color_brewer(palette="Set1")+theme_bw()+
  geom_hline(aes(yintercept=yintercept, linetype="iSGL", color = "iSGL"), 
             data=cutoff, show.legend = F)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(p1, p2, cols = 1)
