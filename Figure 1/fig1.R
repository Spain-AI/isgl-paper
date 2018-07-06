if(!file.exists("../data/colitis.RData")){
  source("../data/colitis.R")
}

load("../data/colitis.RData")


train_idx <- sample(1:nrow(X),50)
X_train <- X[train_idx,]
y_train <- y[train_idx] + 0
X_validate <- X[-train_idx,]
y_validate <- y[-train_idx] + 0

data <- list(x=X_train, y=y_train)

errors_lambda2_fixed <- matrix(rep(0,100*4), nrow=100, ncol=4)
lambda2 <- seq(0.01, 0.1, length.out = 4)
lambda1 <- seq(0.01, 0.1, length.out = 100)
for(i in 1:4){
    lambda_path <- lambda1 + lambda2[i]
    alpha_path <- lambda1/lambda_path
    for(j in 1:100){
      fit.sgl <- SGL::SGL(data, membership.index, type="logit", alpha = alpha_path[j], lambdas = lambda_path[j], nlam=1, standardize = F)
      model_prediction <- X_validate %*% fit.sgl$beta
      log_sum <- sum(log(1+exp(model_prediction)))
      errors_lambda2_fixed[j,i] <- 1/length(y_validate)*(log_sum - t(model_prediction)%*%y_validate)
      }
}


library('ggplot2')
results <- data.frame(lambda1 = rep(lambda1, 4), 
                      lambda2 = rep(round(lambda2,3), each=100),
                      err = as.vector(errors_lambda2_fixed)) 

ggplot(results, aes(x=lambda1, y=err )) + 
  geom_line(aes(linetype=factor(lambda2), color = factor(lambda2)), alpha=0.9)+
  scale_y_continuous("Validation Error") + 
  scale_x_continuous(expression(lambda[1]))+
  labs(linetype=expression(lambda[2]))+
  labs(color=expression(lambda[2]))+
  scale_color_brewer(palette="Set1")+theme_bw()
