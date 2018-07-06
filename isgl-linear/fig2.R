library(ggplot2)

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

load("results/results.RData")

Method = rep(c('iSGL', 'GD'), each = num_runs)
dd <- data.frame(Method = Method, x = rep(1:num_runs, 2), 
                 y = c(test_error[,"iSGL"],
                       test_error[,"HC"]) )

p1 = ggplot(data = dd, aes(x = x, y = y, group = Method, shape = Method)) + geom_line(aes(linetype=Method,col=Method)) +
  geom_point(aes(col=Method))+
  scale_y_continuous("Test Error") + scale_x_continuous('Simulation number') + 
  scale_linetype_manual(values=c("longdash","solid")) + 
  scale_color_brewer(palette="Set1")+theme_bw()

dd <- data.frame(Method = Method, x = rep(1:num_runs, 2), 
                 y = c(validate_error[,"iSGL"],
                       validate_error[,"HC"]) )

p2 =ggplot(data = dd, aes(x = x, y = y, group = Method, shape = Method)) + geom_line(aes(linetype=Method,col=Method)) +
  geom_point(aes(col=Method))+
  scale_y_continuous("Validation Error") + scale_x_continuous('Simulation number') + 
  scale_linetype_manual(values=c("longdash","solid")) + 
  scale_color_brewer(palette="Set1")+theme_bw()

multiplot(p2, p1, cols = 1)
