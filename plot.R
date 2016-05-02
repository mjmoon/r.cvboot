library(ggplot2)
library(animation)

# generate grid space for plotting
get2dspace <- function(min = -3, max = 3, res = 0.05){
  x <- seq(min, max, res)
  return(data.frame(x1 = rep(x, length(x)),
                    x2 = rep(x, each = length(x))))
}

# create a scatter plot for binary class data on x1 and x2
plot2d <- function(dt, title = "", ylim = NULL, xlim = NULL, alpha = 0.5){
  plt <- ggplot() +
    labs(x = "X1", y = "X2", title = title) + theme_minimal() +
    geom_point(data = dt, aes(x = x1, y = x2, color = factor(y)),
               alpha = alpha, size = 3) + 
    scale_color_discrete(name = "Y") +
    theme(plot.title = element_text(size = rel(2)),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.2)),
          legend.text = element_text(size = rel(1)),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.background = element_rect(color="white"))
  if(!is.null(ylim)) plt <- plt + ylim(ylim)
  if(!is.null(xlim)) plt <- plt + xlim(xlim)
  return(plt)
}

# create a scatter plot for binary class data on x1 and x2 
# with a fitted boundary using LDA or KNN
plotfit <- function(dtsam, pspace, 
                    title = "", method = "lda", 
                    p =c(1,1)/2, k = 1, ylim = NULL, xlim = NULL){
  plt <- plot2d(dtsam, title = title, alpha = 1, ylim = ylim, xlim = xlim)
  if(method == "lda"){
    fit <- lda(y ~ x1 + x2, data=dtsam, prior = p)
    contrfit <- cbind(pspace, 
                      pred = as.numeric(predict(fit, pspace)$class))  
  }
  if(method == "knn"){
    contrfit <- cbind(pspace, 
                      pred = as.numeric(
                        knn(train = subset(dtsam, select = c("x1", "x2")),
                            cl = dtsam$y, 
                            test = pspace, k = k))) 
  }
  
  plt <- plt + geom_contour(data = contrfit, aes(x = x1, y = x2, z = pred)) + 
    geom_tile(data = contrfit, aes(x = x1, y = x2, fill = as.factor(pred)), 
              show.legend = FALSE, alpha = 0.1)
  return(plt)
}

# create a scatter plot for binary class data on x1 and x2 
# with a fitted boundary using LDA or KNN from a CV loop
plotcv <- function(dtsam, cv, i, 
                   title = "", ylim = NULL, xlim = NULL){
  plt <- plot2d(dtsam, title = title, alpha = 1, ylim = ylim, xlim = xlim)
  plt <- plt + geom_contour(data = cv$fits[[i]], aes(x = x1, y = x2, z = pred)) + 
    geom_tile(data = cv$fits[[i]], aes(x = x1, y = x2, fill = as.factor(pred)), 
              show.legend = FALSE, alpha = 0.1) +
    geom_point(data = dtsam[i,], aes(x = x1, y = x2),
                          shape = 1, color = "black", size = 5)
  return(plt)
}

# saves an animation HTML page for binary class data on x1 and x2
# over a cross-validation loop using LDA or KNN
aniplot.html <- function(dtsam, cv, imagename = "plot", filename = "aniplot", 
                    title = "", ylim = NULL, xlim = NULL){
  len <- length(cv$fits)
  plots <- NULL
  for(i in 1:len){
    plots[[i]] <- plotcv(dtsam, cv, i, title = title, ylim = ylim, xlim = xlim)
  }
  saveHTML(lapply(plots, function(x) print(x)), 
           image.name = imagename, htmlfile = paste(filename, ".html", sep = ""))
  return(TRUE)
}

# saves a GIF file for binary class data on x1 and x2
# over a cross-validation loop using LDA or KNN
aniplot.gif <- function(dtsam, cv, imagename = "plot", filename = "aniplot", 
                         title = "", ylim = NULL, xlim = NULL, del.ind.imgs = TRUE){
  len <- length(cv$fits)
  plots <- NULL
  for(i in 1:len){
    plots[[i]] <- plotcv(dtsam, cv, i, title = title, ylim = ylim, xlim = xlim)
  }
  saveGIF(lapply(plots, function(x) print(x)), 
           image.name = imagename, movie.name = paste(filename, ".gif", sep = ""),
          clean = del.ind.imgs)
  return(TRUE)
}

