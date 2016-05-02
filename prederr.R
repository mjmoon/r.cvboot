library(class)
library(MASS)

# A leave-one-out cross validation loop for LDA or KNN classification
# of binary class data on x1 and x2
# outputs:
#   mean:   the mean prediction error
#   perrs:  a vector of prediction erros,
#   fits:   a list of data frames for each cross-validation fit
#   method: method used ("lda" or "knn")
loocv <- function(dt, pspace, method = "lda", p = c(1,1)/2, k = 1){
  n <- nrow(dt)
  pes <- NULL
  cntrs <- NULL
  for(i in 1:n){
    dt.cv <- dt[-i,]
    if(method == "lda"){
      fit <- lda(y ~ x1 + x2, data = dt.cv, prior = p)
      pred <- predict(fit, subset(dt, select = c("x1", "x2"))[i,])
      cntrs[[i]] <- cbind(pspace, 
                          pred = as.numeric(predict(fit, pspace)$class))
      pes[i] = factor(dt$y)[i] != pred$class
    }
    if(method == "knn"){
      pred <- knn(train = subset(dt.cv, select = c("x1", "x2")), 
                  cl = dt.cv$y, 
                  test = subset(dt, select = c("x1", "x2"))[i,], k = k)
      cntrs[[i]] <- cbind(pspace, 
                          pred = as.numeric(
                            knn(train = subset(dt.cv, select = c("x1", "x2")),
                                cl = dt.cv$y, 
                                test = pspace, k = k)))
      pes[i] = factor(dt$y)[i] != pred
    }
  }
  return(list(mean = mean(pes), perrs = pes, fits = cntrs, method = method))
}
