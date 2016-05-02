# 2D data with 2 class labels
# Default distribution
#  n = 25
#
#  Label: 
#   Y = 0 with p = 0.5
#       1 with p = 0.5
#
#  Covariates:
#   X1 ~ N( Y-0.5, 1 )
#   X2 ~ N( 0, 1 )

# generate n number of sample data points
get2drand <- function(n = 25, y =round(runif(n)), 
                      x1 = rnorm(n, mean = y-0.5), 
                      x2 = rnorm(n)){
  return(data.frame(y, x1, x2))
}

# generate the population space (0.01 to 0.99 quantiles)
get2dpop <- function(x1.0 = NULL, x1.1 = NULL, 
                     x2.0 = NULL, x2.1 = NULL,
                     res = 0.01){
  p <- seq(0.01,0.99, by = res)
  if(is.null(x1.0)) x1.0 = qnorm(p, mean = -0.5)
  if(is.null(x1.1)) x1.1 = qnorm(p, mean = 0.5)
  if(is.null(x2.0)) x2.0 = qnorm(p)
  if(is.null(x2.1)) x2.1 = qnorm(p)
  
  dt0 <- data.frame(x1 = rep(x1.0, length(x2.0)),
                    x2 = rep(x2.0, each = length(x1.0)),
                    y = 0)
  dt1 <- data.frame(x1 = rep(x1.1, length(x2.1)),
                    x2 = rep(x2.1, each = length(x2.0)),
                    y = 1)
  return(rbind(dt0, dt1))
}