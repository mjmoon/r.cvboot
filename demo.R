setwd("~/Dropbox/Blog/CV-Bootstrap")
source("2drand.R")
source("prederr.R")
source("plot.R")

# generate data
sam <- get2drand()
pop <- get2dpop()
space <- get2dspace(res = 0.03)

# plot population space and sample
plot2d(pop, alpha = 0.1)

# lda-CV
plotfit(sam, space)
cvlda <- loocv(sam, space)
aniplot.gif(sam, cvlda, filename = "cvloop.lda", ylim = c(-3,3), xlim = c(-3,3))

# knn-CV
plotfit(sam, space, method = "knn", k = 5)
cvknn <- loocv(sam, space, method = "knn", k = 5)
aniplot.gif(sam, cvknn, filename = "cvloop.knn", ylim = c(-3,3), xlim = c(-3,3))
