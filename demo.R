source("2drand.R")
source("prederr.R")
source("plot.R")

# generate data
sam <- get2drand()
pop <- get2dpop()
space <- get2dspace(res = 0.03)

# plot population space and sample
plot2d(pop, alpha = 0.1, title = "Popultion Space",
       ylim = c(-3,3), xlim = c(-3,3))
plot2d(sam, alpha = 1, title = "Sample Data",
       ylim = c(-3,3), xlim = c(-3,3))

# lda-CV
plotfit(sam, space, title = "LDA Fit on Sample Data")
cvlda <- loocv(sam, space)
aniplot.gif(sam, cvlda, filename = "cvloop.lda", 
            title = "LooCV Loop of LDA", ylim = c(-3,3), xlim = c(-3,3))

# knn-CV
plotfit(sam, space, method = "knn", k = 3, "3NN Fit on Sample Data")
cvknn <- loocv(sam, space, method = "knn", k = 3)
aniplot.gif(sam, cvknn, filename = "cvloop.3nn", 
            title = "LooCV Loop of 3NN", ylim = c(-3,3), xlim = c(-3,3))
