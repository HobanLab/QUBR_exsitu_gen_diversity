rm(list=ls())
#install.packages("rgl")
library(foreign)
library(ggplot2)
library(GGally)
library(survival)
library(rgl)

dat <- read.dta("https://stats.idre.ucla.edu/stat/data/intreg_data.dta")

# summary of the variables
summary(dat)

# bivariate plots
ggpairs(dat[, -1], lower = list(combo = "box"), upper = list(combo = "blank"))

by(dat[, 2:5], dat$type, colMeans, na.rm = TRUE)

# setup the survival object with interval censoring
(Y <- with(dat, Surv(lgpa, ugpa, event = rep(3, nrow(dat)), type = "interval")))

m <- survreg(Y ~ write + rating + type, data = dat, dist = "gaussian")

summary(m)

# analysis of deviance table
anova(m)

# for the regression surface
f <- function(x, y, type = "vocational") {
  newdat <- data.frame(write = x, rating = y, type = factor(type, levels = levels(dat$type)))
  predict(m, newdata = newdat)
}

# Create X, Y, and Z grids
X <- with(dat, seq(from = min(write), to = max(write), length.out = 10))
Y <- with(dat, seq(from = min(rating), to = max(rating), length.out = 10))
Z <- outer(X, Y, f)

# Create 3d scatter plot and add the regression surface
open3d(windowRect = c(100, 100, 700, 700))

with(dat, plot3d(x = write, y = rating, z = ugpa, xlab = "write", ylab = "rating", 
                 zlab = "ugpa", xlim = range(write), ylim = range(rating), zlim = range(ugpa)))
par3d(ignoreExtent = TRUE)

# add regression surface for each type of program in a different colour
# with 50 percent transparency (alpha = .5)
surface3d(X, Y, outer(X, Y, f, type = "vocational"), col = "blue", alpha = 0.5)
surface3d(X, Y, outer(X, Y, f, type = "general"), col = "red", alpha = 0.5)
surface3d(X, Y, outer(X, Y, f, type = "academic"), col = "green", alpha = 0.5)

# create an animated movie movie3d(spin3d(axis=c(.5,.5,.5), rpm=5),
# duration=6, dir = 'intreg_fig')


(r <- with(dat, cor(cbind(yhat = predict(m), lgpa, ugpa))))

# pseudo R2
r^2
