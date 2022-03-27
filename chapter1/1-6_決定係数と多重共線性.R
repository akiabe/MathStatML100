# 決定係数
R2 <- function(x, y) {
 ybar <- mean(y)
 fit <- lm(y~x)
 yhat <- fit$fitted.values
 rss <- sum((yhat - y)^2)
 tss <- sum((y - ybar)^2)
 return(1 - rss/tss)
}

n <- 100
x <- rnorm(n)
y <- rnorm(n)
R2(x, y)

p <- 2
x <- matrix(rnorm(p*N), ncol=p)
y <- rnorm(N)
R2(x, y)

# VIF
VIF <- function(x) {
  p <- ncol(x)
  values <- array(dim=p)
  for (j in 1:p) {
    values[j] <- 1 / (1 - R2(x[,-j], x[,j]))
  }
  return(values)
}

library(MASS)

x <- as.matrix(Boston)
VIF(x)
