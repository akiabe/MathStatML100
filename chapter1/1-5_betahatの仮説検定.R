## t分布とガウス分布の比較
curve(dnorm(x), -10, 10, ann=FALSE, ylim=c(0, 0.5), lwd=5)
for (i in 1:10) {
  curve(dt(x, df=i), -10, 10, col=i, add=TRUE, ann=FALSE)
  legend("topright", legend=1:10, lty=1, col=1:10)
}

## betahatの仮説検定
n <- 100
p = 1
x <- rnorm(N)
y <- rnorm(N)

# 方法1
lm(y ~ x)
summary(lm(y ~ x))

# 方法2
xbar <- mean(x)
ybar <- mean(y)
betahat1 <- sum((x - xbar) * (y - ybar)) / sum((x - xbar)^2)
betahat0 <- ybar - betahat1 * xbar
yhat <- betahat0 + betahat1 * x
rss <- sum((y - yhat)^2)
sigmahat <- sqrt(rss / (n - p -1))
b0 <- (1/N * sum(x^2)) / sum((x - xbar)^2)
b1 <- 1 / sum((x - xbar)^2)
se0 <- sigmahat * sqrt(b0)
se1 <- sigmahat * sqrt(b1)
t0 <- betahat0 / se0
t1 <- betahat1 / se1
p0 <- 2 * (1 - pt(abs(t0), n-2))
p1 <- 2 * (1 - pt(abs(t1), n-2))

betahat0; se0; t0; p0;
betahat1; se1; t1; p1;

## 仮説検定の可視化
n = 100
p = 1
r = 1000
t = NULL
for (i in 1:r) {
  x <- rnorm(n)
  y <- rnorm(n)
  xbar <- mean(x)
  ybar <- mean(y)
  fit <- lm(y ~ x)
  beta <- fit$coefficients
  rss <- sum((y - fit$fitted.values)^2)
  sigmahat <- sqrt(rss / (n -p - 1))
  b1 <- 1 / sum((x - xbar)^2)
  se1 <- sigmahat * sqrt(b1)
  t <- c(t, beta[2]/se1)
}
hist(t, breaks=sqrt(r), probability=TRUE)
curve(dt(x, n-2), -3, 3, type="l", col="red", add=TRUE)
