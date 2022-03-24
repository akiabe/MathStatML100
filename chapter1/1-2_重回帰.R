# データ生成
n = 100
p = 2
beta = c(1, 2, 3)
x = matrix(rnorm(n*p), nrow=n, ncol=p)
y = beta[1] + beta[2]*x[,1] + beta[3]*x[,2] + rnorm(n)
X = cbind(1, x)

# betaの推定
beta = solve(t(X) %*% X) %*% t(X) %*% y
