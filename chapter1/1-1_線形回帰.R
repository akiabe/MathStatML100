# 最小二乗法
least_squares = function(x, y) {
  xbar = mean(x)
  ybar = mean(y)
  beta1hat = sum((x - xbar)*(y - ybar)) / sum((x - xbar)^2)
  beta0hat = ybar - beta1hat*xbar
  return(list(a = beta0hat, b = beta1hat))
}

# データ生成
N = 100
a = rnorm(1)
b = rnorm(N)
x = rnorm(N)
y = a*x + b + rnorm(N)
plot(x, y)

# 中心化前の直線
abline(least_squares(x, y)$a, least_squares(x, y)$b, col="red")

# 中心化後の直線
x = x - mean(x)
y = y - mean(y)
plot(x, y)
abline(least_squares(x, y)$a, least_squares(x, y)$b, col="blue")

