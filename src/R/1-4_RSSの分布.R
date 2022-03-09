# カイ二乗分布の自由度mの各値の確率密度関数
i = 1
curve(dchisq(i,x), 0, 20, col=i)
for (i in 2:10) {
  curve(dchisq(i,x), 0, 20, col=i, add=TRUE, ann=FALSE)
  legend("topright", legend=1:10, lty=1, col=1:10)
}
