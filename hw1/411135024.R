x <- 1:100
n <- length(x)
mean_x <- sum(x) / n
ss <- sum((x - mean_x)^2)
var_x <- ss / (n - 1)
sd_x <- sqrt(var_x)
cat("平均數 =", mean_x, "\n")
cat("樣本變異數 Var(x) =", var_x, "\n")
cat("樣本標準差 Sd(x) =", sd_x, "\n")
cat("使用內建 var() 檢查 =", var(x), "\n")
cat("使用內建 sd() 檢查 =", sd(x), "\n")
