check_id <- function(id) {
  id <- toupper(id)
  
  # 檢查基本格式
  if (!grepl("^[A-Z][12][0-9]{8}$", id)) {
    return(FALSE)
  }
  
  # 英文字母對應數值表
  letter_values <- c(
    A=10, B=11, C=12, D=13, E=14, F=15, G=16, H=17, I=34,
    J=18, K=19, L=20, M=21, N=22, O=35, P=23, Q=24, R=25,
    S=26, T=27, U=28, V=29, W=32, X=30, Y=31, Z=33
  )
  
  # 取得第一碼對應數值
  code <- letter_values[substr(id, 1, 1)]
  X1 <- code %/% 10
  X2 <- code %% 10
  
  # 拆開數字部分
  nums <- as.numeric(strsplit(substr(id, 2, 10), "")[[1]])
  
  # 權重
  weights <- c(1, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  all_nums <- c(X1, X2, nums)
  
  # 計算加權和
  sum_val <- sum(all_nums * weights)
  
  # 檢查是否為10的倍數
  return(sum_val %% 10 == 0)
}

# 主程式
id_input <- readline("請輸入身分證號：")
if (check_id(id_input)) {
  cat("✅ 身分證號合法！\n")
}else{
  cat("❌ 身分證號不合法。\n")
}
