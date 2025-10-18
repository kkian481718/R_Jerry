calc_ming_from_input <- function() {
  # 讀使用者輸入
  sex <- toupper(trimws(readline("請輸入性別 (M 男 / F 女)：")))
  if (!(sex %in% c("M","F"))) {
    stop("性別只能輸入 M 或 F")
  }
  
  year <- as.integer(readline("請輸入出生年 (西元年，例如 1984)："))
  if (is.na(year) || year < 0) {
    stop("出生年輸入不合法")
  }
  
  # 取出生年後兩位數
  year2 <- year %% 100
  
  # 計算餘數
  remainder <- NA
  if (year < 2000) {
    if (sex == "M") {
      remainder <- (100 - year2) %% 9
    } else {
      remainder <- (year2 - 4) %% 9
    }
  } else {
    # 2000 年以後出生者用圖片中說的方法
    if (sex == "M") {
      remainder <- (99 - year2) %% 9
    } else {
      remainder <- (year2 + 6) %% 9
    }
  }
  # 若餘數為 0，視為 9
  if (remainder == 0) {
    remainder <- 9
  }
  
  # 餘數對應命卦（八卦代號 → 卦名）
  gua <- switch(as.character(remainder),
                "1" = "坎",
                "2" = "坤",
                "3" = "震",
                "4" = "巽",
                "6" = "乾",
                "7" = "兌",
                "8" = "艮",
                "9" = "離",
                "5" = ifelse(sex == "M", "坤", "艮"),
                NA)
  
  # 判定東四命 / 西四命
  life_type <- NA
  if (gua %in% c("坎","震","巽","離")) {
    life_type <- "東四命"
  } else if (gua %in% c("乾","坤","兌","艮")) {
    life_type <- "西四命"
  } else {
    life_type <- NA
  }
  
  # 輸出結果
  cat("性別：", sex, "\n")
  cat("出生年：", year, "（後兩位數：", year2, "）\n", sep = "")
  cat("餘數：", remainder, "\n")
  cat("命卦：", gua, "\n")
  cat("命格：", life_type, "\n")
  
  invisible(list(sex = sex,
                 year = year,
                 year2 = year2,
                 remainder = remainder,
                 gua = gua,
                 life_type = life_type))
}

# 執行時呼叫這個函式
calc_ming_from_input()
