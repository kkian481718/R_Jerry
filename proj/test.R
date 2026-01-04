# === 1. 設定工作目錄與讀取 ===
setwd("D:/MiffyProject/R_Jerry/proj/")
library(dplyr)

file_name <- "gender_data.csv"
raw_data <- read.csv(file_name, header = FALSE, stringsAsFactors = FALSE)

# === 2. 整理特定年份數據 (比較 2019 vs 2021) ===
# 我們抓出這兩年來做「重點對決」
df_compare <- raw_data %>%
  filter(grepl("年", V1) & grepl("\\d{4}", V1)) %>%
  mutate(
    Year = as.numeric(sub(".*(\\d{4}).*", "\\1", V1)),
    Male = as.numeric(gsub(",", "", V3)),
    Female = as.numeric(gsub(",", "", V5)),
    Total = Male + Female
  ) %>%
  filter(Year %in% c(2019, 2021)) # 只留這兩年

print(df_compare)

# === 3. 執行「雙母體比例檢定」 (Two-proportion Z-test) ===
# H0: 2021年的男性比例 <= 2019年的男性比例
# H1: 2021年的男性比例 > 2019年的男性比例 (我們要證明的)

prop_test_result <- prop.test(
  x = c(df_compare$Male[df_compare$Year == 2021], df_compare$Male[df_compare$Year == 2019]), # 男性人數
  n = c(df_compare$Total[df_compare$Year == 2021], df_compare$Total[df_compare$Year == 2019]), # 總人數
  alternative = "greater" # 檢定 2021 是否「大於」 2019
)

cat("\n=== 1. 比例檢定結果 (Z-test) ===\n")
cat("檢定問題：2021年(疫情中)的男性比例是否顯著高於 2019年(疫情前)？\n")
print(prop_test_result)

if(prop_test_result$p.value < 0.05) {
  cat("結論：拒絕虛無假設。統計證實，疫情嚴峻期(2021)的男性出國比例「顯著高於」疫情前(2019)。\n")
}

# === 4. 計算勝算比 (Odds Ratio) ===
# Odds = 男性人數 / 女性人數
odds_2019 <- df_compare$Male[df_compare$Year == 2019] / df_compare$Female[df_compare$Year == 2019]
odds_2021 <- df_compare$Male[df_compare$Year == 2021] / df_compare$Female[df_compare$Year == 2021]
odds_ratio <- odds_2021 / odds_2019

cat("\n=== 2. 勝算比 (Odds Ratio) 分析 ===\n")
cat("2019 男性勝算 (Male/Female):", round(odds_2019, 3), "\n")
cat("2021 男性勝算 (Male/Female):", round(odds_2021, 3), "\n")
cat("Odds Ratio (2021 vs 2019):", round(odds_ratio, 3), "\n")
cat("解讀：在2021年，男性相對於女性的出國勝算(Odds)是2019年的", round(odds_ratio, 2), "倍。\n")
cat("這代表疫情讓男性出國的「相對優勢」大幅提升 (反映剛性需求)。\n")