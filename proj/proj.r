# 安裝並載入必要的套件
# install.packages(c("ggplot2", "dplyr", "tidyr"))
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. 讀取資料
raw_data <- read.csv("D:/MiffyProject/R_Jerry/proj/gender_data.csv", header = FALSE, stringsAsFactors = FALSE)

# 2. 資料清理與提取
# 找出包含所需年份的列 (109年=2020, 110年=2021, 111年=2022, 112年=2023, 113年=2024)
years_of_interest <- c("109 年", "110 年", "111 年", "112 年", "113 年")
target_rows <- raw_data[grep(paste(years_of_interest, collapse="|"), raw_data[,1]), ]

# 建立乾淨的 Dataframe
# 根據原始檔案結構: Col 1=年份, Col 2=合計, Col 3=男, Col 5=女
clean_df <- data.frame(
  Year_Raw = target_rows[,1],
  Total = as.numeric(gsub(",", "", target_rows[,2])), # 去除逗號並轉為數字
  Male = as.numeric(gsub(",", "", target_rows[,3])),
  Female = as.numeric(gsub(",", "", target_rows[,5]))
)

# 新增西元年份與自訂的時間區段 (Period)
clean_df <- clean_df %>%
  mutate(
    Year = case_when(
      grepl("109", Year_Raw) ~ 2020,
      grepl("110", Year_Raw) ~ 2021,
      grepl("111", Year_Raw) ~ 2022,
      grepl("112", Year_Raw) ~ 2023,
      grepl("113", Year_Raw) ~ 2024
    ),
    Period = case_when(
      Year %in% c(2020, 2021) ~ "疫情前 (Pre)",
      Year == 2022 ~ "疫情中 (During)",
      Year %in% c(2023, 2024) ~ "疫情後 (Post)"
    )
  )

# 3. 數據有效性檢定 (Data Validity Check)
# 檢查 "男 + 女" 是否等於 "合計" (允許微小誤差)
clean_df <- clean_df %>%
  mutate(
    Calculated_Total = Male + Female,
    Diff = abs(Total - Calculated_Total),
    IsValid = Diff < 10 # 允許誤差小於10人次
  )

cat("=== 數據有效性檢定結果 ===\n")
print(clean_df[, c("Year", "Total", "Calculated_Total", "IsValid")])

if(all(clean_df$IsValid)) {
  cat("\n檢定通過：所有年度的性別加總與原始合計數值相符。\n")
} else {
  cat("\n警告：部分數據存在誤差，請檢查原始檔案。\n")
}

# 4. 統計檢定：卡方檢定 (Chi-square Test of Independence)
# 彙整各時期的男女總人數
agg_table <- clean_df %>%
  group_by(Period) %>%
  summarise(
    Male_Sum = sum(Male),
    Female_Sum = sum(Female)
  )

# 轉換為矩陣格式以進行卡方檢定
chisq_matrix <- as.matrix(agg_table[, c("Male_Sum", "Female_Sum")])
rownames(chisq_matrix) <- agg_table$Period

cat("\n=== 卡方檢定結果 (Chi-square Test) ===\n")
chisq_result <- chisq.test(chisq_matrix)
print(chisq_result)

# 顯示各時期性別比例
prop_table <- prop.table(chisq_matrix, 1) * 100
cat("\n=== 各時期性別比例 (%) ===\n")
print(round(prop_table, 2))

# 5. 視覺化繪圖
# 將資料轉換為長格式 (Long format) 以利 ggplot 繪圖
plot_data <- agg_table %>%
  pivot_longer(cols = c(Male_Sum, Female_Sum), names_to = "Gender", values_to = "Count") %>%
  group_by(Period) %>%
  mutate(Percentage = Count / sum(Count))

# 指定 Period 的順序
plot_data$Period <- factor(plot_data$Period, levels = c("疫情前 (Pre)", "疫情中 (During)", "疫情後 (Post)"))

ggplot(plot_data, aes(x = Period, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 5) +
  labs(
    title = "疫情前後台灣人出國旅遊性別結構變化",
    x = "時期 (Period)",
    y = "比例 (Proportion)",
    fill = "性別"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1", labels = c("女性", "男性"))