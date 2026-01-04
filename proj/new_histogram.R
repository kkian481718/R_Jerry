# === 1. 設定工作目錄 ===
setwd("D:/MiffyProject/R_Jerry/proj/")
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# === 2. 讀取資料 ===
file_name <- "gender_data.csv"
if (!file.exists(file_name)) { stop("錯誤：找不到檔案，請確認檔名與路徑。") }

raw_data <- read.csv(file_name, header = FALSE, stringsAsFactors = FALSE)

# === 3. 資料清理與定義四個時期 ===
clean_df <- raw_data %>%
  filter(grepl("年", V1) & grepl("\\d{4}", V1)) %>%
  mutate(
    Year = as.numeric(sub(".*(\\d{4}).*", "\\1", V1)),
    Male = as.numeric(gsub(",", "", V3)),
    Female = as.numeric(gsub(",", "", V5))
  ) %>%
  # 修改範圍：從 2019 開始 (包含疫情前) 到 2024
  filter(Year >= 2019 & Year <= 2024) %>%
  mutate(
    Period = case_when(
      Year == 2019 ~ "1. 疫情前常態 (2019)",          # 基準線：應該是女生略多
      Year %in% c(2020, 2021) ~ "2. 疫情嚴峻期 (2020-21)", # 剛性需求：男生變多
      Year == 2022 ~ "3. 恢復過渡期 (2022)",          # 國門重啟：女生回升
      Year %in% c(2023, 2024) ~ "4. 疫情後復甦 (2023-24)" # 報復性旅遊：女生大勝
    )
  )

# === 4. 統計與繪圖 ===
agg_table <- clean_df %>%
  group_by(Period) %>%
  summarise(
    Male_Sum = sum(Male),
    Female_Sum = sum(Female)
  ) %>%
  pivot_longer(cols = c(Male_Sum, Female_Sum), names_to = "Gender", values_to = "Count") %>%
  group_by(Period) %>%
  mutate(Percentage = Count / sum(Count))

# 繪製直方圖
gg_complete <- ggplot(agg_table, aes(x = Period, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  
  # 加入數值標籤
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 4.5, color = "white", fontface = "bold") +
  
  labs(
    title = "疫情前後完整週期：出國旅遊性別結構變化 (2019-2024)",
    subtitle = "加入2019年作為對照，可見結構從「常態」到「異常」再回歸「常態」的過程",
    x = "時期 (Period)",
    y = "比例 (Proportion)",
    fill = "性別"
  ) +
  
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Female_Sum" = "#E41A1C", "Male_Sum" = "#377EB8"), 
                    labels = c("女性", "男性")) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.position = "top"
  )

# 顯示並儲存
print(gg_complete)
ggsave("gender_structure_complete_2019_2024.png", width = 10, height = 6)
cat("\n已產生包含疫情前的完整圖表：gender_structure_complete_2019_2024.png\n")
