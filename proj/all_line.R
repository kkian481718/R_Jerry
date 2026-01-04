# === 1. 設定工作目錄 ===
setwd("D:/MiffyProject/R_Jerry/proj/")

# 載入套件
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) 

# === 2. 讀取資料 ===
file_name <- "gender_data.csv"
if (!file.exists(file_name)) { stop("錯誤：找不到檔案，請確認檔名與路徑。") }

raw_data <- read.csv(file_name, header = FALSE, stringsAsFactors = FALSE)

# === 3. 資料清理 (1994-2024) ===
clean_df <- raw_data %>%
  filter(grepl("年", V1) & grepl("\\d{4}", V1)) %>%
  mutate(
    Year = as.numeric(sub(".*(\\d{4}).*", "\\1", V1)),
    Male = as.numeric(gsub(",", "", V3)),
    Female = as.numeric(gsub(",", "", V5))
  ) %>%
  mutate(
    Total = Male + Female,
    Male_Pct = Male / Total,
    Female_Pct = Female / Total
  ) %>%
  select(Year, Male_Pct, Female_Pct)

# === 4. 轉換格式 ===
plot_data <- clean_df %>%
  pivot_longer(cols = c(Male_Pct, Female_Pct), 
               names_to = "Gender", 
               values_to = "Percentage") %>%
  mutate(
    Gender = factor(Gender, levels = c("Female_Pct", "Male_Pct"), labels = c("女性", "男性"))
  )

# === 5. 繪製趨勢圖 (修正空白問題) ===
gg_trend <- ggplot(plot_data, aes(x = Year, y = Percentage, color = Gender, group = Gender)) +
  
  # 背景標示 (疫情期間)
  annotate("rect", xmin = 2019.5, xmax = 2022.5, ymin = -Inf, ymax = Inf, 
           fill = "gray", alpha = 0.2) +
  annotate("text", x = 2021, y = 0.65, label = "疫情影響期", color = "gray40", fontface = "bold") +
  
  # 線條與點
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  
  scale_color_manual(values = c("女性" = "#E41A1C", "男性" = "#377EB8")) +
  
  # Y 軸設定
  scale_y_continuous(labels = scales::percent, limits = c(0.3, 0.7)) +
  
  # === 修正重點在這裡 ===
  scale_x_continuous(
    breaks = seq(1994, 2024, 2),     # 每2年一個刻度
    limits = c(1994, 2024),          # 強制 X 軸從 1994 開始，到 2024 結束
    expand = c(0.01, 0)              # 0.01 代表左右只留 1% 空間 (原本預設是 5%)，幾乎貼齊邊緣
  ) +
  
  labs(
    title = "1994-2024 歷年中華民國國民出國性別比例趨勢",
    subtitle = "資料來源：交通部觀光署 / 內政部移民署",
    x = "年份 (Year)",
    y = "比例 (Percentage)",
    color = "性別"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

# 顯示並儲存
print(gg_trend)
ggsave("gender_trend_1994_2024_fixed.png", width = 10, height = 6)
cat("\n圖片已修正空白並儲存為 gender_trend_1994_2024_fixed.png\n")