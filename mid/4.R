o_df = read.table(file.choose(), header=T)

mean_o = mean(o_df[[1]])
sd_o = sd(o_df[[1]])

high_b = mean_o + sd_o
low_b = mean_o - sd_o

res = ifelse(o_df[[1]] > high_b, "高標",
             ifelse(o_df[[1]] < low_b, "低標", "均標"))

res_df = data.frame(
  分數 = o_df[[1]],
  標準 = res
)

print(res_df)