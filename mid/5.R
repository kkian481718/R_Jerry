origin_df = read.table(file.choose(), header=F)

dumb = origin_df[[3]] %% 4
seat_res = ifelse(dumb == 1 | dumb == 2, "W",
           ifelse(dumb == 3 | dumb == 0, "A", "ERR"))

result_df = data.frame(
  身分證號碼 = origin_df[[1]],
  姓名 = origin_df[[2]],
  座號 = origin_df[[3]],
  位置 = seat_res
)

print(result_df)