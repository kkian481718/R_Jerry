Lmap = c(
  "A"= 10, "B"= 11, "C"= 12, "D"= 13, "E"= 14,
  "F"= 15, "G"= 16, "H"= 17, "I"= 34, "J"= 18,
  "K"= 19, "L"= 20, "M"= 21, "N"= 22, "O"= 35
)

X=readline(prompt="請輸入任意英文字母: ")
i=toupper(substr(X, 1, 1))

res=Lmap[i]
if(is.na(res)) {
  cat("999")
} else {
  cat(res)
}