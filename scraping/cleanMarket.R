# converts market value to numeric

cleanMarket <- function(res) {
  
  res[res == "-"] <- NA
  res <- sub(pattern = "[.]", x = res, "")
  res <- sub(pattern = ".", x = res, "") #Remove the Euro
  res <- sub(pattern = "bn", x = res, "0000000") # 1000 Mill. they have two decimals already so 7 0s instead of 9 0s
  res <- sub(pattern = "m", x = res, "0000") # Mill. they have two decimals already so 4 0s instead of 6 0s
  res <- sub(pattern = "Th", x = res, "000")
  res <- gsub(pattern='\\s+', replacement = '', x = res)
  res[res == ""] <- NA
  res<-str_trim(res, side = c("both", "left", "right"))
  res <- as.numeric(res)
  
  return(res)
}