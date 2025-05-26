require(moonBook)

Binomial_leave_only_one <- function(mytable){
  require(dplyr)
  suppressWarnings(myt <- mytable2df(mytable))
  TRUE_num <- (1:nrow(myt))[str_detect(myt[[1]], 'TRUE')]
  myt[[1]][TRUE_num] <- myt[[1]][TRUE_num-2]
  myt[TRUE_num,ncol(myt)] <- myt[TRUE_num-2,ncol(myt)]
  myt <- myt[-c(TRUE_num-2, TRUE_num-1),]
  colnames(myt) <- str_c(colnames(myt), ' ', myt[1,])
  myt <- myt[-1,]
  myt <- myt %>% mutate_all(str_replace_all, "\\[ ", "(")
  myt <- myt %>% mutate_all(str_replace_all, "\\[", "(")
  myt <- myt %>% mutate_all(str_replace_all, "\\]", ")")
  myt <- myt %>% mutate_all(str_replace_all, "; ", ";")
  myt <- myt %>% mutate_all(str_replace_all, ";", " — ")
  myt <- myt %>% mutate_all(str_replace_all,  "^(?:\\s+)|(?<=\\()\\s+", , "")
  return(myt)
}
