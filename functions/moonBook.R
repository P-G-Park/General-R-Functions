require(moonBook)

Binomial_leave_only_one <- function(mytable){
  suppressWarnings(myt <- mytable2df(mytable))
  TRUE_num <- (1:nrow(myt))[str_detect(myt[[1]], 'TRUE')]
  myt[[1]][TRUE_num] <- myt[[1]][TRUE_num-2]
  myt[[5]][TRUE_num] <- myt[[5]][TRUE_num-2]
  myt <- myt[-c(TRUE_num-2, TRUE_num-1),]
  return(myt)
}
