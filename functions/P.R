require(tidyverse)

# https://doi.org/10.1016/S0272-6386(03)00905-3
# Target P in CKD5 or KRT children
targetP <- function(age){case_when(age < 13 ~ 6, age >= 13 ~ 5.5)}

# https://doi.org/10.1053/j.ajkd.2008.11.017
# normal P values (target P) in CKD2 ~ CKD4 children
hyperP <- function(age){
  case_when(
    (age >= 0 & age < 0.5) ~ 8.4, 
    (age >= 0.5 & age < 1) ~ 7.8, 
    (age >= 1 & age < 6) ~ 6.5, 
    (age >= 6 & age < 13) ~ 5.8, 
    (age >= 13 & age < 20) ~ 4.5,
    age >= 20 ~ 4.5
  )
}


