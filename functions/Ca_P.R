require(tidyverse)

# https://doi.org/10.1016/S0272-6386(03)00905-3
# Target P in CKD5 or KRT children
targetP <- function(age){case_when(age < 13 ~ 6, age >= 13 ~ 5.5)}

# https://doi.org/10.1053/j.ajkd.2008.11.017
# normal P values (hyper P) in CKD2 ~ CKD4 children
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

# normal P values (hypoP) in CKD2 ~ CKD4 children
hypoP <- function(age){
  case_when(
    (age >= 0 & age < 0.5) ~ 5.2, 
    (age >= 0.5 & age < 1) ~ 5.0, 
    (age >= 1 & age < 6) ~ 4.5, 
    (age >= 6 & age < 13) ~ 3.6, 
    (age >= 13 & age < 20) ~ 2.3,
    age >= 20 ~ 4.5
  )
}
            
# normal Ca values (hyperCa) in CKD2 ~ CKD4 children
hyperCa <- function(age){
  case_when(
    (age >= 0 & age < 3) ~ 11.3, 
    (age >= 3 & age < 6) ~ 10.8, 
    (age >= 6 & age < 13) ~ 10.3, 
    (age >= 13 & age < 20) ~ 10.2,
    age >= 20 ~ 10.2
  )
}
            
# normal Ca values (hypoCa) in CKD2 ~ CKD4 children
hypoCa <- function(age){
  case_when(
    (age >= 0 & age < 3) ~ 8.8, 
    (age >= 3 & age < 6) ~ 9.4, 
    (age >= 6 & age < 13) ~ 9.4, 
    (age >= 13 & age < 20) ~ 8.8,
    age >= 20 ~ 8.8
  )
}
  
