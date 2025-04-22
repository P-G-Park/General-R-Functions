require(tidyverse)
# man = "m", woman = "f"
# reference: https://doi.org/10.1016/j.kint.2020.10.047

u25_CysC <- function(CysC, age, sex){
  K = 0
  age <- floor(age)
  K = case_when(
    age == 1  & sex == "m" ~ 74.8,
    age == 2  & sex == "m" ~ 75.6,
    age == 3  & sex == "m" ~ 76.5,
    age == 4  & sex == "m" ~ 77.3,
    age == 5  & sex == "m" ~ 78.2,
    age == 6  & sex == "m" ~ 79.0,
    age == 7  & sex == "m" ~ 79.9,
    age == 8  & sex == "m" ~ 80.8,
    age == 9  & sex == "m" ~ 81.7,
    age == 10 & sex == "m" ~ 82.6,
    age == 11 & sex == "m" ~ 83.5,
    age == 12 & sex == "m" ~ 84.4,
    age == 13 & sex == "m" ~ 85.3,
    age == 14 & sex == "m" ~ 86.3,
    age == 15 & sex == "m" ~ 87.2,
    age == 16 & sex == "m" ~ 83.7,
    age == 17 & sex == "m" ~ 80.4,
    age >= 18 & age <= 25 & sex == "m" ~ 77.1,
    age == 1  & sex == "f" ~ 76.5,
    age == 2  & sex == "f" ~ 76.8,
    age == 3  & sex == "f" ~ 77.1,
    age == 4  & sex == "f" ~ 77.4,
    age == 5  & sex == "f" ~ 77.7,
    age == 6  & sex == "f" ~ 78.0,
    age == 7  & sex == "f" ~ 78.3,
    age == 8  & sex == "f" ~ 78.6,
    age == 9  & sex == "f" ~ 78.9,
    age == 10 & sex == "f" ~ 79.3,
    age == 11 & sex == "f" ~ 79.6,
    age == 12 & sex == "f" ~ 79.9,
    age == 13 & sex == "f" ~ 77.8,
    age == 14 & sex == "f" ~ 75.8,
    age == 15 & sex == "f" ~ 73.8,
    age == 16 & sex == "f" ~ 71.9,
    age == 17 & sex == "f" ~ 70.0,
    age >= 18 & age <= 25 & sex == "f" ~ 68.3,
    TRUE ~ NA_real_
  )
  egfr = K*0.01/CysC
  return(egfr)
}
#

#
u25_Cr <-  function(Cr, age, height, sex){
  K = 0
  age <- floor(age)
  K = case_when(
    age == 1  & sex == "m" ~ 35.7,
    age == 2  & sex == "m" ~ 36.0,
    age == 3  & sex == "m" ~ 36.3,
    age == 4  & sex == "m" ~ 36.6,
    age == 5  & sex == "m" ~ 36.9,
    age == 6  & sex == "m" ~ 37.2,
    age == 7  & sex == "m" ~ 37.5,
    age == 8  & sex == "m" ~ 37.8,
    age == 9  & sex == "m" ~ 38.1,
    age == 10 & sex == "m" ~ 38.4,
    age == 11 & sex == "m" ~ 38.7,
    age == 12 & sex == "m" ~ 39.0,
    age == 13 & sex == "m" ~ 40.8,
    age == 14 & sex == "m" ~ 42.6,
    age == 15 & sex == "m" ~ 44.5,
    age == 16 & sex == "m" ~ 46.5,
    age == 17 & sex == "m" ~ 48.6,
    age >= 18 & age <= 25 & sex == "m" ~ 50.8,
    age == 1  & sex == "f" ~ 33.1,
    age == 2  & sex == "f" ~ 33.3,
    age == 3  & sex == "f" ~ 33.6,
    age == 4  & sex == "f" ~ 33.9,
    age == 5  & sex == "f" ~ 34.1,
    age == 6  & sex == "f" ~ 34.4,
    age == 7  & sex == "f" ~ 34.7,
    age == 8  & sex == "f" ~ 35.0,
    age == 9  & sex == "f" ~ 35.2,
    age == 10 & sex == "f" ~ 35.5,
    age == 11 & sex == "f" ~ 35.8,
    age == 12 & sex == "f" ~ 36.1,
    age == 13 & sex == "f" ~ 36.9,
    age == 14 & sex == "f" ~ 37.8,
    age == 15 & sex == "f" ~ 38.6,
    age == 16 & sex == "f" ~ 39.5,
    age == 17 & sex == "f" ~ 40.4,
    age >= 18 & age <= 25 & sex == "f" ~ 41.4,
    TRUE ~ NA_real_
  )
  egfr = K*height*0.01/Cr
  return(egfr)
}


fas_cr <- function(cr, sex, age){
  age = floor(age)
  q = case_when(
    age <= 1 ~ 0.26,
    age == 2 ~ 0.29,
    age == 3 ~ 0.31,
    age == 4 ~ 0.34,
    age == 5 ~ 0.38,
    age == 6 ~ 0.41,
    age == 7 ~ 0.44,
    age == 8 ~ 0.46,
    age == 9 ~ 0.49,
    age == 10 ~ 0.51,
    age == 11 ~ 0.53,
    age == 12 ~ 0.57,
    age == 13 ~ 0.59,
    age == 14 ~ 0.61,
    age == 15 & sex == 'm' ~ 0.72, 
    age == 16 & sex == 'm' ~ 0.78, 
    age == 17 & sex == 'm' ~ 0.82, 
    age == 18 & sex == 'm' ~ 0.85, 
    age == 19 & sex == 'm' ~ 0.88, 
    age >= 20 & sex == 'm' ~ 0.90,
    age == 15 & sex == 'f' ~ 0.64, 
    age == 16 & sex == 'f' ~ 0.67, 
    age == 17 & sex == 'f' ~ 0.69, 
    age == 18 & sex == 'f' ~ 0.69, 
    age == 19 & sex == 'f' ~ 0.70, 
    age >= 20 & sex == 'f' ~ 0.70,
    TRUE ~ 0.9
  )
  return(107.3 * q / cr)
}

