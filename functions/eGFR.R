require(tidyverse)
# man = "m", woman = "f"
# reference: https://doi.org/10.1016/j.kint.2020.10.047

u25_CysC <- function(CysC, age, sex){
  K = 0
  age <- floor(age)
  if (age == 0) {return("NA")}
  if (age > 25) {return("NA")}
  if (age == 1 & sex == "m") {K = 74.8}
  if (age == 2 & sex == "m") {K = 75.6}
  if (age == 3 & sex == "m") {K = 76.5}
  if (age == 4 & sex == "m") {K = 77.3}
  if (age == 5 & sex == "m") {K = 78.2}
  if (age == 6 & sex == "m") {K = 79.0}
  if (age == 7 & sex == "m") {K = 79.9}
  if (age == 8 & sex == "m") {K = 80.8}
  if (age == 9 & sex == "m") {K = 81.7}
  if (age == 10 & sex == "m") {K = 82.6}
  if (age == 11 & sex == "m") {K = 83.5}
  if (age == 12 & sex == "m") {K = 84.4}
  if (age == 13 & sex == "m") {K = 85.3}
  if (age == 14 & sex == "m") {K = 86.3}
  if (age == 15 & sex == "m") {K = 87.2}
  if (age == 16 & sex == "m") {K = 83.7}
  if (age == 17 & sex == "m") {K = 80.4}
  if (age >= 18 & age <= 25 & sex == "m") {K = 77.1}
  if (age == 1 & sex == "f") {K = 76.5}
  if (age == 2 & sex == "f") {K = 76.8}
  if (age == 3 & sex == "f") {K = 77.1}
  if (age == 4 & sex == "f") {K = 77.4}
  if (age == 5 & sex == "f") {K = 77.7}
  if (age == 6 & sex == "f") {K = 78.0}
  if (age == 7 & sex == "f") {K = 78.3}
  if (age == 8 & sex == "f") {K = 78.6}
  if (age == 9 & sex == "f") {K = 78.9}
  if (age == 10 & sex == "f") {K = 79.3}
  if (age == 11 & sex == "f") {K = 79.6}
  if (age == 12 & sex == "f") {K = 79.9}
  if (age == 13 & sex == "f") {K = 77.8}
  if (age == 14 & sex == "f") {K = 75.8}
  if (age == 15 & sex == "f") {K = 73.8}
  if (age == 16 & sex == "f") {K = 71.9}
  if (age == 17 & sex == "f") {K = 70.0}
  if (age >= 18 & age <= 25 & sex == "f") {K = 68.3}
  egfr = K/CysC
  return(egfr)
}
#

#
u25_Cr <-  function(Cr, age, height, sex){
  K = 0
  age <- floor(age)
  if (age == 0) {return("NA")}
  if (age > 25) {return("NA")}
  if (!(sex %in% c("m","f"))) {return("NA")}
  if (age == 1 & sex == "m") {K = 35.7}
  if (age == 2 & sex == "m") {K = 36.0}
  if (age == 3 & sex == "m") {K = 36.3}
  if (age == 4 & sex == "m") {K = 36.6}
  if (age == 5 & sex == "m") {K = 36.9}
  if (age == 6 & sex == "m") {K = 37.2}
  if (age == 7 & sex == "m") {K = 37.5}
  if (age == 8 & sex == "m") {K = 37.8}
  if (age == 9 & sex == "m") {K = 38.1}
  if (age == 10 & sex == "m") {K = 38.4}
  if (age == 11 & sex == "m") {K = 38.7}
  if (age == 12 & sex == "m") {K = 39.0}
  if (age == 13 & sex == "m") {K = 40.8}
  if (age == 14 & sex == "m") {K = 42.6}
  if (age == 15 & sex == "m") {K = 44.5}
  if (age == 16 & sex == "m") {K = 46.5}
  if (age == 17 & sex == "m") {K = 48.6}
  if (age >= 18 & age <= 25 & sex == "m") {K = 50.8}
  if (age == 1 & sex == "f") {K = 33.1}
  if (age == 2 & sex == "f") {K = 33.3}
  if (age == 3 & sex == "f") {K = 33.6}
  if (age == 4 & sex == "f") {K = 33.9}
  if (age == 5 & sex == "f") {K = 34.1}
  if (age == 6 & sex == "f") {K = 34.4}
  if (age == 7 & sex == "f") {K = 34.7}
  if (age == 8 & sex == "f") {K = 35.0}
  if (age == 9 & sex == "f") {K = 35.2}
  if (age == 10 & sex == "f") {K = 35.5}
  if (age == 11 & sex == "f") {K = 35.8}
  if (age == 12 & sex == "f") {K = 36.1}
  if (age == 13 & sex == "f") {K = 36.9}
  if (age == 14 & sex == "f") {K = 37.8}
  if (age == 15 & sex == "f") {K = 38.6}
  if (age == 16 & sex == "f") {K = 39.5}
  if (age == 17 & sex == "f") {K = 40.4}
  if (age >= 18 & age <= 25 & sex == "f") {K = 41.4}
  egfr = K*height/Cr
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


