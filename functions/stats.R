from_fit_to_OR <- function(fit, digits = 2){
  exp(cbind(coef(fit), confint(fit))) %>% 
    as.data.frame() %>% 
    rownames_to_column("Risk Factor") %>% 
    `colnames<-`(c('Risk Factor', 'aOR', 'lower', 'upper')) %>% 
    mutate(aOR = sprintf(str_c('%.', digits, 'f'), aOR),
           '95% CI' = str_c(sprintf(str_c('%.', digits, 'f'),lower), " - ", 
                            sprintf(str_c('%.', digits, 'f'), upper), ""),
           'aOR (95% CI)' = str_c(aOR, ' (',`95% CI`, ')')) %>% select(-3, -4) %>% 
    bind_cols(`p value` = sprintf(str_c('%.', digits + 1, 'f'),summary(fit)$coefficients[, "Pr(>|z|)"] ))
}

from_fit_to_OR2 <- function(fit, digits = 2){
  exp(cbind(coef(fit), confint(fit))) %>% 
  as.data.frame() %>% 
  rownames_to_column("Risk Factor") %>% 
  `colnames<-`(c('Risk Factor', 'aOR', 'lower', 'upper')) %>% 
  mutate(aOR = sprintf(str_c('%.', digits, 'f'), aOR),
         '95% CI' = str_c(sprintf(str_c('%.', digits, 'f'),lower), " - ",  sprintf(str_c('%.', digits, 'f'), upper), "")) %>% select(-3, -4)
}
