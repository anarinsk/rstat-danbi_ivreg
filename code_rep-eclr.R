# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R
# https://rpubs.com/wsundstrom/t_ivreg

xfun::pkg_attach2('tidyverse', 'AER')

mydata <- 
  read_csv('data/mroz.csv') %>% 
  na_if('.') %>%  
  filter(!is.na(wage)) %>% 
  mutate(
    lwage = as.numeric(lwage)
  )

reg_ex0 <- lm(lwage ~ educ, data=mydata)
summary(reg_ex0)

reg_iv0 <- ivreg(lwage~educ|fatheduc,data=mydata)
summary(reg_iv0, vcov = sandwich, diagnostics = TRUE)

reg_1 <- lm(lwage~educ+age+exper+expersq, data=mydata) # OLS estimation
reg_1_sm <- summary(reg_1)
print(reg_1_sm)

reg_iv1 <- ivreg(lwage~educ+exper+expersq|fatheduc+motheduc+exper+expersq,data=mydata)
summary(reg_iv1, vcov = sandwich, diagnostics = T)

reg_iv2 <- ivreg(lwage~educ+exper+expersq|.-educ+fatheduc+motheduc,data=mydata)
summary(reg_iv2, vcov = sandwich, diagnostics = T)



# IV related Testing procedures

## Instrument relevance
# First Stage
first_stage <- lm(educ~age+exper+expersq+fatheduc+motheduc,data=mydata)
instrFtest <- waldtest(first_stage,.~.-fatheduc-motheduc)
print(instrFtest)

## Testing for exogeneity
# First Stage
first_stage <- lm(educ~age+exper+expersq+fatheduc+motheduc,data=mydata)
# Hausman
Hausman_reg <- lm(lwage~educ+age+exper+expersq+first_stage$residuals,data=mydata)
print(summary(Hausman_reg))

HausWutest <- waldtest(Hausman_reg,.~.-first_stage$residuals)
print(HausWutest)

Sargan_reg <- lm(reg_iv1$residuals~age+exper+expersq+fatheduc+motheduc,data=mydata)
Sargan_reg_sm <- summary(Sargan_reg)

Sargan_test <- Sargan_reg_sm$r.squared*nrow(mydata)
print(Sargan_test)
print(1-pchisq(Sargan_test,1))  # prints p-value
