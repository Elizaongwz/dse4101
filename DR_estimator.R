# DR Estimator

#install.packages("drgee")
library(drgee)
library(dplyr)

## Regression from the paper but using totalfakeavg as the outcome variable
rhode_ols = lm(totalfakeavg~republican+echofake+socialavg+knowscale+age+prime+effavg, data=df)
summary(rhode_ols)

dr_prime = drgee(oformula=formula(totalfakeavg~republican+socialavg+knowscale+age+effavg),
           eformula = formula(prime~republican+socialavg+knowscale+age+effavg),
           elink="logit",
           fanily="gaussian",
           data=df)
summary(dr_prime)

dr_echo = drgee(oformula=formula(totalfakeavg~republican+socialavg+knowscale+age+effavg),
           eformula = formula(echofake~republican+socialavg+knowscale+age+effavg),
           elink="logit",
           fanily="gaussian",
           data=df)
summary(dr_echo)

# estimates are biased, OLS invalid model



