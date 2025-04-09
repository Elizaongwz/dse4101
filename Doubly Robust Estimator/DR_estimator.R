# DR Estimator

#install.packages("drgee")
library(drgee)
library(dplyr)
library(np)
load("Datasets/Cleaning Scripts and R Data/clean.RData")
## Regression from the paper but using totalfakeavg as the outcome variable
dr_prime = drgee(oformula=formula(totalfakeavg~republican+socialavg+knowscale+age+effavg),
                 eformula = formula(prime~republican+socialavg+knowscale+age+effavg),
                 elink="logit",
                 data=df2_avg)
dr_prime = drgee(oformula=formula(totalfakeavg~republican+socialavg+knowscale+age+effavg),
           eformula = formula(prime~republican+socialavg+knowscale+age+effavg),
           elink="logit",
           data=df2_avg)
summary(dr_prime)

dr_echo = drgee(oformula=formula(totalfakeavg~republican+socialavg+knowscale+age+effavg),
           eformula = formula(echofake~republican+socialavg+knowscale+age+effavg),
           elink="logit",
           data=df1_avg)
summary(dr_echo)

# estimates are biased, OLS invalid model

# Now adding more variables and splitting efficacy and social media use into its various types and platforms as they do have difference importance in driving heterogeneity
# multicollinearity check
#vif(lm(totalfakeavg~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh, data=df)) # all <5
dr_fake_echo = drgee(oformula=formula(totalfakeavg~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                eformula = formula(echofake~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                elink="logit",
                data=df1)
summary(dr_fake_echo)

dr_fake_prime = drgee(oformula=formula(totalfakeavg~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                     eformula = formula(prime~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                     elink="logit",
                     data=df2)
summary(dr_fake_prime)

save.image("Doubly Robust Estimator/DR_estimators.RData")




