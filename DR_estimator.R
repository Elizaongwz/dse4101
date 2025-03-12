# DR Estimator

#install.packages("drgee")
library(drgee)
library(dplyr)

## Regression from the paper but using totalfakeavg as the outcome variable
#rhode_ols = lm(totalfakeavg~republican+echofake+socialavg+knowscale+age+prime+effavg, data=df)
#summary(rhode_ols)

# Testing for biasness using total fake average as the outcome using the same variables as the author
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

# Now adding more variables and splitting efficacy and social media use into its various types and platforms as they do have difference importance in driving heterogeneity
# multicollinearity check
vif(lm(totalfakeavg~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh, data=df)) # all <5

dr_fake_echo = drgee(oformula=formula(totalfakeavg~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                eformula = formula(echofake~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                elink="logit",
                fanily="gaussian",
                data=df)
summary(dr_fake_echo)

dr_fake_prime = drgee(oformula=formula(totalfakeavg~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                     eformula = formula(prime~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                     elink="logit",
                     fanily="gaussian",
                     data=df)
summary(dr_fake_prime)

# For totalrealavg as the outcome to mitigate biasness from small sample size (check for causal forest estimators)
dr_real_echo = drgee(oformula=formula(totalfakeavg~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                 eformula = formula(echoreal~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                 elink="logit",
                 fanily="gaussian",
                 data=df_real)
summary(dr_real_echo)

dr_real_prime = drgee(oformula=formula(totalrealavg~republican+pin+snap+facebook+insta+reddit+twitter+knowscale+age+effext+effint+infopros+infoproh),
                      eformula = formula(prime~republican+socialavg+knowscale+age+effavg),
                      elink="logit",
                      fanily="gaussian",
                      data=df_real)
summary(dr_real_prime)

save.image("DR_estimators.RData")




