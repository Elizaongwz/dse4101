library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)
library(ggplot2)

load("Datasets/Cleaning Scripts and R Data/clean.RData")
set.seed(123)
# Klein and Spady will be used to estimate propensity scores
# Fake news accuracy
### Prime used as treatment
## external, internal efficacy and individual time spent on social media apps will be averaged

X = dplyr::select(df2_avg,polint2, income:effavg, infopros, infoproh)
#ks_model = npindex(df2_avg$prime ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+socialavg+knowscale+effavg+infopros+infoproh, data=df2_avg, method="kleinspady")
#W.hat = fitted(ks_model)
ps = glm(df2_avg$prime ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+socialavg+knowscale+effavg+infopros+infoproh, data=df2_avg,family=binomial('logit'))$fitted
summary(ps[df2_avg$prime==1])
summary(ps[df2_avg$prime==0]) #good matches found
cf_prime_avg = causal_forest(X=X,Y=df2_avg$totalfakeavg,W=df2_avg$prime,W.hat=ps, seed=1234)
tau.hat_prime_avg = predict(cf_prime_avg, estimate.variance=TRUE)$predictions
sqrt_prime_avg =  predict(cf_prime_avg, estimate.variance=TRUE)$variance.estimates
#tree_prime_avg <- get_tree(cf_prime_avg, 5) # get a representative tree out of the forest
#plot(tree_fake_prime)
# Estimate Average Treatment Effect (ATE)
ATE = average_treatment_effect(cf_prime_avg)
cat("95% CI for the ATE:", round(ATE[1], 3), "+/-", round(qnorm(0.975) * ATE[2], 3), "\n")
# Omnibus Test for Heterogeneity
test_calibration(cf_prime_avg) #reasonably calibrated
importance = data.frame(CATE=tau.hat_prime_avg,X)
rfcate_prime_avg=randomForest(CATE~., data=importance)
importance(rfcate_prime_avg)
var_imp = importance(rfcate_prime_avg)
var_imp_df = data.frame(Variable = rownames(var_imp), Importance = var_imp[, 1])
var_imp_df$Variable = recode(var_imp_df$Variable, 
                             "socialavg" = "Average Social Media Use",
                             "age" = "Age",
                             "twitter" = "Twitter",
                             "infoproh" = "Heuristic Information Processing",
                             "knowscale" = "Political Knowledge",
                             "effavg" = "Average Efficacy",
                             "infopros" = "Systematic Information Processing",
                             "income" = "Income",
                             "facebook" = "Facebook",
                             "newsattention" = "News Attention",
                             "edu" = "Education",
                             "polint2" = "Political Interest",
                             "male" = "Male",
                             "republican" = "Republican",
                             "white" = "White",
                             "vote16" = "Voted")
ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variables", y = "Importance") +
  theme_minimal()


# Bootstrap SE

n_bootstrap <- 500  

# Store bootstrap estimates
boot_cate <- matrix(NA, nrow = n_bootstrap, ncol = length(df2_avg$prime))
boot_ate = rep(NA,n_bootstrap)
for (i in 1:n_bootstrap) {
  # Resample data with replacement
  boot_indices <- sample(1:nrow(df2), replace = TRUE)
  df_boot <- df1[boot_indices, ]
  # Train causal forest on resampled data
  cf_boot <- causal_forest(X = X, 
                           W = df_boot$prime, 
                           Y = df_boot$totalfakeavg,
                           W.hat = ps)
  
  # Store CATE estimates for this bootstrap sample
  boot_cate[i, ] <- predict(cf_boot)$predictions
  boot_ate[i]=mean(predict(cf_boot, estimate.variance=TRUE)$predictions)
}

# Age
cate_age <- importance %>%
  group_by(age_bin = cut(df2_avg$age, breaks = 10)) %>%  # Bin age into groups
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_age, aes(x = age_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +  # One dot per category
  geom_line(group = 1, color = "red") +  # Connect points with a line
  labs(title = "Average CATE by Age", 
       x = "Age", 
       y = "Average CATE") +
  theme_minimal()

df2_avg$age_binned <- cut(df2_avg$age, 
                          breaks = quantile(df2_avg$age, probs = seq(0, 1, length.out = 11), na.rm = TRUE), 
                          include.lowest = TRUE)
boxplot(tau.hat_prime_avg ~ df2_avg$age_binned, 
        xlab = "Age (Binned)", 
        ylab = "Estimated CATE", 
        col = "gray", border = "black")
lines(smooth.spline(df2_avg$age_binned, tau.hat_prime_avg), lwd = 3, col = 4)
# social media use
cate_social <- importance %>%
  group_by(social_bin = cut(df2_avg$socialavg, breaks = 10)) %>%  # Bin age into groups
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_social, aes(x = social_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +  # One dot per category
  geom_line(group = 1, color = "red") +  # Connect points with a line
  labs(title = "Average CATE by Average Social Media Use", 
       x = "Average Social Media Use", 
       y = "Average CATE") +
  theme_minimal()

# heuristic information processing
cate_infopros <- importance %>%
  group_by(infoproh_bin = cut(df2_avg$infoproh, breaks = 5)) %>% 
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_infopros, aes(x = infoproh_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Heuristic Information Processing", x = "Heuristic Information Processing", y = "Average CATE") +
  theme_minimal()

boxplot(tau.hat_prime_avg ~ round(df2_avg$infoproh), xlab = "Heuristic Information Processing", ylab = "Estimated CATE")
spline_fit <- smooth.spline(df2_avg$infoproh, tau.hat_prime_avg)
extended_x <- seq(min(df2_avg$infoproh), max(df2_avg$infoproh) + 1, length.out = 100)
spline_pred <- predict(spline_fit, extended_x)
lines(spline_pred$x, spline_pred$y, lwd = 3, col = 4)

# Compute bootstrap standard error
btse = sqrt(sum((boot_ate - mean(boot_ate))^2)/(n_bootstrap-1))
z = ATE[1]/btse
p_value = 2 * (1 - pnorm(abs(z)))
save.image("Causal Forest Believability for Fake News Outcome/Prime Treatment/R Data/causal_forest_prime_avg.RData")
