library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)
library(ggplot2)

load("Datasets/Cleaning Scripts and R Data/clean.RData")
set.seed(123)
#load("causal_forest_fake_echo_avg.RData")
# Klein and Spady will be used to estimate propensity scores
# Fake news accuracy
### echo chamber as treatment
## external, internal efficacy and individual time spent on social media apps will be averaged

X = dplyr::select(df1_avg,polint2, income:effavg, infopros, infoproh)
#ks_model = npindexbw(df1_avg$echofake ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+socialavg+knowscale+effavg+infopros+infoproh, data=df1_avg, method="kleinspady")
#ps_ks = npindex(ks_model,newdata=df1_avg)$mean
ps = glm(df1_avg$echofake ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+socialavg+knowscale+effavg+infopros+infoproh, data=df1_avg,family=binomial('logit'))$fitted
#W.hat = fitted(ks_model)

cf_fake_echo_avg = causal_forest(X=X,Y=df1_avg$totalfakeavg,W=df1_avg$echofake,W.hat=ps, seed=1234)
tau.hat_fake_echo_avg = predict(cf_fake_echo_avg, estimate.variance=TRUE)$predictions
sqrt_fake_echo_avg =  predict(cf_fake_echo_avg, estimate.variance=TRUE)$variance.estimates
#tree_fake_echo_avg <- get_tree(cf_fake_echo_avg, 5) # get a representative tree out of the forest
#plot(tree_fake_echo_avg)
importance = data.frame(CATE=tau.hat_fake_echo_avg,X)
rfcate_fake_echo_avg=randomForest(CATE~., data=importance)
importance(rfcate_fake_echo_avg)
var_imp = importance(rfcate_fake_echo_avg)
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

# Estimate Average Treatment Effect (ATE)
ATE = average_treatment_effect(cf_fake_echo_avg)
cat("95% CI for the ATE:", round(ATE[1], 3), "+/-", round(qnorm(0.975) * ATE[2], 3), "\n")

# Omnibus Test for Heterogeneity
test_calibration(cf_fake_echo_avg) #reasonably calibrated

# Compare regions with high and low estimated CATEs
high_effect = tau.hat_fake_echo_avg > median(tau.hat_fake_echo_avg)
ate.high = average_treatment_effect(cf_fake_echo_avg, subset = high_effect)
ate.low = average_treatment_effect(cf_fake_echo_avg, subset = !high_effect)

cat("95% CI for difference in ATE:",
    round(ate.high[1] - ate.low[1], 3), "+/-",
    round(qnorm(0.975) * sqrt(ate.high[2]^2 + ate.low[2]^2), 3), "\n")

# Test whether covariates influence treatment effects
dr.score = tau.hat_fake_echo_avg + df1_avg$echofake / cf_fake_echo_avg$W.hat *
  (df1_avg$totalfakeavg - cf_fake_echo_avg$Y.hat - (1 - cf_fake_echo_avg$W.hat) * tau.hat_fake_echo_avg) -
  (1 - df1_avg$echofake) / (1 - cf_fake_echo_avg$W.hat) * (df1_avg$totalfakeavg - cf_fake_echo_avg$Y.hat + cf_fake_echo_avg$W.hat * tau.hat_fake_echo_avg)

#t.test(tau.hat_fake_echo_avg, mu = 0)  # Tests if mean ATE is different from zero


# Histogram of CATEs
ggplot(data.frame(CATE = tau.hat_fake_echo_avg), aes(x = CATE)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6, color = "black") +
  labs(title = "Distribution of CATE Estimates", x = "CATE", y = "Frequency") +
  theme_minimal()

# political knowledge
cate_knowledge <- importance %>%
  group_by(know_bin = cut(df1_avg$knowscale, breaks = 10)) %>%  # Bin age into groups
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_knowledge, aes(x = know_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +  # One dot per category
  geom_line(group = 1, color = "red") +  # Connect points with a line
  labs(title = "Average CATE by Political Knowledge", 
       x = "Political Knowledge", 
       y = "Average CATE") +
  theme_minimal()

boxplot(tau.hat_fake_echo_avg ~ round(df1_avg$knowscale), xlab = "Political Knowledge", ylab = "Estimated CATE")
lines(smooth.spline(df1_avg$knowscale, tau.hat_fake_echo_avg, df = 4), lwd = 3, col = 4)


# Average Efficacy
cate_effavg <- importance %>%
  group_by(effavg_bin = cut(df1_avg$effavg, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_effavg, aes(x = effavg_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Efficacy", x = "Average Efficacy", y = "Average CATE") +
  theme_minimal()
boxplot(tau.hat_fake_echo_avg ~ round(df1_avg$effavg), xlab = "Average Efficacy", ylab = "Estimated CATE")
lines(smooth.spline(df1_avg$effavg, tau.hat_fake_echo_avg, df = 4), lwd = 3, col = 4)

# age
cate_age <- importance %>%
  group_by(age_bin = cut(df1_avg$age, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_age, aes(x = age_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Age Group", x = "Age Group", y = "Average CATE") +
  theme_minimal()

df1_avg$age_binned <- cut(df1_avg$age, 
                          breaks = quantile(df1_avg$age, probs = seq(0, 1, length.out = 11), na.rm = TRUE), 
                          include.lowest = TRUE)
boxplot(tau.hat_fake_echo_avg ~ df1_avg$age_binned, 
        xlab = "Age (Binned)", 
        ylab = "Estimated CATE", 
        col = "gray", border = "black")
lines(smooth.spline(df1_avg$age_binned, tau.hat_fake_echo_avg), lwd = 3, col = 4)

# systematic information processing

boxplot(tau.hat_fake_echo_avg ~ round(df1_avg$infopros), xlab = "Systematic Information Processing", ylab = "Estimated CATE")
lines(smooth.spline(df1_avg$infopros, tau.hat_fake_echo_avg, df = 4), lwd = 3, col = 4)

# average social media use
cate_social <- importance %>%
  group_by(social_bin = cut(df1_avg$socialavg, breaks = 10)) %>%  
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_social, aes(x = social_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Social Media Use", x = "Average Social Media Use", y = "Average CATE") +
  theme_minimal()

boxplot(tau.hat_fake_echo_avg ~ round(df1_avg$socialavg), xlab = "Average Social Media Use", ylab = "Estimated CATE")
lines(smooth.spline(df1_avg$socialavg, tau.hat_fake_echo_avg, df = 4), lwd = 3, col = 4)

# Bootstrap SE

n_bootstrap <- 500  

# Store bootstrap estimates
boot_cate <- matrix(NA, nrow = n_bootstrap, ncol = length(df1_avg$echofake))
boot_ate = rep(NA,n_bootstrap)
for (i in 1:n_bootstrap) {
  # Resample data with replacement
  boot_indices <- sample(1:nrow(df1), replace = TRUE)
  df_boot <- df1[boot_indices, ]
  # Train causal forest on resampled data
  cf_boot <- causal_forest(X = X, 
                           W = df_boot$echofake, 
                           Y = df_boot$totalfakeavg,
                           W.hat = ps)
  
  # Store CATE estimates for this bootstrap sample
  boot_cate[i, ] <- predict(cf_boot)$predictions
  boot_ate[i]=mean(predict(cf_boot, estimate.variance=TRUE)$predictions)
}

# Compute bootstrap standard error
btse = sqrt(sum((boot_ate - mean(boot_ate))^2)/(n_bootstrap-1))
t <- ATE[1]/btse
p_value = 2 * pt(-abs(t), df = length(tau.hat_fake_echo_avg) - 1)

save.image("Causal Forest Believability for Fake News Outcome/Echo Chamber Treatment/R Data/causal_forest_fake_echo_avg.RData")