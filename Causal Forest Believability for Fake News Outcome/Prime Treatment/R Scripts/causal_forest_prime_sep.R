library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)
library(Matching)
library(pracma)

load("Datasets/Cleaning Scripts and R Data/clean.RData")
set.seed(123)
# Fake news accuracy
### Prime used as treatment
## external, internal efficacy and individual time spent on social media apps separated

X = dplyr::select(df2,polint2, income:effint, infopros, infoproh,-echofake)
ps = glm(df2$prime ~ polint2+income+newsattention+age+edu+male+vote16+white+republican+facebook+insta+twitter+pin+snap+reddit+knowscale+effext+effint+infopros+infoproh, data=df2,family=binomial('logit'))$fitted
summary(ps[df2$prime==1])
summary(ps[df2$prime==0]) #good matches found
cf_prime_separate = causal_forest(X=X,Y=df2$totalfakeavg,W=df2$prime,W.hat=ps, seed=1234)
tau.hat_prime_sep = predict(cf_prime_separate, estimate.variance=TRUE)$predictions
sqrt_prime_sep =  predict(cf_prime_separate, estimate.variance=TRUE)$variance.estimates
#tree_prime_sep <- get_tree(cf_prime_separate, 5) # get a representative tree out of the forest
#plot(tree_prime_sep)
# Estimate Average Treatment Effect (ATE)
ATE = average_treatment_effect(cf_prime_separate)
cat("95% CI for the ATE:", round(ATE[1], 3), "+/-", round(qnorm(0.975) * ATE[2], 3), "\n")
# Omnibus Test for Heterogeneity
test_calibration(cf_prime_separate) #reasonably calibrated
importance = data.frame(CATE=tau.hat_prime_sep,X)
rfcate_prime_sep=randomForest(CATE~., data=importance)
importance(rfcate_prime_sep)
var_imp = importance(rfcate_prime_sep)
var_imp_df = data.frame(Variable = rownames(var_imp), Importance = var_imp[, 1])
var_imp_df$Variable = recode(var_imp_df$Variable, 
                             "reddit" = "Reddit",
                             "snap" = "Snapchat",
                             "insta" = "Instagram",
                             "pin" = "Pinterest",
                             "age" = "Age",
                             "twitter" = "Twitter",
                             "infoproh" = "Heuristic Information Processing",
                             "knowscale" = "Political Knowledge",
                             "effint" = "Internal Efficacy",
                             "infopros" = "Systematic Information Processing",
                             "effext" = "External Efficacy",
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

# Cumulative Gain Plot
df2$CATE <- tau.hat_prime_sep
df2 <- df2[order(df2$CATE, decreasing = FALSE), ]
df2$quantile <- cut(seq(1, nrow(df2)), breaks = 10, labels = FALSE)  
gain_data <- aggregate(df2$totalfakeavg, by = list(df2$quantile), FUN = sum)
gain_data$cum_population <- seq(1, 10) / 10 
auc = trapz(gain_data$cum_population,gain_data$x)
# Plot cumulative gains chart
ggplot(gain_data, aes(x = cum_population, y = x)) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  geom_line(data= gain_data %>% slice(1,n()))+ 
  labs(x = "Proportion of Population", y = "Cumulative Gain", title = "Cumulative Gain Curve") +
  theme_minimal()

# average reddit use
cate_reddit <- importance %>%
  group_by(reddit_bin = cut(df2$reddit, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_snapchat, aes(x = snap_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Reddit Use", x = "Average Reddit Use", y = "Average CATE") +
  theme_minimal()
boxplot(tau.hat_prime_sep ~ round(df2$reddit), xlab = "Average Reddit Use", ylab = "Estimated CATE")
spline_fit <- smooth.spline(df2$reddit, tau.hat_prime_sep)
extended_x <- seq(min(df2$reddit), max(df2$reddit) + 1, length.out = 100)
spline_pred <- predict(spline_fit, extended_x)
lines(spline_pred$x, spline_pred$y, lwd = 3, col = 4)
#lines(smooth.spline(df2$reddit, tau.hat_prime_sep, df = 4), lwd = 3, col = 4)
# heuristic information processing
cate_infoproh <- importance %>%
  group_by(infoproh_bin = cut(df2$infoproh, breaks = 5)) %>% 
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_infoproh, aes(x = infoproh_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Heuristic Information Processing", x = "Heuristic Information Processing", y = "Average CATE") +
  theme_minimal()
boxplot(tau.hat_prime_sep ~ round(df2$infoproh), xlab = "Heuristic Information Processing", ylab = "Estimated CATE")
spline_fit <- smooth.spline(df2$infoproh, tau.hat_prime_sep)
extended_x <- seq(min(df2$infoproh), max(df2$infoproh) + 1, length.out = 100)
spline_pred <- predict(spline_fit, extended_x)
lines(spline_pred$x, spline_pred$y, lwd = 3, col = 4)

# Internal Efficacy
cate_effint <- importance %>%
  group_by(effint_bin = cut(df2$effint, breaks = 10)) %>%  # Bin age into groups
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_effint, aes(x = effint_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +  # One dot per category
  geom_line(group = 1, color = "red") +  # Connect points with a line
  labs(title = "Average CATE by Internal Efficacy", 
       x = "Internal Efficacy", 
       y = "Average CATE") +
  theme_minimal()
boxplot(tau.hat_prime_sep ~ round(df2$effint), xlab = "Internal Efficacy", ylab = "Estimated CATE")
spline_fit <- smooth.spline(df2$effint, tau.hat_prime_sep)
extended_x <- seq(min(df2$effint), max(df2$effint) + 1, length.out = 100)
spline_pred <- predict(spline_fit, extended_x)
lines(spline_pred$x, spline_pred$y, lwd = 3, col = 4)
#lines(smooth.spline(df2$effint, tau.hat_prime_sep, df = 4), lwd = 3, col = 4)
# Bootstrap SE

n_bootstrap <- 500  

# Store bootstrap estimates
boot_cate <- matrix(NA, nrow = n_bootstrap, ncol = length(df2$prime))
boot_ate = rep(NA,n_bootstrap)
for (i in 1:n_bootstrap) {
  # Resample data with replacement
  boot_indices <- sample(1:nrow(df2), replace = TRUE)
  df_boot <- df2[boot_indices, ]
  # Train causal forest on resampled data
  cf_boot <- causal_forest(X = X, 
                           W = df_boot$prime, 
                           Y = df_boot$totalfakeavg,
                           W.hat = ps)
  
  # Store CATE estimates for this bootstrap sample
  boot_cate[i, ] <- predict(cf_boot)$predictions
  boot_ate[i]=mean(predict(cf_boot, estimate.variance=TRUE)$predictions)
}

# Compute bootstrap standard error
btse_prime_sep = sqrt(sum((boot_ate - mean(boot_ate))^2)/(n_bootstrap-1))
t <- ATE[1]/btse
p_value = 2 * pt(-abs(t), df = length(tau.hat_prime_sep) - 1)
save.image("Causal Forest Believability for Fake News Outcome/Prime Treatment/R Data/causal_forest_prime_sep.RData")