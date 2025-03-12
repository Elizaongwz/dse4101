library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)

load("clean.RData")
#load("causal_forest_prime_sep.RData")
set.seed(123)
# Klein and Spady will be used to estimate propensity scores
# Fake news accuracy
### Prime used as treatment
## external, internal efficacy and individual time spent on social media apps separated

# without echo chamber included
X = select(df1,polint2, income:effint, infopros, infoproh)
#Y.forest = regression_forest(X,df1$totalfakeavg, num.trees = 500, min.node.size = 5)
#Y.hat = predict(Y.forest)$predictions
ks_model = npindex(df1$prime ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+facebook+insta+twitter+pin+snap+reddit+knowscale+effext+effint+infopros+infoproh, data=df1, method="kleinspady")
W.hat = fitted(ks_model)

cf_prime_separate = causal_forest(X=X,Y=df1$totalfakeavg,W=df1$prime,W.hat=W.hat, seed=1234)
tau.hat_prime_sep = predict(cf_prime_separate, estimate.variance=TRUE)$predictions
sqrt_prime_sep =  predict(cf_prime_separate, estimate.variance=TRUE)$variance.estimates
#tree_prime_sep <- get_tree(cf_prime_separate, 5) # get a representative tree out of the forest
#plot(tree_prime_sep)
ATE = mean(tau.hat_prime_sep)
importance = data.frame(CATE=tau.hat_prime_sep,X)
rfcate_prime_sep=randomForest(CATE~., data=importance)
importance(rfcate_prime_sep)

# average snapchat use
cate_snapchat <- importance %>%
  group_by(snap_bin = cut(df1$snap, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_snapchat, aes(x = snap_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Snapchat Use", x = "Average Snapchat Use", y = "Average CATE") +
  theme_minimal()

# systematic information processing
cate_infopros <- importance %>%
  group_by(infopros_bin = cut(df1$infopros, breaks = 5)) %>% 
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_infopros, aes(x = infopros_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Systematic Information Processing", x = "Systematic Information Processing", y = "Average CATE") +
  theme_minimal()

# average pinterest use
cate_pin <- importance %>%
  group_by(pin_bin = cut(df1$pin, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_pin, aes(x = pin_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Pinterest Use", x = "Average Pinterest Use", y = "Average CATE") +
  theme_minimal()

# age
cate_age <- importance %>%
  group_by(age_bin = cut(df1$age, breaks = 10)) %>%  # Bin age into groups
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_age, aes(x = age_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +  # One dot per category
  geom_line(group = 1, color = "red") +  # Connect points with a line
  labs(title = "Average CATE by Age", 
       x = "Age", 
       y = "Average CATE") +
  theme_minimal()
# Bootstrap SE

n_bootstrap <- 500  

# Store bootstrap estimates
boot_cate <- matrix(NA, nrow = n_bootstrap, ncol = length(df1$prime))
boot_ate = rep(NA,n_bootstrap)
for (i in 1:n_bootstrap) {
  # Resample data with replacement
  boot_indices <- sample(1:nrow(df1), replace = TRUE)
  df_boot <- df1[boot_indices, ]
  # Train causal forest on resampled data
  cf_boot <- causal_forest(X = X, 
                           W = df_boot$prime, 
                           Y = df_boot$totalfakeavg,
                           W.hat = W.hat)
  
  # Store CATE estimates for this bootstrap sample
  boot_cate[i, ] <- predict(cf_boot)$predictions
  boot_ate[i]=mean(predict(cf_boot, estimate.variance=TRUE)$predictions)
}

# Compute bootstrap standard error
btse_prime_sep = sqrt(sum((boot_ate - mean(boot_ate))^2)/(n_bootstrap-1))
p_value_prime_sep <- mean(abs(boot_ate) >= abs(mean(boot_ate)))
print(p_value_prime_sep) #0.608, not statistically significant
# with echo chamber included

X = select(df1,polint2, income:effint, infopros, infoproh, echofake)
#Y.forest = regression_forest(X,df1$totalfakeavg, num.trees = 500, min.node.size = 5)
#Y.hat = predict(Y.forest)$predictions
ks_model_echo_prime = npindex(df1$prime ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+facebook+insta+twitter+pin+snap+reddit+knowscale+effext+effint+infopros+infoproh+echofake, data=df1, method="kleinspady")
W.hat = fitted(ks_model)

cf_echo_prime_separate = causal_forest(X=X,Y=df1$totalfakeavg,W=df1$prime,W.hat=W.hat, seed=1234)
tau.hat_echo_prime_sep = predict(cf_echo_prime_separate, estimate.variance=TRUE)$predictions
sqrt_echo_prime_sep =  predict(cf_echo_prime_separate, estimate.variance=TRUE)$variance.estimates
#tree_prime_sep <- get_tree(cf_prime_separate, 5) # get a representative tree out of the forest
#plot(tree_prime_sep)
ATE_echo_prime = mean(tau.hat_prime_sep)
importance_echo_prime = data.frame(CATE=tau.hat_echo_prime_sep,X)
rfcate_echo_prime_sep=randomForest(CATE~., data=importance_echo_prime)
importance(rfcate_echo_prime_sep)

save.image("causal_forest_prime_sep.RData")