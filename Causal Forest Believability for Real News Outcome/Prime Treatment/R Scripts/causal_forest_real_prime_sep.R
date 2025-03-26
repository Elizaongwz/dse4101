library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)
library(ggplot2)

load("Datasets/Cleaning Scripts and R Data/clean.RData")
set.seed(123)
#load("causal_forest_real_prime_sep.RData")
# Klein and Spady will be used to estimate propensity scores
# real news accuracy
### prime chamber as treatment
## external, internal efficacy and individual time spent on social media apps will be averaged

X = dplyr::select(df2_real,polint2, income:effint, infopros, infoproh,-socialavg,-effavg)
ks_model_real = npindex(df2_real$prime ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+pin+facebook+insta+twitter+snap+reddit+knowscale+effext+effint+infopros+infoproh, data=df2_real, method="kleinspady")
W.hat = fitted(ks_model_real)

cf_real_prime_sep = causal_forest(X=X,Y=df_real$totalrealavg,W=df_real$prime,W.hat=W.hat, seed=1234)
tau.hat_real_prime_sep = predict(cf_real_prime_sep, estimate.variance=TRUE)$predictions
sqrt_real_prime_sep =  predict(cf_real_prime_sep, estimate.variance=TRUE)$variance.estimates
tree_real_prime_sep <- get_tree(cf_real_prime_sep, 5) # get a representative tree out of the forest
ATE = mean(tau.hat_real_prime_sep)
importance = data.frame(CATE=tau.hat_real_prime_sep,X)
rfcate_real_prime_sep=randomForest(CATE~., data=importance)
importance(rfcate_real_prime_sep)

# Bootstrap SE

n_bootstrap <- 500  

# Store bootstrap estimates
boot_cate <- matrix(NA, nrow = n_bootstrap, ncol = length(df_real$prime))
boot_ate = rep(NA,n_bootstrap)
for (i in 1:n_bootstrap) {
  # Resample data with replacement
  boot_indices <- sample(1:nrow(df_real), replace = TRUE)
  df_boot <- df_real[boot_indices, ]
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
btse = sqrt(sum((boot_ate - mean(boot_ate))^2)/(n_bootstrap-1))
p_value_prime_real_sep <- mean(abs(boot_ate) >= abs(mean(boot_ate)))
print(p_value_prime_real_sep)

# age
cate_age <- importance %>%
  group_by(age_bin = cut(df_real$age, breaks = 5)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_age, aes(x = age_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Age", x = "Age", y = "Average CATE") +
  theme_minimal()

# average snapchat use
cate_snap <- importance %>%
  group_by(snap_bin = cut(df_real$snap, breaks = 10)) %>%  
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_snap, aes(x = snap_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Snapchat Use", x = "Average Snapchat Use", y = "Average CATE") +
  theme_minimal()

# average insta use
cate_insta <- importance %>%
  group_by(insta_bin = cut(df_real$insta, breaks = 10)) %>%  
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_insta, aes(x = insta_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Instagram Use", x = "Average Instagram Use", y = "Average CATE") +
  theme_minimal()

# average pinterest use
cate_pin <- importance %>%
  group_by(pin_bin = cut(df_real$pin, breaks = 10)) %>%  
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_pin, aes(x = pin_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Pinterest Use", x = "Average Pinterest Use", y = "Average CATE") +
  theme_minimal()

save.image("Causal Forest Believability for Real News Outcome/Prime Treatment/R Data/causal_forest_real_prime_sep.RData")