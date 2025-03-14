library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)
library(ggplot2)

load("clean.RData")
set.seed(123)
#load("causal_forest_real_echo_avg.RData")
# Klein and Spady will be used to estimate propensity scores
# real news accuracy
### echo chamber as treatment
## external, internal efficacy and individual time spent on social media apps will be averaged

X = select(df_real,polint2, income:republican, -facebook:-reddit, infopros, infoproh, -effext,-effint,socialavg,effavg)
ks_model_real = npindex(df_real$echoreal ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+socialavg+knowscale+effavg+infopros+infoproh, data=df_real, method="kleinspady")
W.hat = fitted(ks_model_real)

cf_real_echo_avg = causal_forest(X=X,Y=df_real$totalrealavg,W=df_real$echoreal,W.hat=W.hat, seed=1234)
tau.hat_real_echo_avg = predict(cf_real_echo_avg, estimate.variance=TRUE)$predictions
sqrt_real_echo_avg =  predict(cf_real_echo_avg, estimate.variance=TRUE)$variance.estimates
tree_real_echo_avg <- get_tree(cf_real_echo_avg, 5) # get a representative tree out of the forest
plot(tree_real_echo_avg)
ATE = mean(tau.hat_real_echo_avg)
importance = data.frame(CATE=tau.hat_real_echo_avg,X)
rfcate_real_echo_avg=randomForest(CATE~., data=importance)
importance(rfcate_real_echo_avg)

# Bootstrap SE

n_bootstrap <- 500  

# Store bootstrap estimates
boot_cate <- matrix(NA, nrow = n_bootstrap, ncol = length(df_real$echoreal))
boot_ate = rep(NA,n_bootstrap)
for (i in 1:n_bootstrap) {
  # Resample data with replacement
  boot_indices <- sample(1:nrow(df_real), replace = TRUE)
  df_boot <- df_real[boot_indices, ]
  # Train causal forest on resampled data
  cf_boot <- causal_forest(X = X, 
                           W = df_boot$echoreal, 
                           Y = df_boot$totalfakeavg,
                           W.hat = W.hat)
  
  # Store CATE estimates for this bootstrap sample
  boot_cate[i, ] <- predict(cf_boot)$predictions
  boot_ate[i]=mean(predict(cf_boot, estimate.variance=TRUE)$predictions)
}

# Compute bootstrap standard error
btse = sqrt(sum((boot_ate - mean(boot_ate))^2)/(n_bootstrap-1))
p_value_echo_real_avg <- mean(abs(boot_ate) >= abs(mean(boot_ate)))
print(p_value_echo_real_avg)

# heuristic information processing
cate_infoproh <- importance %>%
  group_by(infoproh_bin = cut(df_real$infoproh, breaks = 5)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_infoproh, aes(x = infoproh_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Heurisic Information Processing", x = "Heuristic Information Processing", y = "Average CATE") +
  theme_minimal()

# average social media use
cate_social <- importance %>%
  group_by(social_bin = cut(df_real$socialavg, breaks = 10)) %>%  
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_social, aes(x = social_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Social Media Use", x = "Average Social Media Use", y = "Average CATE") +
  theme_minimal()

save.image("causal_forest_real_echo_avg.RData")