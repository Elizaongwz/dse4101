library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)
library(ggplot2)

load("clean.RData")
#load("causal_forest_prime_avg.RData")
set.seed(123)
# Klein and Spady will be used to estimate propensity scores
# Fake news accuracy
### Prime used as treatment
## external, internal efficacy and individual time spent on social media apps will be averaged

X = select(df_avg,polint2, income:effavg, infopros, infoproh)
ks_model = npindex(df_avg$prime ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+socialavg+knowscale+effavg+infopros+infoproh, data=df_het_avg, method="kleinspady")
W.hat = fitted(ks_model)

cf_prime_avg = causal_forest(X=X,Y=df_avg$totalfakeavg,W=df_avg$prime,W.hat=W.hat, seed=1234)
tau.hat_prime_avg = predict(cf_prime_avg, estimate.variance=TRUE)$predictions
sqrt_prime_avg =  predict(cf_prime_avg, estimate.variance=TRUE)$variance.estimates
#tree_prime_avg <- get_tree(cf_prime_avg, 5) # get a representative tree out of the forest
#plot(tree_fake_prime)
ATE = mean(tau.hat_prime_avg)
importance = data.frame(CATE=tau.hat_prime_avg,X)
rfcate_fake_echo_avg=randomForest(CATE~., data=importance)
importance(rfcate_fake_echo_avg)

# Bootstrap SE

n_bootstrap <- 500  

# Store bootstrap estimates
boot_cate <- matrix(NA, nrow = n_bootstrap, ncol = length(df_avg$prime))
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
btse = sqrt(sum((boot_ate - mean(boot_ate))^2)/(n_bootstrap-1))

save.image("causal_forest_prime_avg.RData")