library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)
library(ggplot2)

load("clean.RData")
set.seed(123)
#load("causal_forest_real_prime_sep.RData")
# Klein and Spady will be used to estimate propensity scores
# real news accuracy
### prime chamber as treatment
## external, internal efficacy and individual time spent on social media apps will be averaged

X = select(df_real,polint2, income:effint, infopros, infoproh,-socialavg,-effavg)
ks_model_real = npindex(df_real$prime ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+pin+facebook+insta+twitter+snap+reddit+knowscale+effext+effint+infopros+infoproh, data=df_real, method="kleinspady")
W.hat = fitted(ks_model_real)

cf_real_prime_avg = causal_forest(X=X,Y=df_real$totalrealavg,W=df_real$prime,W.hat=W.hat, seed=1234)
tau.hat_real_prime_avg = predict(cf_real_prime_avg, estimate.variance=TRUE)$predictions
sqrt_real_prime_avg =  predict(cf_real_prime_avg, estimate.variance=TRUE)$variance.estimates
tree_real_prime_avg <- get_tree(cf_real_prime_avg, 5) # get a representative tree out of the forest
plot(tree_real_prime_avg)
ATE = mean(tau.hat_real_prime_avg)
importance = data.frame(CATE=tau.hat_real_prime_avg,X)
rfcate_real_prime_avg=randomForest(CATE~., data=importance)
importance(rfcate_real_prime_avg)

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

save.image("causal_forest_real_prime_sep.RData")