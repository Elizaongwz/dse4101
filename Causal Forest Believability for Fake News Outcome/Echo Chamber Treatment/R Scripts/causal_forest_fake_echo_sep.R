library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)
library(pracma)

load("Datasets/Cleaning Scripts and R Data/clean.RData")
set.seed(123)
# Fake news accuracy
### echo chamber as treatment
## external, internal efficacy and individual time spent on social media apps separated

X = dplyr::select(df1,polint2, income:effint, infopros, infoproh)
ps = glm(df1$echofake ~ polint2+income+newsattention+age+edu+male+vote16+white+republican+facebook+insta+twitter+pin+snap+reddit+knowscale+effext+effint+infopros+infoproh, data=df1,family=binomial('logit'))$fitted
summary(ps[df1$echofake==1])
summary(ps[df1$echofake==0]) #good matches found

# Omnibus Test for Heterogeneity
test_calibration(cf_fake_echo_separate) #reasonably calibrated
cf_fake_echo_separate = causal_forest(X=X,Y=df1$totalfakeavg,W=df1$echofake,W.hat=ps, seed=1234)
tau.hat_fake_echo_sep = predict(cf_fake_echo_separate, estimate.variance=TRUE)$predictions
sqrt_fake_echo_sep =  predict(cf_fake_echo_separate, estimate.variance=TRUE)$variance.estimates
#tree_fake_echo_sep <- get_tree(cf_fake_echo_separate, 5) # get a representative tree out of the forest
#plot(tree_fake_echo_sep)

# Estimate Average Treatment Effect (ATE)
ATE = average_treatment_effect(cf_fake_echo_separate)
cat("95% CI for the ATE:", round(ATE[1], 3), "+/-", round(qnorm(0.975) * ATE[2], 3), "\n")

# Cumulative Gain Plot
df1$CATE <- tau.hat_fake_echo_sep
df1 <- df1[order(df1$CATE, decreasing = TRUE), ]
df1$quantile <- cut(seq(1, nrow(df1)), breaks = 10, labels = FALSE)  
gain_data <- aggregate(df1$totalfakeavg, by = list(df1$quantile), FUN = sum)
gain_data$cum_population <- seq(1, 10) / 10 

# Plot cumulative gains chart
ggplot(gain_data, aes(x = cum_population, y = x)) +
  geom_line(color = "black") +  # Main curve
  geom_point(color = "black") + # Dots at each quantile
  geom_line(data= gain_data %>% slice(1,n()))+ 
  labs(x = "Proportion of Population", y = "Cumulative Gain", title = "Cumulative Gain Curve") +
  theme_minimal()

auc = trapz(gain_data$cum_population,gain_data$x) #4628.625

# Importance
importance = data.frame(CATE=tau.hat_fake_echo_sep,X)
rfcate_fake_echo_sep=randomForest(CATE~., data=importance)
importance(rfcate_fake_echo_sep)
var_imp = importance(rfcate_fake_echo_sep)
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

# average snapchat use
cate_snapchat <- importance %>%
  group_by(age_bin = cut(df1$snap, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_snapchat, aes(x = age_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Snapchat Use", x = "Average Snapchat Use", y = "Average CATE") +
  theme_minimal()

boxplot(tau.hat_fake_echo_sep ~ round(df1$snap), xlab = "Average Snapchat Use", ylab = "Estimated CATE")
spline_fit <- smooth.spline(df1$snap, tau.hat_fake_echo_sep)
extended_x <- seq(min(df1$snap), max(df1$snap) + 1, length.out = 100)
spline_pred <- predict(spline_fit, extended_x)
lines(spline_pred$x, spline_pred$y, lwd = 3, col = 4)

# Average Reddit use
cate_reddit <- importance %>%
  group_by(reddit_bin = cut(df1$reddit, breaks = 5)) %>% 
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_reddit, aes(x = reddit_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Reddit Use", x = "Average Reddit Use", y = "Average CATE") +
  theme_minimal()

boxplot(tau.hat_fake_echo_sep ~ round(df1$reddit), xlab = "Average Reddit Use", ylab = "Estimated CATE")
spline_fit <- smooth.spline(df1$reddit, tau.hat_fake_echo_sep)
extended_x <- seq(min(df1$reddit), max(df1$reddit) + 1, length.out = 100)
spline_pred <- predict(spline_fit, extended_x)
lines(spline_pred$x, spline_pred$y, lwd = 3, col = 4)

# average Instagram use
cate_insta <- importance %>%
  group_by(insta_bin = cut(df1$insta, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_insta, aes(x = insta_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Instagram Use", x = "Average Instagram Use", y = "Average CATE") +
  theme_minimal()

boxplot(tau.hat_fake_echo_sep ~ round(df1$insta), xlab = "Average Instagram Use", ylab = "Estimated CATE")
spline_fit <- smooth.spline(df1$insta, tau.hat_fake_echo_sep)
extended_x <- seq(min(df1$insta), max(df1$insta) + 1, length.out = 100)
spline_pred <- predict(spline_fit, extended_x)
lines(spline_pred$x, spline_pred$y, lwd = 3, col = 4)

# Internal Efficacy
cate_knowledge <- importance %>%
  group_by(effint_bin = cut(df1$effint, breaks = 10)) %>%  # Bin age into groups
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_knowledge, aes(x = effint_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +  # One dot per category
  geom_line(group = 1, color = "red") +  # Connect points with a line
  labs(title = "Average CATE by Internal Efficacy", 
       x = "Internal Efficacy", 
       y = "Average CATE") +
  theme_minimal()

# Bootstrap SE

n_bootstrap <- 500  

# Store bootstrap estimates
boot_cate <- matrix(NA, nrow = n_bootstrap, ncol = length(df1$echofake))
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
btse_echofake_sep = sqrt(sum((boot_ate - mean(boot_ate))^2)/(n_bootstrap-1))
t <- ATE[1]/btse_echofake_sep
p_value = 2 * pt(-abs(t), df = length(tau.hat_fake_echo_sep) - 1)
#p_value_echofake_sep <- mean(abs(boot_ate) >= abs(mean(boot_ate)))
print(p_value_echofake_sep)
save.image("Causal Forest Believability for Fake News Outcome/Echo Chamber Treatment/R Data/causal_forest_fake_echo_separate.RData")