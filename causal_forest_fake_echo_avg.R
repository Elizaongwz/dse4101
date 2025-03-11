library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)
library(ggplot2)

load("clean.RData")
set.seed(123)
# Klein and Spady will be used to estimate propensity scores
# Fake news accuracy
### echo chamber as treatment
## external, internal efficacy and individual time spent on social media apps separated

X = select(df_avg,polint2, income:effavg, infopros, infoproh)
ks_model = npindex(df_avg$echofake ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+socialavg+knowscale+effavg+infopros+infoproh, data=df_avg, method="kleinspady")
W.hat = fitted(ks_model)


cf_fake_echo_avg = causal_forest(X=X,Y=df_avg$totalfakeavg,W=df_avg$echofake,W.hat=W.hat, seed=1234)
tau.hat_fake_echo_avg = predict(cf_fake_echo_avg, estimate.variance=TRUE)$predictions
sqrt_fake_echo_avg =  predict(cf_fake_echo_avg, estimate.variance=TRUE)$variance.estimates
tree_fake_echo_avg <- get_tree(cf_fake_echo_avg, 5) # get a representative tree out of the forest
plot(tree_fake_echo_avg)
ATE = mean(tau.hat_fake_echo_avg)
importance = data.frame(CATE=tau.hat_fake_echo_avg,X)
rfcate_fake_echo_avg=randomForest(CATE~., data=importance)
importance(rfcate_fake_echo_avg)

# Histogram of CATEs
ggplot(data.frame(CATE = tau.hat_fake_echo_avg), aes(x = CATE)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6, color = "black") +
  labs(title = "Distribution of CATE Estimates", x = "CATE", y = "Frequency") +
  theme_minimal()

# political knowledge
cate_knowledge <- importance %>%
  group_by(know_bin = cut(df_avg$knowscale, breaks = 10)) %>%  # Bin age into groups
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_knowledge, aes(x = know_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +  # One dot per category
  geom_line(group = 1, color = "red") +  # Connect points with a line
  labs(title = "Average CATE by Political Knowledge", 
       x = "Political Knowledge", 
       y = "Average CATE") +
  theme_minimal()

# age
cate_age <- importance %>%
  group_by(age_bin = cut(df_avg$age, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_age, aes(x = age_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Age Group", x = "Age Group", y = "Average CATE") +
  theme_minimal()

# heuristic information processing
cate_infoproh <- importance %>%
  group_by(infoproh_bin = cut(df_avg$infoproh, breaks = 5)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_infoproh, aes(x = infoproh_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Heurisic Information Processing", x = "Heuristic Information Processing", y = "Average CATE") +
  theme_minimal()

# average social media use
cate_social <- importance %>%
  group_by(social_bin = cut(df_avg$socialavg, breaks = 10)) %>%  
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_social, aes(x = social_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Social Media Use", x = "Average Social Media Use", y = "Average CATE") +
  theme_minimal()

# magnifying results that go against hypothesis
summary(lm(formula = infoproh ~ socialavg, data = df_avg))
# it seems like using more social media increases heuristic information processing


save.image("causal_forest_fake_echo_avg.RData")