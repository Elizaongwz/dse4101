library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)

load("clean.RData")
set.seed(123)
# Klein and Spady will be used to estimate propensity scores
# Fake news accuracy
### echo chamber as treatment
## external, internal efficacy and individual time spent on social media apps separated

X = select(df1,polint2, income:effint, infopros, infoproh)
#Y.forest = regression_forest(X,df1$totalfakeavg, num.trees = 500, min.node.size = 5)
#Y.hat = predict(Y.forest)$predictions
ks_model = npindex(df1$echofake ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+facebook+insta+twitter+pin+snap+reddit+knowscale+effext+effint+infopros+infoproh, data=df1, method="kleinspady")
W.hat = fitted(ks_model)
#W.forest = regression_forest(X, df1$echofake, num.trees = 500, min.node.size = 5)
#W.hat = predict(W.forest)$predictions

cf_fake_echo_separate = causal_forest(X=X,Y=df1$totalfakeavg,W=df1$echofake,W.hat=W.hat, seed=1234)
tau.hat_fake_echo_sep = predict(cf_fake_echo_separate, estimate.variance=TRUE)$predictions
sqrt_fake_echo_sep =  predict(cf_fake_echo_separate, estimate.variance=TRUE)$variance.estimates
#tree_fake_echo_sep <- get_tree(cf_fake_echo_separate, 5) # get a representative tree out of the forest
#plot(tree_fake_echo_sep)
ATE = mean(tau.hat_fake_echo_sep)
importance = data.frame(CATE=tau.hat_fake_echo_sep,X)
rfcate_fake_echo_sep=randomForest(CATE~., data=importance)
importance(rfcate_fake_echo_sep)

# average snapchat use
cate_snapchat <- importance %>%
  group_by(age_bin = cut(df1$snap, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_snapchat, aes(x = age_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Snapchat Use", x = "Average Snapchat Use", y = "Average CATE") +
  theme_minimal()

# heuristic information processing
cate_infoproh <- importance %>%
  group_by(infoproh_bin = cut(df1$infoproh, breaks = 5)) %>% 
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_infoproh, aes(x = infoproh_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Heurisic Information Processing", x = "Heuristic Information Processing", y = "Average CATE") +
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

# political knowledge
cate_knowledge <- importance %>%
  group_by(know_bin = cut(df1$knowscale, breaks = 10)) %>%  # Bin age into groups
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_knowledge, aes(x = know_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +  # One dot per category
  geom_line(group = 1, color = "red") +  # Connect points with a line
  labs(title = "Average CATE by Political Knowledge", 
       x = "Political Knowledge", 
       y = "Average CATE") +
  theme_minimal()

save(ks_model, file="ks.RData")
save.image("causal_forest_fake_echo_separate.RData")