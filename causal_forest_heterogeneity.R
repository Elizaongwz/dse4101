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
### breaking up of echo chambers will be used as treatment
## external, internal efficacy and individual time spent on social media apps separated

X = select(df_het,polint2, income:effint, infopros, infoproh)
ks_model = npindex(df_het$hetero ~ polint2+income+newsattention+age+edu+male+white+vote16+republican+facebook+insta+twitter+pin+snap+reddit+knowscale+effext+effint+infopros+infoproh, data=df_het, method="kleinspady")
W.hat = fitted(ks_model)

cf_hetero = causal_forest(X=X,Y=df_het$totalfakeavg,W=df_het$hetero,W.hat=W.hat, seed=1234)
tau.hat_hetero = predict(cf_hetero, estimate.variance=TRUE)$predictions
sqrt_hetero =  predict(cf_hetero, estimate.variance=TRUE)$variance.estimates
#tree_fake_echo_sep <- get_tree(cf_fake_echo_separate, 5) # get a representative tree out of the forest
#plot(tree_fake_echo_sep)
ATE = mean(tau.hat_hetero)
importance = data.frame(CATE=tau.hat_hetero,X)
rfcate_hetero=randomForest(CATE~., data=importance)
importance(rfcate_hetero)

# political knowledge
cate_knowledge <- importance %>%
  group_by(knowscale) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_knowledge, aes(x = knowscale, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +  # One dot per category
  geom_line(group = 1, color = "red") +  # Connect points with a line
  labs(title = "Average CATE by Political Knowledge", 
       x = "Political Knowledge", 
       y = "Average CATE") +
  theme_minimal()

# average snapchat use
cate_snapchat <- importance %>%
  group_by(snap_bin = cut(df_het$snap, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_snapchat, aes(x = snap_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Snapchat Use", x = "Average Snapchat Use", y = "Average CATE") +
  theme_minimal()

# heuristic information processing
cate_infoproh <- importance %>%
  group_by(infoproh_bin = cut(df_het$infoproh, breaks = 5)) %>% 
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_infoproh, aes(x = infoproh_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Heurisic Information Processing", x = "Heuristic Information Processing", y = "Average CATE") +
  theme_minimal()

# average pinterest use
cate_pin <- importance %>%
  group_by(pin_bin = cut(df_het$pin, breaks = 10)) %>%
  summarise(mean_CATE = mean(CATE, na.rm = TRUE))

ggplot(cate_pin, aes(x = pin_bin, y = mean_CATE)) +
  geom_point(size = 4, color = "blue") +
  geom_line(group = 1, color = "red") +
  labs(title = "Average CATE by Average Pinterest Use", x = "Average Pinterest Use", y = "Average CATE") +
  theme_minimal()

save.image("causal_forest_fake_echo_separate.RData")