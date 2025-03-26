library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)
library(np)
library(ggplot2)
library(policytree)
library(Matching)

# fake new echo chamber
load("~/Downloads/dse4101/Causal Forest Believability for Fake News Outcome/Echo Chamber Treatment/R Data/causal_forest_fake_echo_separate.RData")
dr.scores = double_robust_scores(cf_fake_echo_separate)
tree = policy_tree(X,dr.scores)
plot(tree)

rm(list=ls())
# fake news prime
load("Causal Forest Believability for Fake News Outcome/Prime Treatment/R Data/causal_forest_prime_sep.RData")
dr.scores_fake_prime = double_robust_scores(cf_prime_separate)
tree_fake_prime = policy_tree(X,dr.scores_fake_prime)
plot(tree_fake_prime)
save.image("Optimal Policy Trees/policy_tree.RData")