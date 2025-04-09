library(causalweight)
library(grf)
library(DiagrammeR)
library(devtools)
library(randomForest)
library(dplyr)

load("Datasets/Cleaning Scripts and R Data/clean.RData")

set.seed(123)

#Echo Chamber
X = dplyr::select(df1,polint2, income:effint, infopros, infoproh)
W = df1$echofake
Y = df1$totalfakeavg

dml_echo = treatDML(y=Y,d=W,x=X, MLmethod='randomforest')

dml_echo$effect
dml_echo$pval

# Prime
X = dplyr::select(df2,polint2, income:effint, infopros, infoproh,-echofake)
W=df2$prime
Y=df2$totalfakeavg

dml_prime = treatDML(y=Y,d=W,x=X,MLmethod='randomforest')
dml_prime$effect
dml_prime$pval

