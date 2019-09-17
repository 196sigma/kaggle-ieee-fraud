rm(list=ls())
gc()
library(glmnet)
source("code/utils.R")
load("data/model-data.Rda")
load("data/test-data.Rda")

## L1-Penalized Logistic
X <- na.omit(X)
y <- X$isfraud
X$isfraud <- NULL
X <- as.matrix(X)

logist.model <- glmnet(X, y, family = "binomial", alpha=1, lambda=0.01, standardize = TRUE); coef(logist.model)

library(rpart)
rpart(isfraud ~ ., data = X)
