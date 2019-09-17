rm(list=ls())
gc()
source("code/utils.R")

## load data
load("data/train-data-transformed.Rda")
load("data/train-labels-transformed.Rda")
load("data/test-data-transformed.Rda")

## Ensemble models

## load NN
nn.model <- load_model_hdf5("output/models/adv-nn.h5")

nn.ypred = nn.model %>% predict_classes(x_test) ; table(ypred)
nn.class_probs <- nn.model$predict(x_test, use_multiprocessing=TRUE)
summary(nn.class_probs[,1])
summary(nn.class_probs[,2])
hist(nn.class_probs[,2])
nn.predictions <- cbind(nn.ypred, nn.class_probs)
colnames(nn.predictions) <- c("class.pred", "prob0", "prob1")
nn.predictions <- data.frame(nn.predictions); head(nn.predictions)
head(nn.predictions)

## logistic

## load lin prob model