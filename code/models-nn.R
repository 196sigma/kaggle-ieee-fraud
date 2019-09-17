## Neural Network Models
rm(list=ls())
gc()
source("code/utils.R")
library(keras)
load("data/train-data.Rda"); dim(train_data)
load("data/val-data.Rda"); dim(val_data)
load("data/train-labels.Rda"); dim(train_labels)
load("data/val-labels.Rda"); dim(val_labels)
load("data/test-data.Rda"); dim(test_data)

###################################################################################################
## setup data for modeling
###################################################################################################
X <- train_data
fraud <- which(X$isfraud==1)
notfraud <- which(X$isfraud==0)
o <- c(fraud, sample(notfraud, length(notfraud)*.1))
train_labels <- train_labels[o, ]
X <- X[o, ]
table(X$isfraud)

train_data <- as.matrix(X[, -which(colnames(X) == "isfraud")])
dim(train_data)[2] == dim(test_data)[2]
## TRUE

###################################################################################################
## build the model
###################################################################################################
model.description <- "Baseline neural network model"
model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(train_data), units = 100, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 2, activation = "softmax")
model %>% summary()

# add a loss function and optimizer
model %>%
  compile(
    loss = "binary_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

# fit model with our training data set, training will be done for 200 times data set
fit = model %>%
  fit(
    x = train_data,
    y = train_labels,
    shuffle = T,
    batch_size = 100,
    validation_split = 0.3,
    epochs = 100
  )
ypred = model %>% predict_classes(test_data) ; table(ypred)
ypred %>% make.submission(description = model.description)

#########################################################################################
## better NN
#########################################################################################

model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(train_data), units = 100, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 50, activation = "relu", use_bias=TRUE) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 10, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 2, activation = "softmax")
model %>% summary()

# add a loss function and optimizer
model %>%
  compile(
    loss = "binary_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

fit = model %>%
  fit(
    x = train_data,
    y = train_labels,
    shuffle = T,
    batch_size = 20,
    validation_split = 0.3,
    epochs = 100
  )
model.description <- "Advanced neural network model with all features"

## save model 
model %>% save_model_hdf5("output/models/adv-nn.h5")

ypred = model %>% predict_classes(test_data) ; table(ypred)
class_probs <- model$predict(test_data, use_multiprocessing=TRUE); summary(class_probs)
ypred %>% make.submission(description = model.description)
