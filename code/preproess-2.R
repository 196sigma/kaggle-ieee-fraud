rm(list=ls())
gc()
library(caret)
set.seed(3456)
library(glmnet)
source("code/utils.R")

## Load raw data
train.identity <- read.csv("data/raw/train_identity.csv", na.strings = "")
names(train.identity) <- tolower(names(train.identity))

train.transaction <- read.csv("data/raw/train_transaction.csv", na.strings = "")
names(train.transaction) <- tolower(names(train.transaction))

test.transaction <- read.csv("data/raw/test_transaction.csv", na.strings = "")
names(test.transaction) <- tolower(names(test.transaction))

test.identity <- read.csv("data/raw/test_identity.csv", na.strings = "")
names(test.identity) <- tolower(names(test.identity))

X.test <- merge(test.transaction, test.identity, all.x=TRUE)

X.train <- merge(train.transaction, train.identity)
y.train <- X.train[, c("transactionid", "isfraud")]
X.train$isfraud <- NULL

## check percent missing
train.names <- as.character(names(X.train))
miss.df <- data.frame(v = train.names)
for(v in train.names) miss.df[miss.df$v == v, "nmiss"] <- length(which(is.na(X.train[,v])))
miss.df$pctmiss <- miss.df$nmiss/nrow(X.train)
miss.df <- miss.df[order(miss.df$pctmiss, decreasing = TRUE), ]
drops <- as.character(miss.df[which(miss.df$pctmiss > .3), "v"])

## check low variance variables
var.df <- data.frame(v = train.names)
var.df$var <- NA
var.df$sd <- NA
for(v in train.names){
  if(is.numeric(X.train[,v])){
    var.df[var.df$v == v, "var"] <- var(X.train[,v], na.rm = TRUE)
    var.df[var.df$v == v, "sd"] <- sd(X.train[,v], na.rm = TRUE)
  }else{
    if(all(duplicated(X.train[,v])[-1])){
      drops <- c(drops, v)
    }
  }
} 

var.df <- var.df[order(var.df$var), ]
var.df <- var.df[-which(is.na(var.df$var)),]
drops <- c(drops, var.df[which(var.df$sd < 0.0001), "v"]); drops

for(v in drops){
  X.train[,v] <- NULL
  X.test[,v] <- NULL
}

## collapse device info
X.train$deviceinfo_normalized <- sapply(X.train$deviceinfo, getDeviceInfo)
X.train$deviceinfo_normalized <- ifelse(is.na(X.train$deviceinfo_normalized), 
                                        "missing", 
                                        X.train$deviceinfo_normalized)
top.devices <- data.frame(table(X.train$deviceinfo_normalized))
top.devices <- top.devices[order(top.devices$Freq, decreasing = TRUE), ]
top.devices <- as.character(top.devices[1:20, 1])
for(i in 1:nrow(X.train)){
  if(!(X.train[i,"deviceinfo_normalized"] %in% top.devices)) X.train[i,"deviceinfo_normalized"] <- "other"
}

X.test$deviceinfo_normalized <- sapply(X.test$deviceinfo, getDeviceInfo)
X.test$deviceinfo_normalized <- ifelse(is.na(X.test$deviceinfo_normalized), 
                                       "missing", X.test$deviceinfo_normalized)
for(i in 1:nrow(X.test)){
  if(!(X.test[i,"deviceinfo_normalized"] %in% top.devices)) X.test[i,"deviceinfo_normalized"] <- "other"
}

table(X.train$deviceinfo_normalized)
table(X.test$deviceinfo_normalized)

X.train$deviceinfo <- NULL
X.test$deviceinfo <- NULL

## collapsew browser version
X.train$browser <- sapply(X.train$id_31, getDeviceInfo)
X.train$browser <- ifelse(is.na(X.train$browser), "missing", X.train$browser)
X.train$id_31 <- NULL

X.test$browser <- sapply(X.test$id_31, getDeviceInfo)
X.test$browser <- ifelse(is.na(X.test$browser), "missing", X.test$browser)
X.test$id_31 <- NULL

## deal with missing values in factors
for(v in colnames(X.train)){
  if(is.factor(X.train[,v])){
    X.train[,v] <- as.character(X.train[,v])
    X.train[,v] <- replace(X.train[,v], is.na(X.train[,v]), "missing")
  }
}
for(v in colnames(X.test)){
  if(is.factor(X.test[,v]) | is.character(X.test[,v])){
    X.test[,v] <- as.character(X.test[,v])
    X.test[,v] <- replace(X.test[,v], is.na(X.test[,v]), "missing")
  }
}

## Split numeric and factor variables
X.train.numeric <- data.frame(transactionid = X.train[["transactionid"]])
X.test.numeric <- data.frame(transactionid = X.test[["transactionid"]])
X.train.factor <- data.frame(transactionid = X.train[["transactionid"]])
X.test.factor <- data.frame(transactionid = X.test[["transactionid"]])
for(v in names(X.train)){
  if( is.character(X.train[,v]) | is.factor(X.train[,v])){
    X.train.factor[,v] <- X.train[,v]
  }else{
    X.train.numeric[,v] <- X.train[,v]
  }
}

for(v in names(X.test)){
  if( is.character(X.test[,v]) | is.factor(X.test[,v])){
    X.test.factor[,v] <- X.test[,v]
  }else{
    X.test.numeric[,v] <- X.test[,v]
  }
}

## deal with new factor levels in test data
for(v in colnames(X.test.factor)[-1]){
  x <- unique(X.train[,v])
  X.test.factor[,v] <- ifelse(X.test.factor[,v] %in% x, X.test.factor[,v], "other")
}

## one-hot encode factors
options(na.action='na.pass')
#dmy <- dummyVars(" ~ .", data = X.train.factor[,-1])
#dmy.train <- data.frame(predict(dmy, newdata = X.train.factor[,-1]))
dmy.train <- model.matrix(~ -1 + ., data = X.train.factor[,-1])
X.train2 <- cbind(X.train, dmy.train)
X <- merge(y.train, X.train2, by = "transactionid")
X$transactionid <- NULL
X$transactiondt <- NULL

## Remove factors that have been turned into dummies
for(v in colnames(X)) if(is.character(X[,v]) | is.factor(X[,v])) X[,v] <- NULL

## TODO: there needs to be a dummy variable X_other for each factor X
#dmy.test <- data.frame(predict(dmy, newdata = X.test.factor[,-1])); dim(dmy.test)
dmy.test <- data.frame(model.matrix(~ -1 + ., data = X.test.factor[,-1]))

for(v in colnames(dmy.test)){
  if(!(v %in% colnames(dmy.train))) dmy.test[,v] <- NULL
}
for(v in colnames(dmy.train)){
  if(!(v %in% colnames(dmy.test))) dmy.test[,v] <- 0
}
X.test2 <-cbind(X.test.numeric, dmy.test)
X.test2$transactionid <- NULL
X.test2$transactiondt <- NULL


## Save model data
train_data <- X
test_data <- X.test2

## no variation
drops <- c()
for(v in names(train_data)){
  if( min(train_data[,v], na.rm = TRUE) == max(train_data[,v], na.rm = TRUE)) drops <- c(drops,v)
}
for(v in drops){
  train_data[,v] <- NULL
  test_data[,v] <- NULL
}
##

###################################################################################################
## setup data for modeling
###################################################################################################
##
y <- train_data[, "isfraud"]; table(y)
X1 <- train_data
##
X <- train_data
X$isfraud <- NULL

## replace missing in training data with mean of variable
for(v in names(X)) X[,v] <- ifelse(is.na(X[,v]), mean(X[,v], na.rm=TRUE), X[,v])
train_data <- as.matrix(X)

## generate own CV samples
train_data <- data.frame(train_data)
train_data$isfraud <- y
trainIndex <- createDataPartition(train_data$isfraud, p = .8,
                                  list = FALSE,
                                  times = 1)
val_data <- train_data[-trainIndex,]
train_data <- train_data[trainIndex,]
mean(val_data$isfraud)
mean(train_data$isfraud)

## check low variance variables
train.names <- as.character(names(train_data))
var.df <- data.frame(v = train.names)
var.df$var <- NA
var.df$sd <- NA
for(v in train.names){
  if(is.numeric(train_data[,v])){
    var.df[var.df$v == v, "var"] <- var(train_data[,v], na.rm = TRUE)
    var.df[var.df$v == v, "sd"] <- sd(train_data[,v], na.rm = TRUE)
  }else{
    if(all(duplicated(train_data[,v])[-1])){
      drops <- c(drops, v)
    }
  }
} 

var.df <- var.df[order(var.df$var), ]
o <- which(is.na(var.df$var))
if(length(o)>0) var.df <- var.df[-o,]
drops <- as.character(var.df[which(var.df$sd < 0.01), "v"]); drops

for(v in drops){
  train_data[,v] <- NULL
  val_data[,v] <- NULL
  test_data[,v] <- NULL
}
rm(list=c("v", "var.df", "drops"))


# one hot encode classes / create DummyFeatures
train_labels <- to_categorical(train_data$isfraud)
val_labels <- to_categorical(val_data$isfraud)

## Use means and standard deviations from training set to normalize test set
## TODO sep factors before scaling?

col_means_train <- vector()
col_stddevs_train <- vector()
for(v in colnames(train_data)){ 
  col_means_train[v] <- mean(train_data[,v])
  col_stddevs_train[v] <- sd(train_data[,v])
}
attr(train_data, "scaled:center") <- col_means_train
attr(train_data, "scaled:scale") <- col_stddevs_train

## replace missing in test data with mean from training data
for(v in names(test_data)) test_data[,v] <- ifelse(is.na(test_data[,v]), 
                                                   col_means_train[v], 
                                                   test_data[,v])

X1 <- test_data
test_data <- X1
test_data$p_emaildomainservicios.ta.com <- test_data["p_emaildomainservicios-ta.com"]
test_data$r_emaildomainservicios.ta.com <- test_data["r_emaildomainservicios-ta.com"]
test_data[,"p_emaildomainservicios-ta.com"] <- NULL
test_data[,"r_emaildomainservicios-ta.com"] <- NULL

## scale testing data
for(v in names(test_data)) test_data[,v] <- (test_data[,v] - col_means_train[v])/col_stddevs_train[v]
test_data <- as.matrix(test_data)

## scale validation data
for(v in names(val_data)) val_data[,v] <- (val_data[,v] - col_means_train[v])/col_stddevs_train[v]
val_data <- as.matrix(val_data)

###################################################################################################
## save data
###################################################################################################

## testing
write.csv(x=test_data, 
          file="data/train-data.csv",
          quote=FALSE,
          row.names=FALSE)
save(test_data, file="data/test-data.Rda")

## training
write.csv(x=train_data, 
          file="data/train-data.csv",
          quote=FALSE,
          row.names=FALSE)
write.csv(x=train_labels, 
          file="data/val-labels.csv",
          quote=FALSE,
          row.names=FALSE)
save(train_data, file="data/train-data.Rda")
save(train_labels, file="data/train-labels.Rda")

## validation
write.csv(x=val_data, 
          file="data/train-data.csv",
          quote=FALSE,
          row.names=FALSE)
write.csv(x=val_labels, 
          file="data/val-labels.csv",
          quote=FALSE,
          row.names=FALSE)
save(val_data, file="data/val-data.Rda")
save(val_labels, file="data/val-labels.Rda")



## Linear prob model
load("data/model-data.Rda")
m <- lm(isfraud ~ ., data = X); summary(m)
ypred <- predict(m, data.frame(test_data))
hist(ypred)
ypred2 <- ifelse(ypred < .5, 0, 1); table(ypred2)

## Naive baseleine
sample.submission$isFraud <- mean(train.transaction$isFraud)
x <- sample.submission
make.submission(x, "naive baseline")
## Achieves LB position of 2215/2226; AUROC of .5
