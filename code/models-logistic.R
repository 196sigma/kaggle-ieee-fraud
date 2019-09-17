rm(list=ls())
gc()
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
X.train$transactiondt <- NULL
X.train$transactionid <- NULL

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

###################################################################################################
## Logistic Regression
###################################################################################################
for(v in names(X.train))if(is.factor(X.train[,v])){
  X.train[,v] <- as.factor(X.train[,v])
  X.test[,v] <- as.factor(X.test[,v])
}
X <- data.frame(y.train[,2])
X <- cbind(X, X.train)
logistic.model <- glm(y ~ ., data = X, family="binomial"); summary(logistic.model)

###################################################################################################
## L1-Penalized Logistic
###################################################################################################
load("data/train-data.Rda")
load("data/test-data.Rda")

y <- train_data$isfraud
train_data$isfraud <- NULL
train_data <- as.matrix(train_data); dim(train_data)
for(i in 1:ncol(train_data)) train_data[,i] <- ifelse(is.na(train_data[,i]), mean(train_data[,i], na.rm=TRUE), train_data[,i])
l1.logistic.model <- glmnet(train_data, 
                         y, 
                         family = "binomial", 
                         alpha=.25, 
                         lambda=0.03, 
                         standardize = TRUE); coef(l1.logistic.model)

save(X=logistic.model, file="output/models/logistic-model")

library(magrittr)
(526/2) %>% `/`(2)
