rm(list=ls())
gc()
library(caret)
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

new_data <- merge(test.transaction, test.identity, all.x=TRUE)
save(x=new_data, file="data/test-data.Rda")

sample.submission <- read.csv("data/raw/sample_submission.csv")

X.train <- merge(train.transaction, train.identity)

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
  var.df[var.df$v == v, "var"] <- var(X.train[,v], na.rm = TRUE)
  var.df[var.df$v == v, "sd"] <- sd(X.train[,v], na.rm = TRUE)
} 

var.df <- var.df[order(var.df$var), ]
var.df <- var.df[-which(is.na(var.df$var)),]
drops <- c(drops, var.df[which(var.df$sd < 0.0001), "v"]); drops

## Collapse device info column
getDeviceInfo <- function(x){
  x1 <- as.character(x)
  x1 <- gsub("(-|:|_|/)"," ",x1)
  #x1 <- strsplit(x1, split = "-")[[1]][1]
  device.info <- strsplit(x1, split = " ")[[1]][1]
  device.info <- tolower(device.info)
  if(is.na(device.info)) device.info <- "missing"
  return(device.info)
}

deviceinfo <- sapply(train.identity$deviceinfo, getDeviceInfo)
deviceinfo.df <- data.frame(table(deviceinfo))
names(deviceinfo.df) <- c("deviceinfo2", "freq")
deviceinfo.df <- deviceinfo.df[order(deviceinfo.df$freq, decreasing = TRUE), ]
deviceinfo.df <- deviceinfo.df[order(deviceinfo.df$deviceinfo2, decreasing = TRUE), ]
write.csv(x=deviceinfo.df, file="data/deviceinfo.csv", row.names = FALSE, quote = FALSE)


## sample training data
#n <- 10000
#X.train.transaction <- train.transaction[1:n,]
#X.train.identity <- train.identity[1:n,]
#X.train <- merge(X.train.transaction, X.train.identity)
y.train <- X.train[,c("transactionid", "isfraud")]
X.train$isfraud <- NULL
mean(y.train$isfraud)
for(v in drops) X.train[,v] <- NULL


## manually normalize device info in Excel
deviceinfo.df.normalized <- read.csv("data/deviceinfo-normalized.csv", stringsAsFactors = FALSE)
deviceinfo.df.normalized$deviceinfo_normalized <- ifelse(deviceinfo.df.normalized$freq <= 10, 
                                                         "other", 
                                                         deviceinfo.df.normalized$deviceinfo_normalized)

## Merge normalized device info field
X.train$deviceinfo2 <- sapply(X.train$deviceinfo, getDeviceInfo)
X.train <- merge(X.train, 
                 deviceinfo.df.normalized, 
                 by.x = "deviceinfo2", 
                 by.y = "deviceinfo2", all.x = TRUE)

X.train$deviceinfo <- NULL
X.train$deviceinfo2 <- NULL
X.train$deviceinfo_normalized <- ifelse(is.na(X.train$deviceinfo_normalized), "other", X.train$deviceinfo_normalized)

## collapse id_33
getScreen <- function(x, which="w"){
  x <- as.character(x)
  resolution <- unlist(strsplit(x, "x"))
  if(which=="w") screen_w <- as.integer(resolution[1])
  if(which=="h") screen_w <- as.integer(resolution[2])
  return(screen_w)
}

screen_w <- sapply(train.identity$id_33, getScreen, which="w")
screen_h <- sapply(train.identity$id_33, getScreen, which="h")
resolution.df <- data.frame(train.identity["transactionid"])
resolution.df$screen_w <- screen_w
resolution.df$screen_h <- screen_h
resolution.df <- resolution.df[complete.cases(resolution.df),]

resolution.df$screen_w2 <- round(resolution.df$screen_w/100)
resolution.df$screen_h2 <- round(resolution.df$screen_h/100)
X.train <- merge(X.train, resolution.df, by.x = "transactionid",by.y="transactionid",all.x=TRUE)
X.train$id_33 <- NULL

## collaps browser version
foo <- function(x) unlist(strsplit(as.character(x), " "))[1]
X.train$browser <- sapply(X.train$id_31, getDeviceInfo)
X.train$browser <- ifelse(is.na(X.train$browser), "other", X.train$browser)
table(X.train$browser)
X.train$id_31 <- NULL

## collapse os version
#X.train$osversion <- sapply(X.train$id_30, foo)
#X.train$osversion <- ifelse(is.na(X.train$osversion), "other", X.train$osversion)
#table(X.train$osversion)
#X.train$id_30 <- NULL

## deal with email domains
X.train$p_emaildomain <- NULL
X.train$r_emaildomain <- NULL

## code device type
X.train$devicetype <- as.character(X.train$devicetype)
X.train$devicetype <- ifelse(X.train$devicetype == "", "missing", X.train$devicetype)
table(X.train$devicetype)

## Split numeric and factor variables
X.train.numeric <- data.frame(X.train[["transactionid"]])
X.train.factor <- data.frame(X.train[["transactionid"]])

for(v in names(X.train)){
  if( is.character(X.train[,v])){
    X.train.factor[,v] <- X.train[,v]
  }else{
    X.train.numeric[,v] <- X.train[,v]
  }
}

## one-hot encode factors
dmy <- dummyVars(" ~ .", data = X.train.factor[,-1])
trsf <- data.frame(predict(dmy, newdata = X.train.factor[,-1]))
names(trsf)
X.train2 <- cbind(X.train, trsf)


X <- merge(y.train, X.train2, by = "transactionid")
X$transactionid <- NULL
X$transactiondt <- NULL
for(v in names(X)) if(is.character(X[,v])) X[,v] <- NULL
for(v in names(X)) if(is.factor(X[,v])) X[,v] <- NULL

## Save model data
write.csv(x=X, 
          file="data/model-data.csv",
          quote=FALSE,
          row.names=FALSE)
save(X, file="data/model-data.Rda")
load("data/model-data.Rda")
## Linear prob model
summary(lm(isfraud ~ ., data = X))


## Naive baseleine
sample.submission$isFraud <- mean(train.transaction$isFraud)
x <- sample.submission
make.submission(x, "naive baseline")
## Achieves LB position of 2215/2226; AUROC of .5




## NN
x.train <- train.transaction
x.train$TransactionID <- NULL
x.train$isFraud <- NULL
y.train <- train.transaction$isFraud
x.test <- test.transaction
x.test$TransactionID <- NULL

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(392)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = 'accuracy'
)

history <- model %>% fit(
  x.train, y.train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)
?model.matrix
x <- x.train[1:10,]
x$M1 <- 1*x$M1
x$M2 <- 1*x$M2
x$M3 <- 1*x$M3
x$M4 <- NULL
x$M5 <- 1*x$M5
x$M6 <- 1*x$M6
x$M7 <- 1*x$M7
x$M8 <- 1*x$M8
x$M9 <- 1*x$M9

model.matrix(~ -1 + TransactionAmt + ProductCD + card4 + P_emaildomain + R_emaildomain, x)
model.matrix(~ -1 + TransactionAmt + ProductCD, x)
model.matrix(~ -1 + ., x)
