rm(list=ls())
gc()
source("code/utils.R")
load("data/model-data.Rda")
## Baseline rate of fraud
mean(X$isfraud)
## 3.5%

## TODO
## * log transormations

## export summary statistics
summarize <- function(x){
  x <- na.omit(x)
  percentiles <- quantile(x, probs = seq(0,1,.01))
  
  p1 <- percentiles[["1%"]]
  p5 <- percentiles[["5%"]]
  p95 <- percentiles[["95%"]]
  p99 <- percentiles[["99%"]]
  return(c(mean(x), sd(x), min(x), max(x), p1,p5,median(x),p95,p99))
}
summarize(X$transactionamt)
summary.stats.df <- data.frame(matrix(ncol = 10))
names(summary.stats.df) <- c("x", "mean", "sd", "min", "max", "p01", "p05", "med", "p95", "p99")
for(v in names(X)){
  summary.stats.df <- rbind(summary.stats.df, c(v, summarize(X[,v])))
}
summary.stats.df <- na.omit(summary.stats.df)

## investigate fraud rate by decile
decile <- bucket(X$transactionamt, n=10)
y <- X[, c("isfraud", "transactionamt")]
y$decile <- decile
fraud.by.decile <- aggregate(y$isfraud, by = list(y$decile), FUN = mean, na.rm = TRUE)
names(fraud.by.decile) <- c("decile", "isfraud.pct")
par(mfrow=c(1,1))
barplot(fraud.by.decile$isfraud.pct)

par(mfrow=c(6,6))
for(i in 1:36){
  hist(X[,i], main=NA)
  title(names(X)[i])
}

for(i in 37:(37+35)){
  hist(X[,i], main=NA)
  title(names(X)[i])
}
par(mfrow=c(1,1))
hist(log10(X$v110))
