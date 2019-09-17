## function for making submissions
make.submission <- function(x, description, submission.directory = "output/submissions"){
  submission.id = format(Sys.time(), "%m%d_%H%M%S"); submission.id
  description.table = c(submission.id, "|", description)
  write(x = description.table, 
        file = paste(submission.directory, "submission-descriptions.txt", sep = "/"), 
        ncolumns = 3, append = TRUE)
  
  sample.submission <- read.csv("data/raw/sample_submission.csv")
  sample.submission$isFraud <- x
  filename <- paste(submission.directory, "/", submission.id, ".csv", sep = "")
  write.csv(sample.submission, 
            file = filename, 
            quote = FALSE, 
            row.names = FALSE)
  print(filename)
}

bucket <- function(x, n){
  #x <- seq(1,100,length.out = 50); n = 10
  ntiles <- as.integer(cut(x, quantile(x, probs= seq(0,n)/n), include.lowest=TRUE))

}


getDeviceInfo <- function(x){
  x1 <- as.character(x)
  x1 <- gsub("(-|:|_|/)"," ",x1)
  #x1 <- strsplit(x1, split = "-")[[1]][1]
  device.info <- strsplit(x1, split = " ")[[1]][1]
  device.info <- tolower(device.info)
  if(is.na(device.info)) device.info <- "missing"
  return(device.info)
}


get_dummies <- function(x, prefix){
  x <- as.factor(x)
  contr <- contrasts(x)
  n <- ncol(contr)
  dummy.names <- paste(rep(prefix,n), colnames(contr), sep="_")
  colnames(contr) <- dummy.names
  return(contr)
}
