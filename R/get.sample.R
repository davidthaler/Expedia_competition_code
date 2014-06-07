get.sample <- function(max.rows){
  tr.col.classes <- c("numeric", "character", rep("numeric", 52))
  train <- read.table('data/train.csv', header=T, sep=',', na.strings='NULL',
             colClasses=tr.col.classes, nrows=max.rows)
  list(xtrain=train[,c(1:14, 16:51)], ytrain=train[, c(1,15,52:54)])
}