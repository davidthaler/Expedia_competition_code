require(data.table)

paths <- list('base'='/Users/davidthaler/Documents/Kaggle/expedia/')
paths$data <- paste0(paths$base, 'data/')
paths$sample <- paste0(paths$data, 'sample.csv')
paths$train <- paste0(paths$data, 'train2.csv')
paths$test <- paste0(paths$data, 'test2.csv')

load.expedia <- function(what='sample', basic.clean=TRUE){
 x <- fread(paths[[what]])  
 if(basic.clean){
   x[is.na(x)] <- 0
 }
 if(what %in% c('sample','train')){
   x[, rel:=4*booking_bool + click_bool]
 }
 setkey(x, srch_id)
 x
}

train.val.split <- function(nq.train, nq.val, what='sample', seed=7){
  set.seed(seed)
  x <- load.expedia(what)
  qid <- unique(x$srch_id)
  qid <- sample(qid, length(qid))
  xtr <- x[srch_id %in% qid[1:nq.train]]
  xval <- x[srch_id %in% qid[(nq.train+1):(nq.train + nq.val)]]
  list('train'=xtr, 'val'=xval)
}


