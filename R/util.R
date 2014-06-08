require(data.table)

paths <- list('base'='../../')
paths$data <- paste0(paths$base, 'data/')
paths$sample <- paste0(paths$data, 'sample.csv')
paths$train <- paste0(paths$data, 'train2.csv')
paths$test <- paste0(paths$data, 'test2.csv')
paths$models <- paste0(paths$base, 'models/')
paths$submissions <- paste0(paths$base, 'submissions/')

load.expedia <- function(what='sample', basic.clean=TRUE){
 x <- fread(paths[[what]])  
 if(what %in% c('sample','train')){
   x[, rel:=4*booking_bool + click_bool]
 }
 if(basic.clean){
   x[is.na(x)] <- 0
 }
 setkey(x, srch_id)
 x
}

train.val.split <- function(nq.train, nq.val, x=NULL, what='sample', seed=7){
  set.seed(seed)
  if(is.null(x)){
    x <- load.expedia(what)
  }
  qid <- unique(x$srch_id)
  qid <- sample(qid, length(qid))
  xtr <- x[srch_id %in% qid[1:nq.train]]
  xval <- x[srch_id %in% qid[(nq.train+1):(nq.train + nq.val)]]
  list('train'=xtr, 'val'=xval, 'qid'=qid)
}

prop.features.split <- function(split, x, smooth=100){
  tr.qids <- unique(split$train$srch_id)
  val.qids <- unique(split$val$srch_id)
  f <- x[!(srch_id %in% union(tr.qids, val.qids)), 
         list(count=.N, total.rel=sum(rel)), by=prop_id]
  setkey(f, prop_id)
  f[, rate:=total.rel/(count+smooth)]
  f$total.rel <- NULL
  split$train <- merge(split$train, f, by='prop_id', all.x=TRUE, all.y=FALSE)
  split$val <- merge(split$val, f, by='prop_id', all.x=TRUE, all.y=FALSE)
  split$train$count[is.na(split$train$count)] <- 0
  split$train$rate[is.na(split$train$rate)] <- 0
  split$val$count[is.na(split$val$count)] <- 0
  split$val$rate[is.na(split$val$rate)] <- 0
  o <- order(split$val$srch_id, split$val$prop_id)
  split$val <- split$val[o]
  o <- order(split$train$srch_id, split$train$prop_id)
  split$train <- split$train[o]
  split
}


