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
  if(nq.val > 0){
    xval <- x[srch_id %in% qid[(nq.train+1):(nq.train + nq.val)]]
  }else{
    xval <- NULL
  }
  list('train'=xtr, 'val'=xval, 'qid'=qid)
}

make.f.table <- function(split, x, smooth=100){
  qids <- unique(split$train$srch_id)
  if(!is.null(split$val)){
    qids <- union(qids, unique(split$val$srch_id))
  }
  f <- x[!(srch_id %in% qids), 
         list(count=.N, total.rel=sum(rel)), by=prop_id]
  setkey(f, prop_id)
  f[, rate:=total.rel/(count+smooth)]
  f$total.rel <- NULL
  f
}

apply.f.table <- function(f, x){
  x <- merge(x, f, by='prop_id', all.x=TRUE, all.y=FALSE)
  x$count[is.na(x$count)] <- 0
  x$rate[is.na(x$rate)] <- 0
  o <- order(x$srch_id, x$prop_id)
  x <- x[o]
}

write.submission <- function(submit.number, pred, test){
  path <- paste0(paths$submissions, 'resub', submit.number, '.csv')
  sub <- test[, list(srch_id, prop_id)]
  o <- order(sub$srch_id, -pred)
  sub <- sub[o]
  write.table(sub, path, 
              row.names=FALSE, 
              quote=FALSE, 
              sep=",",
              col.names=c("SearchId", "PropertyId"))
  print(paste('Wrote submission to : ', path))
}





