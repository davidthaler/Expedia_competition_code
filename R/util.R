require(data.table)
paths <- list('base'='../../')
paths$data <- paste0(paths$base, 'data/')
paths$sample <- paste0(paths$data, 'sample40kq.csv')
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


make.counts <- function(split, x, test=NULL){
  qids <- unique(split$train$srch_id)
  if(!is.null(split$val)){
    qids <- union(qids, unique(split$val$srch_id))
  }
  idx <- !(x$srch_id %in% qids)
  c1 <- x[idx, list(site_ct = .N), by=site_id]
  c2 <- x[idx, list(vloc_ctry = .N), by=visitor_location_country_id]
  c3 <- x[idx, list(prop_ctry = .N), by=prop_country_id]
  c4 <- x[idx, list(srch_dest = .N), by=srch_destination_id]
  if(is.null(test)){
    split$train <- merge.cts(c1, c2, c3, c4, split$train)
    if(!is.null(split$val)){
      split$val <- merge.cts(c1, c2, c3, c4, split$val)
    }
    return(split)
  }else{
    test <- merge.cts(c1, c2, c3, c4, test)
    return(test)
  }
}


merge.cts <- function(c1,c2,c3,c4,x){
  x <- merge(x, c1, by='site_id', all.x=TRUE, all.y=FALSE)
  x <- merge(x, c2, by='visitor_location_country_id', all.x=TRUE, all.y=FALSE)
  x <- merge(x, c3, by='prop_country_id', all.x=TRUE, all.y=FALSE)
  x <- merge(x, c4, by='srch_destination_id', all.x=TRUE, all.y=FALSE)
  x[is.na(x)] <- 0
  o <- order(x$srch_id, x$prop_id)
  x <- x[o]
  x
}


make.f.table <- function(split, x, smooth=100, rate1.only=TRUE){
  qids <- unique(split$train$srch_id)
  if(!is.null(split$val)){
    qids <- union(qids, unique(split$val$srch_id))
  }
  
  f1 <- x[!(srch_id %in% qids), 
          list(count1=.N, rel1=sum(rel), tot.pos=sum(1/sqrt(position))),
          by=prop_id]
  f1[, rate1:=rel1/(count1 + smooth)]
  f1[, pos:=tot.pos/(count1 + smooth)]
  setkey(f1, prop_id)
  f1$rel1 <- NULL
  f1$tot.pos <- NULL
  if(rate1.only){
    return(f1)  
  }
  
  f2 <- x[!(srch_id %in% qids) & (x$random_bool==1), 
          list(count2=.N, rel2=sum(rel)), by=prop_id]
  f2[, rate2:=rel2/(count2 + smooth)]
  setkey(f2, prop_id)
  f2$rel2 <- NULL
  
  f3 <- x[!(srch_id %in% qids) & (x$position > 10), 
          list(count3=.N, rel3=sum(rel)), by=prop_id]
  f3[, rate3:=rel3/(count3 + smooth)]
  setkey(f3, prop_id)
  f3$rel3 <- NULL
  
  f <- f3[f2[f1]]
  f[is.na(f)] <- 0
  f
}


apply.f.table <- function(f, x){
  x <- merge(x, f, by='prop_id', all.x=TRUE, all.y=FALSE)
  x[is.na(x)] <- 0
  o <- order(x$srch_id, x$prop_id)
  x <- x[o]
}

comp.comp <- function(x){
  # compresses the 8 pairs of compK_rate and compK_rate_percent_diff variables
  # into 8 variables
  for (k in 1:8){
    col1 <- paste0('comp', k, '_rate')
    col2 <- paste0('comp', k, '_rate_percent_diff')
    col3 <- paste0('comp', k)
    x[[col3]] <- x[[col1]] * x[[col2]]
    x[[col1]] <- NULL
    x[[col2]] <- NULL
  }
  x
}


fuzz.rate <- function(x, sigma, col.name='rate1'){
  n <- rnorm(nrow(x), 0, sigma)
  x[[col.name]] <- x[[col.name]] + n
  x
}

z.features <- function(x){
  x[,zprice:=(price_usd - mean(price_usd))/(sd(price_usd) + 0.001), by=srch_id]
  x[,zloc1:=(prop_location_score1 -
                mean(prop_location_score1))/(sd(prop_location_score1) + 0.001),
    by=srch_id]
  x[,zloc2:=(prop_location_score2 -
                mean(prop_location_score2))/(sd(prop_location_score2) + 0.001),
    by=srch_id]
  x[,zstar:=(prop_starrating - mean(prop_starrating))/(sd(prop_starrating)+ 0.001),
    by=srch_id]
  x[,zstar:=(prop_review_score - mean(prop_review_score))/(sd(prop_review_score)+
                                                             0.001),
    by=srch_id]
  x
}


split.plus <- function(nq.train, 
                       nq.val, 
                       x, 
                       smooth=100, 
                       fuzz=0.01, 
                       use.counts=TRUE,
                       rate1.only=TRUE){
  split <- train.val.split(nq.train, nq.val, x)
  f <- make.f.table(split, x, smooth=smooth, rate1.only=rate1.only)
  split$train <- apply.f.table(f, split$train)
  split$val <- apply.f.table(f, split$val)
  split$train <- fuzz.rate(split$train, fuzz)
  split$train <- comp.comp(split$train)
  split$val <- comp.comp(split$val)
  if(use.counts){
    split <- make.counts(split, x)
  }
  list(train=split$train, val=split$val, f=f)
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





