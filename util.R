require(data.table)
paths <- list('base'='../')
paths$data <- paste0(paths$base, 'data/')
paths$sample <- paste0(paths$data, 'sample40kq.csv')
paths$train <- paste0(paths$data, 'train2.csv')
paths$test <- paste0(paths$data, 'test2.csv')
paths$models <- paste0(paths$base, 'models/')
paths$submissions <- paste0(paths$base, 'submissions/')


load.expedia <- function(what='sample', basic.clean=TRUE){
  # Function loads the train, test or sample (of train) data from files into
  # a data.table. Computes a relevance score if data is 'train' or 'sample'.
  # If basic.clean is TRUE, then a new field 'noloc2' is added, which is 1 
  # for rows with no data for prop_location_score2, missing values in 
  # prop_location_score2 are filled with the median of the non-missing values,
  # and all other missing values are zero-filled. The field 
  # prop_location_score2 gets special treatment because of its high influence.
  # 
  # args:
  #   what - a string, name of data to load.
  #     Should be one of ('sample', 'train', 'test'). Default is 'sample'.
  #   basic.clean - if TRUE
  #
  # returns:
  #   a data.table with the selected raw object
  x <- fread(paths[[what]])  
  if(what %in% c('sample','train')){
    x[, rel:=4*booking_bool + click_bool]
  }
  if(basic.clean){
    x$noloc2 <- as.numeric(is.na(x$prop_location_score2))
    m <- median(x$prop_location_score2, na.rm=TRUE)
    x$prop_location_score2[is.na(x$prop_location_score2)] <- m
    x[is.na(x)] <- 0
  }
  setkey(x, srch_id)
  x
}


train.val.split <- function(nq.train, nq.val, x=NULL, what='sample', seed=7){
  # Function splits some amount of training data into training and validation
  # sets. The split is random, but repeatable by setting the seed. Data is
  # loaded if none is provided. Split respects query boundaries; each query
  # is entirely in one set or the other.
  #
  # args:
  #   nq.train - # of queries to put in the training set
  #   nq.val - # of queries to put in validation set
  #   x - input data or NULL. If data is provided, it is split into 
  #     training/validation sets, else data is loaded using load.expedia.
  #   what - a string, name of data to load, if none provided as x.
  #     Should be one either 'sample' or 'train'. Default is 'sample'.
  #     Not used if x is not NULL. Passed to load.expedia if used.
  #   seed - integer. The random number seed. Default is 7.
  #
  # return:
  #   a 3-element list with training and validation splits and the query ids
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


make.counts <- function(split, x){
  # Computes counts of distinct feature values in the fields site_id,
  # visitor_location_country_id, prop_country_id, srch_destination_id.
  # Only uses queries in x that are not used in the split.
  #
  # args:
  #   split - the data used for training and model validation
  #   x - data to collect counts from. Usually a superset of the data
  #     that training/validation sets come from, but could also be disjoint.
  qids <- unique(split$train$srch_id)
  if(!is.null(split$val)){
    qids <- union(qids, unique(split$val$srch_id))
  }
  idx <- !(x$srch_id %in% qids)
  c1 <- x[idx, list(site_ct = .N), by=site_id]
  c2 <- x[idx, list(vloc_ctry = .N), by=visitor_location_country_id]
  c3 <- x[idx, list(prop_ctry = .N), by=prop_country_id]
  c4 <- x[idx, list(srch_dest = .N), by=srch_destination_id]
  list(c1,c2,c3,c4)
}


merge.cts <- function(cts, x){
  # Adds count features to data, joining on the fields that the counts were 
  # taken from. Count features are collected in make.counts().
  #
  # args:
  #   cts - a list of data.table objects from make.counts
  #   x - the data that count features should be added to
  #
  # return:
  #   the input data, with count features added
  x <- merge(x, cts[[1]], by='site_id', all.x=TRUE, all.y=FALSE)
  x <- merge(x, cts[[2]], by='visitor_location_country_id', all.x=TRUE, all.y=FALSE)
  x <- merge(x, cts[[3]], by='prop_country_id', all.x=TRUE, all.y=FALSE)
  x <- merge(x, cts[[4]], by='srch_destination_id', all.x=TRUE, all.y=FALSE)
  x[is.na(x)] <- 0
  o <- order(x$srch_id, x$prop_id)
  x <- x[o]
  x
}


make.f.table <- function(split, x, smooth=100, rate1.only=TRUE){
  # Computes a data.table of features derived using label information grouped
  # by prop_id. For each prop_id, computes the average relevance, probability
  # of clicking, booking, and clicking without booking. If rate1.only is FALSE,
  # similar features are computed for rows in which this prop_id appeared in
  # randomized order and rows in which this prop_id appeared in the top 10 
  # positions. All of the rates are smoothed with a virtual sample size added
  # to the counts.
  #
  # args:
  #   split - the data used for training and model validation
  #   x - data to collect counts from. Usually a superset of the data
  #     that training/validation sets come from, but could also be disjoint.
  #   smooth - a virtual sample size added to the counts when figuring 
  #     average relevance and click/booking probabilities.
  #   rate1.only - If TRUE, counts and smoothed rates are computed for prop_id
  #     only, otherwise, they are also computed for rows when this prop_id
  #     appears in the top 10 positions and in random positions and those
  #     features are called count/rate[2,3].
  #
  # return:
  #   a data.table listing unique prop_id's as well as the count and 
  #   label-based feature values for each.
  qids <- unique(split$train$srch_id)
  if(!is.null(split$val)){
    qids <- union(qids, unique(split$val$srch_id))
  }
  
  f1 <- x[!(srch_id %in% qids), 
          list(count1=.N, 
               rel1=sum(rel), 
               book1=sum(booking_bool),
               click1=sum(click_bool),
               tot.pos=sum(1/sqrt(position))),
          by=prop_id]
  f1[, rate1:=rel1/(count1 + smooth)]
  f1[, bookrate:=book1/(count1 + smooth)]
  f1[, clickrate:=click1/(count1 + smooth)]
  f1[, bouncerate:=(click1 - book1)/(click1)]
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
  # Joins the f-features, computed by make.f.table, to a dataset, x.
  #
  # args:
  #   f - the f-features, as given by make.f.table()
  #   x - the data to apply f-features to.
  #
  # return:
  #   the input data with the f-features left-joined and any 
  #   missing values 0-filled
  x <- merge(x, f, by='prop_id', all.x=TRUE, all.y=FALSE)
  x[is.na(x)] <- 0
  o <- order(x$srch_id, x$prop_id)
  x <- x[o]
}

comp.comp <- function(x){
  # There are 24 columns of compk_* features. This function takes 16 columns 
  # of them (compk_rate and compk_rate_percent_diff) and collapses them down
  # to 8 columns.
  #
  # args:
  #   x - input data, labeled or unlabeled
  #
  # return:
  #   x - with the columns collapsed.
  for (k in 1:8){
    col1 <- paste0('comp', k, '_rate')
    col2 <- paste0('comp', k, '_rate_percent_diff')
    col3 <- paste0('comp', k)
    x[[col3]] <- round(x[[col1]] * x[[col2]] / 10)
    x[[col1]] <- NULL
    x[[col2]] <- NULL
  }
  x
}


z.features <- function(x){
  # Adds features for per-query z-scores of price, prop_location_score1,
  # prop_location_score2, prop_starrating, and prop_review_score.
  #
  # args:
  #   x - input data, does not need labels
  #
  # return:
  #   x, with the new columns added
  x[,zprice:=(price_usd - mean(price_usd))/(sd(price_usd) + 0.001), by=srch_id]
  x[,zloc1:=(prop_location_score1 -
                mean(prop_location_score1))/(sd(prop_location_score1) + 0.001),
    by=srch_id]
  x[,zloc2:=(prop_location_score2 -
                mean(prop_location_score2))/(sd(prop_location_score2) + 0.001),
    by=srch_id]
  x[,zstar:=(prop_starrating - mean(prop_starrating))/(sd(prop_starrating)+ 0.001),
    by=srch_id]
  x[,zrev:=(prop_review_score - mean(prop_review_score))/(sd(prop_review_score)+
                                                             0.001),
    by=srch_id]
  x
}

downsample <- function(x, rate=0.2){
  # Downsamples a training set such that all of the positive instances
  # (x$rel is 1 or 5) are retained, but the negatives (x$rel = 0) are
  # selected with probability specied in rate parameter.
  #
  # args:
  #   x - the labeled data to downsample
  #   rate - a float in [0.0, 1.0]
  #     negatives are selected with this probability
  #
  # return:
  #   nothing, but negatives in x are downsampled
  p <- x$click_bool + runif(nrow(x))
  x[p > (1-rate)]
}

split.plus <- function(nq.train, 
                       nq.val, 
                       x, 
                       smooth=100, 
                       rate1.only=FALSE){
  # Takes labeled data and returns a split into training and validation sets,
  # enhanced with computed features: f-features, z-features and count features.
  # Also collapses the competitor-based data, which is mostly missing in the 
  # raw data, into a few fields.
  #
  # args:
  #   nq.train - # queries to include in the training data
  #   nq.val - # of queries in the validation data
  #   x - source of data, can be None, in which case 
  #   smooth - virtual sample size added to counts for computing rate features
  #   rate1.only - If TRUE, counts and smoothed rates are computed for prop_id
  #     only, otherwise, they are also computed for rows when this prop_id
  #     appears in the top 10 positions and in random positions and those
  #     features are called count/rate[2,3].
  #
  # return:
  #   a list with training and validation data, including the newly computed
  #   features, as well as the f-table and feature counts, which are needed
  #   to make a matching test set.
  split <- train.val.split(nq.train, nq.val, x)
  f <- make.f.table(split, x, smooth=smooth, rate1.only=rate1.only)
  split$train <- apply.f.table(f, split$train)
  split$val <- apply.f.table(f, split$val)
  split$train <- comp.comp(split$train)
  split$val <- comp.comp(split$val)
  split$train <- z.features(split$train)
  split$val <- z.features(split$val)
  cts <- make.counts(split, x)
  split$train <- merge.cts(cts, split$train)
  split$val <- merge.cts(cts, split$val)
  list(train=split$train, val=split$val, f=f, cts=cts)
}


make.test <- function(split){
  # Loads the test set, and merges in all of the special features collected
  # from labeled data.
  #
  # args:
  #   split - a list with a training/validation split (validation set is
  #   probably empty) as well as the computed features. The split just
  #   provides the f-table and the feature counts.
  #
  # return:
  #   the test data, with all of the calculated features merged in
  xtest <- load.expedia('test')
  xtest <- apply.f.table(split$f, xtest)
  xtest <- z.features(xtest)
  xtest <- comp.comp(xtest)
  xtest <- merge.cts(split$cts, xtest)
  xtest
}


write.submission <- function(submit.number, pred, test){
  # Writes out a valid submission at paths$submit/.
  #
  # args:
  #   submit.number - submission is named "resub<submit.num>.csv"
  #   pred - the predictions  
  #   test - the whole test data
  #
  # return:
  #   nothing, but a valid submission is written at paths$submissions/.
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





