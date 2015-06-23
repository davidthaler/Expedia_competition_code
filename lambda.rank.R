require(data.table)
require(gbm)
source('ndcg3.R')


rank.split <- function(split, n.trees, score.at=NULL, depth=3, seed=7){
  set.seed(seed)
  dist <- list(name="pairwise",
               group="srch_id",
               metric="ndcg",
               max.rank=38)
  model <- gbm(rel ~ . -srch_id -click_bool -booking_bool -position
               -date_time -gross_bookings_usd,
               data=split$train,
               distribution=dist,
               shrinkage=0.1,
               interaction.depth=depth,
               n.trees=n.trees)
  if(is.null(score.at)){
    score.at = n.trees
  }
  for (s in score.at){
    pred <- predict(model, split$val, n.trees=s)
    score <- ndcg.all(split$val, pred)
    print(paste("NDCG on validation at", s, "trees:", score))
  }
  model
}

train.mrr <- function(split, n.trees, score.at=NULL, depth=3, seed=7){
  set.seed(seed)
  dist <- list(name="pairwise",
               group="srch_id",
               metric="mrr",
               max.rank=38)
  model <- gbm(booking_bool ~ . -srch_id -rel -date_time
               -click_bool -position -gross_bookings_usd,
               data=split$train,
               distribution=dist,
               shrinkage=0.1,
               interaction.depth=depth,
               n.trees=n.trees)
  if(is.null(score.at)){
    score.at = n.trees
  }
  for (s in score.at){
    pred <- predict(model, split$val, n.trees=s)
    score <- ndcg.all(split$val, pred)
    print(paste("NDCG on validation at", s, "trees:", score))
  }
  model
}

train.beroulli <- function(split, n.trees, score.at=NULL, depth=3, seed=7){
  set.seed(seed)
  model <- gbm(click_bool ~ . -srch_id -rel -date_time
               -booking_bool -position -gross_bookings_usd,
               data=split$train,
               distribution='bernoulli',
               shrinkage=0.1,
               interaction.depth=depth,
               n.trees=n.trees)
  if(is.null(score.at)){
    score.at = n.trees
  }
  for (s in score.at){
    pred <- predict(model, split$val, n.trees=s)
    score <- ndcg.all(split$val, pred)
    print(paste("NDCG on validation at", s, "trees:", score))
  }
  model
}