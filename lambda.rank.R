# Functions in lambda.rank.R implement gradient-boosting/LambdaRank models
# for the Expedia hotel search results ranking competition on Kaggle.
#
# author: David Thaler

require(data.table)
require(gbm)
source('ndcg3.R')


rank.split <- function(split, n.trees, score.at=NULL, depth=3, seed=7){
  # Takes a trainin/validation split and produces a trained model and an
  # NDCG score for that model over the validation set. 
  # Model is trained to minimize NDCG.
  #
  # args:
  #   split - a list with the training and validation sets as elements.
  #   n.trees - # of trees to use in the GBM
  #   score.at - A number or vector of numbers for tree counts to score at. 
  #     Validation set is scored using every number of trees in score.at.
  #     Default is NULL, which just uses all trees.
  #   depth - Depth of trees in the GBM. Default is 3.
  #   seed - integer. The random number seed. Default is 7.
  #
  # return:
  #   the trained model; also prints validation score
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
  # Takes a trainin/validation split and produces a trained model and an
  # NDCG score for that model over the validation set. 
  # Model is trained to minimize mean reciprocal rank (MRR).
  #
  # args:
  #   split - a list with the training and validation sets as elements.
  #   n.trees - # of trees to use in the GBM
  #   score.at - A number or vector of numbers for tree counts to score at. 
  #     Validation set is scored using every number of trees in score.at.
  #     Default is NULL, which just uses all trees.
  #   depth - Depth of trees in the GBM. Default is 3.
  #   seed - integer. The random number seed. Default is 7.
  #
  # return:
  #   the trained model; also prints validation score
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
  # Takes a trainin/validation split and produces a trained model and an
  # NDCG score for that model over the validation set. 
  # Model is trained to minimize log-loss on click_bool.
  #
  # args:
  #   split - a list with the training and validation sets as elements.
  #   n.trees - # of trees to use in the GBM
  #   score.at - A number or vector of numbers for tree counts to score at. 
  #     Validation set is scored using every number of trees in score.at.
  #     Default is NULL, which just uses all trees.
  #   depth - Depth of trees in the GBM. Default is 3.
  #   seed - integer. The random number seed. Default is 7.
  #
  # return:
  #   the trained model; also prints validation score
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