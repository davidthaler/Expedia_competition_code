# Functions in ndcg.R compute the Normalized Discounted Cummulative Gain, 
# which is a ranking metric used to evaluate entries in the Expedia hotel
# search results ranking competition on Kaggle
#
# author: David Thaler
#
# author: David Thaler
require(data.table)

ndcg1 <- function(rel, pred){
  # Computes the NDCG over one querey.
  #
  # args:
  #   rel - a vector or data.table column of relevance scores (labels)
  #   pred - a vector or data.table column of predictions
  #
  # return:
  #   NDCG for this query
  o <- order(pred, decreasing=TRUE)
  actual <- rel[o]
  ideal <- sort(rel, decreasing=TRUE)
  dcg <- function(y) sum((2^y - 1)/log(2:(length(y)+1), base = 2))
  ndcg <- dcg(actual)/dcg(ideal)
}

ndcg.all <- function(val, pred){
  # Figures NDCG for all queries.
  #
  # args:
  #   val - the labeled data as a data.table
  #   pred - predictions over that data as a vector
  #
  # return:
  #   NDCG over all of the queries
  df <- val[, list(srch_id, rel)]
  df[, pred:=pred]
  ndcg.query <- df[, ndcg1(rel, pred), by=srch_id]
  ndcg <- ndcg.query[, mean(V1)]
}