require(data.table)

ndcg1 <- function(rel, pred){
  o <- order(pred, decreasing=TRUE)
  actual <- rel[o]
  ideal <- sort(rel, decreasing=TRUE)
  dcg <- function(y) sum((2^y - 1)/log(2:(length(y)+1), base = 2))
  ndcg <- dcg(actual)/dcg(ideal)
}

ndcg.all <- function(val, pred){
  df <- val[, list(srch_id, rel)]
  df[, pred:=pred]
  ndcg.query <- df[, ndcg1(rel, pred), by=srch_id]
  ndcg <- ndcg.query[, mean(V1)]
}