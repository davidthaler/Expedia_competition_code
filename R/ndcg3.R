require(data.table)

ndcg1 <- function(rel, pred, k=38){
  # NB : this is designed to be used in the expression (2nd part) of data.table
  # NB2: this is not quite right...we actuall want ndcg@38
  o <- order(pred, decreasing=TRUE)
  actual <- rel[o]
  ideal <- sort(rel, decreasing=TRUE)
  dcg <- function(y) sum((2^y - 1)/log(2:(length(y)+1), base = 2))
  ndcg <- dcg(actual[1:k])/dcg(ideal[1:k])
}

ndcg.all <- function(val, pred){
  df <- val[, list(srch_id, rel)]
  df[, pred:=pred]
  ndcg.query <- df[, ndcg1(rel, pred), by=srch_id]
  ndcg <- ndcg.query[, mean(V1)]
}