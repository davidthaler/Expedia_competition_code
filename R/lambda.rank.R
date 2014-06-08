require(data.table)
require(gbm)
source('my_ndcg.R')


rank.split <- function(split, n.trees, seed=7){
  set.seed(seed)
  dist <- list(name="pairwise",
               group="srch_id",
               metric="ndcg",
               max.rank=38)
  model <- gbm(rel ~ . -srch_id,
               data=split$train[, c(1, 3:51, 55), with=FALSE],
               distribution=dist,
               shrinkage=0.1,
               interaction.depth=3,
               n.trees=n.trees)
  pred <- predict(model, split$val, n.trees=n.trees)
  score <- my_ndcg(split$val$rel, split$val$srch_id, -pred, 38)
  print(paste("NDCG on validation:", score))
}