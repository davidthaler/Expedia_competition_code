require(data.table)
require(gbm)
source('ndcg3.R')


rank.split <- function(split, n.trees, score.at=NULL, seed=7){
  set.seed(seed)
  dist <- list(name="pairwise",
               group="srch_id",
               metric="ndcg",
               max.rank=38)
  model <- gbm(rel ~ . -srch_id,
               data=split$train[, c(1, 3:14, 16:51, 55), with=FALSE],
               distribution=dist,
               shrinkage=0.1,
               interaction.depth=3,
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