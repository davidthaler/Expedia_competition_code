require(data.table)
require(gbm)
source('ndcg3.R')


rank.split <- function(split, n.trees, score.at=NULL, depth=3, seed=7){
  set.seed(seed)
  dist <- list(name="pairwise",
               group="srch_id",
               metric="ndcg",
               max.rank=38)
  if(exists('booking_bool', split$train)){
    split$train$booking_bool <- NULL
  }
  if(exists('click_bool', split$train)){
    split$train$click_bool <- NULL
  }
  if(exists('position', split$train)){
    split$train$position <- NULL
  }
  if(exists('gross_bookings_usd', split$train)){
    split$train$gross_bookings_usd <- NULL
  }
  if(exists('date_time', split$train)){
    split$train$date_time <- NULL
  }
  model <- gbm(rel ~ . -srch_id,
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