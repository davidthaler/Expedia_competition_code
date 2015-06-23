require(foreach)
require(doMC)
source('ndcg3.R')
require(gbm)
require(data.table)

#NB: may have to registerDoMC

# trying added noise, first on zloc2
cv.noise <- function(split, trees, test.col, noises){
  set.seed(7)
  dist <- list(name="pairwise",
               group="srch_id",
               metric="ndcg",
               max.rank=38)
  kill.cols <- c('rel', 'click_bool', 'booking_bool', 
                 'gross_bookings_usd','date_time',
                 'position', 'srch_id')
  kill.cols <- c(kill.cols, test.col)
  base.cols <- names(split$train)
  base.cols <- base.cols[!(base.cols %in% kill.cols)]
  extra.cols <- NULL
  for(n in noises){
    col.name <- paste0(test.col, n)
    eps <- rnorm(nrow(split$train), 0, n)
    split$train[[col.name]] <- split$train[[test.col]] + eps
    split$val[[col.name]] <- split$val[[test.col]]
    extra.cols <- c(extra.cols, col.name)
  }
  result <- foreach(k=1:length(noises), .packages='gbm') %dopar%{
              cols <- c(base.cols, extra.cols[k])
              f <- as.formula(paste('rel ~', paste(cols, collapse='+')))
              model <- gbm(f, data=split$train,
                           distribution=dist, 
                           shrinkage=0.1, 
                           n.trees=trees, 
                           interaction.depth=3,
                           keep.data=FALSE)
              pred <- predict(model, split$val, n.trees=trees)
              score <- ndcg.all(split$val, pred)
              c(noises[k], score)
            }
}




