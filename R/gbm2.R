gbm2 <- function(x, y){

library(gbm)
MAXITER <- 1000
LEARNRATE <- 0.1
DEPTH <- 4
MINOBS <- 10
HOLDOUT <- 0.25
tr.sz <- 10000

dist <- list(name = 'pairwise', 
             group = 'srch_id',
             metric = 'ndcg',
             max.rank = 39)

model <- gbm.fit(x, y,
                 distribution = 'pairwise',
                 n.trees = MAXITER,
                 shrinkage = LEARNRATE,
                 interaction.depth = DEPTH,
                 n.minobsinnode = MINOBS,
                 bag.fraction = 0.5,
                 nTrain = tr.sz,
                 keep.data = FALSE,
                 verbose = TRUE,
                 group = 'srch_id')
}