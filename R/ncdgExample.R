generate.data <- function(N) {

  # create query groups, with an average size of 25 items each
  num.queries <- floor(N/25)
  query <- sample(1:num.queries, N, replace=TRUE)

  # X1 is a variable determined by query group only
  query.level <- runif(num.queries)
  X1 <- query.level[query]

  # X2 varies with each item
  X2 <- runif(N)

  # X3 is uncorrelated with target
  X3 <- runif(N)

  # The target
  Y <- X1 + X2

  # Add some random noise to X2 that is correlated with
  # queries, but uncorrelated with items

  X2 <- X2 + scale(runif(num.queries))[query]

  # Add some random noise to target
  SNR <- 5 # signal-to-noise ratio
  sigma <- sqrt(var(Y)/SNR)
  Y <- Y + runif(N, 0, sigma)

  data.frame(Y, query=query, X1, X2, X3)
}

cat('Generating data\n')
N=1000
data.train <- generate.data(N)

cat('Fitting a model with pairwise loss function (ranking metric: normalized discounted cumulative gain)\n')

gbm.ndcg <- gbm(Y~X1+X2+X3,          # formula
                data=data.train,     # dataset
                distribution=list(   # loss function:
                  name='pairwise',   # pairwise
                  metric="ndcg",     # ranking metric: normalized discounted cumulative gain
                  group='query'),    # column indicating query groups
                n.trees=2000,        # number of trees
                shrinkage=0.005,     # learning rate
                interaction.depth=3, # number per splits per tree
                bag.fraction = 0.5,  # subsampling fraction
                train.fraction = 1,  # fraction of data for training
                n.minobsinnode = 10, # minimum number of obs for split
                keep.data=TRUE,      # store copy of input data in model
                cv.folds=5,          # number of cross validation folds
                verbose = FALSE,     # don't print progress
                n.cores = 1)         # use a single core
# estimate number of trees
best.iter.ndcg <- gbm.perf(gbm.ndcg, method='cv')
title('Training of pairwise model with ndcg metric')