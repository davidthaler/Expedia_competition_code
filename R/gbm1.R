library(gbm)

ITER = 1000
LEARNRATE = 0.1
DEPTH = 4
MINOBS = 10
#NTRAIN = length(yn)
NTRAIN = 10000

model <- gbm.fit(vtr[1:NTRAIN,], yn[1:NTRAIN],
            distribution="bernoulli",
            w = y.wts[1:NTRAIN],
            n.trees = ITER,
            shrinkage = LEARNRATE,
            interaction.depth = DEPTH,
            n.minobsinnode = MINOBS,
            bag.fraction = 0.5,
            nTrain = NTRAIN,
            keep.data = FALSE,
            verbose = TRUE) 