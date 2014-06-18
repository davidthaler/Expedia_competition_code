require(foreach)
require(doMC)
source('ndcg3.R')

#This is how to cross-validate one parameter
cv.par <- function(split, maxnodes, ntree=30){
  #maxnodes should be a vector
  rf.list <- foreach(n=maxnodes, .packages='randomForest') %dopar%{
               rf <- randomForest(rel ~ . -srch_id -click_bool -position 
                                 -booking_bool -gross_bookings_usd -date_time,
                                 data=split$train, ntree=ntree, maxnodes=n)
               pred <- predict(rf, split$val)
               score <- ndcg.all(split$val, pred)
               list(maxnodes=n, ndcg=score)
            }
  rf.list
}

# This is how you make 1 RF in parallel
rf.par <- function(split, maxnodes, ntree, cycles){
  # maxnodes, ntrees, cycles - all ints
  rf.par <- foreach(1:cycles, .combine=combine, .packages='randomForest') %dopar%{
                   randomForest(rel ~ . -srch_id -click_bool -position 
                               -booking_bool -gross_bookings_usd -date_time,
                                data=split$train, ntree=ntree, maxnodes=maxnodes)
                   }
}