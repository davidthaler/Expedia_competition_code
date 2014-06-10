

do.business <- function(x){
  b <- with(x, as.numeric((srch_adults_count==1) & (srch_children_count==0) &
                            (srch_saturday_night_bool==0) & (srch_room_count==1) &
                            (srch_length_of_stay <=3) & (srch_booking_window <= 10)))
  x[['bus']] <- b
  x
}


f.short <- function(nq.train, nq.val, x, vss=100){
  split <- train.val.split(nq.train, nq.val, x)
  qids <- unique(split$train$srch_id)
  if(!is.null(split$val)){
    qids <- union(qids, unique(split$val$srch_id))
  }
  idx <- !(x$srch_id %in% qids)
  mu <- x[idx, mean(rel)]
  f <- x[idx, list(raw = mean(rel), ct=.N), by=prop_id]
  f[, rate := (vss*mu + ct*raw) / (vss+ct)]
  f$raw <- NULL
  setkey(f, prop_id)
  split$train <- apply.f.table(f, split$train)
  split$val <- apply.f.table(f, split$val)
  split$f <- f
  split
}