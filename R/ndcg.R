#author: Dmitry Efimov

ndcg <- function(x) {
   ideal_x <- rev(sort(x))
   dcg <- function(y) sum((2^y - 1)/log(2:(length(y)+1), base = 2))
   dcg(x)/dcg(ideal_x)
}