# author:Owen

my_ndcg<-function(rel, idx, orderby, k=1000){
  d<-data.frame(rel=rel, idx=idx, one=1, o=orderby)
  d<-d[order(d$idx, -d$rel),]
  d$co<-with(d, unlist(tapply(one, idx, cumsum)))
  d$v<-with(d, (2^rel-1)/(log(co+1, 2)))
  #print(head(d, 20))
  dcg0<-with(d[d$co<=k,], unlist(tapply(v, idx, sum)))
  d<-d[order(d$idx, d$o),]
  d$co<-with(d, unlist(tapply(one, idx, cumsum)))
  d$v<-with(d, (2^rel-1)/(log(co+1, 2)))
  #print(head(d, 20))
  #dcg<-sum(d$v[d$co<=k])
  dcg<-with(d[d$co<=k,], unlist(tapply(v, idx, sum)))
  return(mean(dcg/dcg0))
}