import numpy as np

# NOTE: This implementation didn't really work out.
# What is actually needed is a distinction between dcg/ndcg
# in the sorted/unsorted cases and in the per-query/whole list
# cases. Then the whole thing should get a single interface
# that dispatches correctly according to what it gets in.
# Also, dcg (& maybe IDCG) can be pseudo-private, I think.
#
# GOTCHA #1: There are different definitions of DCG, and
# hence NDCG rattling around out there.
# GOTCHA #2: In the other main def, the log base matters.
# 
# Outside of Kaggle, this would have to handle the case where
# there are no relevant results.
#
# Secondly, for Kaggle Expedia, this needs to work in 
# the out-of-core case. That makes this code a do-over.
# Obviously, that only has to work for the whole list case.
#
# Finally, the code produces correct output, so it is a valid
# reference implementation.

def dcg_k(rel, k=None):
  """
  
  Takes a Numpy array with the relevances of each item ranked, 
  with the items in the order in which they were ranked, 
  and returns the cumulative discounted gain for the ranking.
  
  Parameters
  -----------
  rel: a Numpy array of relevence scores for the items returned
       in a query, in the order in which they were ranked.
  k: the point in the list to figure DCG up to.
     To get DCG for the whole list, use None.
  
  Returns
  -------
  The DCG of the items for the provided ranking
  """
  if k is None:
    k = len(rel)
  num = np.exp2(rel) - 1
  den = np.log2(np.linspace(1, len(rel), len(rel)) + 1)
  return np.sum(np.divide(num[0:k], den[0:k]))
  
  
def ndcg_k(rel, k=None):
  """
  
  Takes a Numpy array with the relevances of each item ranked, 
  with the items in the order in which they were ranked, and 
  returns the normalized cumulative discounted gain for the ranking.
  
  Parameters
  ----------
  rel: a Numpy array of relevence scores for the items returned 
       in a query, in the order in which they were ranked.
  k: the point in the list to figure DCG up to.
     To get DCG for the whole list, use None.
     
  Returns
  -------
  The NDCG of the items for the provided ranking
  """
  idcg = dcg_k(np.sort(rel)[::-1], k)
  return dcg_k(rel, k)/idcg
  
  
def mean_ndcg_k(scores, k=None):
  """
  
  Computes the normalized cumulative discounted gain(NDCG@k)
  for a set of search results, averaged over all of the results.
  
  Parameters
  ----------
  scores: an n x 2 Numpy array in which the first column has the 
          query id of each result and the second column has the 
          relevance score (label) for that result. The results 
          within each query should be in the order in which they
          were ranked.
  
  k: NDCG@k, the NDCG over the first k results, is computed
     for each query. Use None to compute over all results.
  
  Returns
  -------
  The average of NDCG@k (or over all results) for all of the query
  results in scores.
  """
  results = []
  for qid in np.unique(scores[:,0]):
    rel = scores[scores[:,0]==qid, 1]
    results.append(ndcg_k(rel, k))
  return np.mean(np.array(results))
  
def unsorted_ndcg(qid, rel, pred, k=None):
  """
  """
  data = np.column_stack((qid, rel, pred))
  idx = np.lexsort((-data[:,2], data[:,0]))
  data = data[idx, 0:2]
  return mean_ndcg_k(data, k)
  