import pandas as pd

def medianNormalize(x,col):
  f = lambda z: z - z.median()
  x[col + 'medNorm'] = x.groupby('srch_id')[col].transform(f)
  
def maxNormalize(x,col):
  f = lambda z: z- z.max()
  x[col + 'maxNorm'] = x.groupby('srch_id')[col].transform(f)
  
def minNormalize(x, col):
  f = lambda z: z- z.min()
  x[col + 'minNorm'] = x.groupby('srch_id')[col].transform(f)

def rankItems(x, col, asc=False):
  cname = col + 'rank'
  x[cname] = 0
  x[cname] = x.groupby('srch_id')[col].rank(ascending=asc)

def enhance(x):
  rankItems(x,'prop_starrating')
  rankItems(x,'prop_review_score')
  rankItems(x,'prop_location_score1')
  rankItems(x,'prop_location_score2')
  rankItems(x,'prop_log_historical_price')
  rankItems(x,'price_usd', asc=True)
  medianNormalize(x,'prop_starrating')
  medianNormalize(x,'prop_review_score')
  medianNormalize(x,'prop_location_score1')
  medianNormalize(x,'prop_location_score2')
  medianNormalize(x,'prop_log_historical_price')
  medianNormalize(x,'price_usd')
  minNormalize(x,'price_usd')
  maxNormalize(x,'prop_starrating')
  maxNormalize(x,'prop_review_score')
  maxNormalize(x,'prop_location_score1')
  maxNormalize(x,'prop_location_score2')

  