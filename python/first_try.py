import numpy as np
import pandas as pd
from rank_metrics import *
import pdb
from sklearn.ensemble import RandomForestRegressor
from time import time

DATAPATH = '../data/'
EXT = '.h5'
Y_COLS = ['click_bool', 'booking_bool', 'position']
REMOVE_COLS = ['gross_bookings_usd', 'date_time']
DEFAULT_STORE = 'first'

def loadData(ntrain, nval=0, store_name=DEFAULT_STORE):
  nrows = ntrain + nval
  store_path = DATAPATH + store_name + EXT
  store = pd.HDFStore(store_path)
  xtr = store.select('df', stop=nrows)
  store.close()
  ytr = xtr[Y_COLS]
  ytr['rel'] = ytr['click_bool'] + 4*ytr['booking_bool']
  ytr['total'] = ytr['rel'] + ytr['position']**-0.5
  kill_cols = REMOVE_COLS + Y_COLS
  for k in kill_cols:
    del xtr[k]
  xtr.fillna(0, inplace=True)
  xval = xtr.iloc[ntrain:,:]
  xtr = xtr.iloc[:ntrain,:]
  yval = ytr.iloc[ntrain:,:]
  ytr = ytr.iloc[:ntrain,:]
  return xtr, xval, ytr, yval
  
def trimData(data, labels):
  nrows = data.shape[0]
  first = data.srch_id.iloc[0]
  last = data.srch_id.iloc[nrows - 1]
  idx = ~(data.srch_id.isin([first, last]))
  return data[idx], labels[idx]
  
def trainModel(x, y, 
               ntrees=50,
               min_samples_split=10,
               min_samples_leaf=5):
               
  rf = RandomForestRegressor(n_estimators=ntrees, 
                             min_samples_split=min_samples_split,
                             min_samples_leaf=min_samples_leaf,
                             max_features=0.3)
  rf.fit(x,y)                  
  return rf
  
def runAll(ntrain, 
           nval=0,
           store_name=DEFAULT_STORE,
           ntrees=50, 
           min_samples_split=10,
           min_samples_leaf=5,
           target='rel',
           train_target='rel'):
  xtr, xval, ytr, yval = loadData(ntrain, nval, store_name)
  print 'Data loaded.'
  xval, yval = trimData(xval, yval)
  ytr = ytr[train_target].values
  yval = yval[target].values
  xtr = xtr.values
  xval = xval.values
  print 'Data prepared.'
  start = time()
  rf = trainModel(xtr, ytr,
                  ntrees, 
                  min_samples_split, 
                  min_samples_leaf)
  end = time()
  print 'Model trained.'
  print 'Training time: %f' % (end - start)
  score = rf.predict(xval)
  print 'Predictions made.'
  qid = xval[:,0]
  ndcg = unsorted_ndcg(qid, yval, score, 38)
  print 'Scoring complete.'
  print 'NDCG: %f' % ndcg
  return rf
