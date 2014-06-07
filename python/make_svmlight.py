import numpy as np
import pandas as pd
import pdb
from sklearn.datasets import dump_svmlight_file as dumpsvm

DATAPATH = '../data/'
H5EXT = '.h5'
L2REXT = '.l2r'
Y_COLS = ['click_bool', 'booking_bool', 'position']
REMOVE_COLS = ['gross_bookings_usd', 'date_time']
DEFAULT_STORE = 'first'
fmt_base = '%d qid:%d'
  
def loadData(ntrain, nval=0, fill_na=False, store_name=DEFAULT_STORE):
  nrows = ntrain + nval
  store_path = DATAPATH + store_name + H5EXT
  store = pd.HDFStore(store_path)
  xtr = store.select('df', stop=nrows)
  store.close()
  ytr = xtr[Y_COLS]
  ytr['rel'] = ytr['click_bool'] + 4*ytr['booking_bool']
  kill_cols = REMOVE_COLS + Y_COLS
  for k in kill_cols:
    del xtr[k]
  if fill_na:
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

def fillGroupMedian(x):
  f = lambda z: z.fillna(z.median())
  xmod = x.groupby('srch_id').transform(f)
  return xmod.iloc[:,1]

def cleanData(x):
  x['no_vis_hist_star'] = np.isnan(x.visitor_hist_starrating)
  x.visitor_hist_starrating.fillna(x.visitor_hist_starrating.median(), inplace=True)
  x['no_vis_hist_adr'] = np.isnan(x.visitor_hist_adr_usd)
  x.visitor_hist_adr_usd.fillna(x.visitor_hist_adr_usd.median(), inplace=True)

def basic_write(ntrain, nval, train_file, val_file):
  train_file = DATAPATH + train_file + L2REXT
  val_file = DATAPATH + val_file + L2REXT
  xtr, xval, ytr, yval = loadData(ntrain, nval, fill_na=True)
  xval, yval = trimData(xval, yval)
  ytr = ytr['rel'].values
  yval = yval['rel'].values
  xtr = xtr.values
  xval = xval.values
  fmt = fmt_base
  n = xtr.shape[1]
  for j in range(1, n):                
    fmt = fmt + ' ' + str(j) + ':%.3f'
  ytr.shape = (len(ytr), 1)
  yval.shape = (len(yval), 1)
  xtr = np.hstack((ytr,xtr))
  xval = np.hstack((yval, xval))
  np.savetxt(train_file, xtr, fmt=fmt)
  np.savetxt(val_file, xval, fmt=fmt)
  
def write_test(outfile, hdf_source='test.h5', chunksize=1000000):
  outfile_base = DATAPATH + outfile
  store_path = DATAPATH + hdf_source
  store = pd.HDFStore(store_path)
  #pdb.set_trace()
  for k, x in enumerate(store.select('df', chunksize=chunksize)):
    dummy = np.zeros((x.shape[0],1))
    del x['date_time']
    x.fillna(0, inplace=True)
    x = x.values
    n = x.shape[1]
    # Tricky...this is 1...n-1, inclusive. There are n-1 values. That is correct,
    # because the first column of x is the qid at this point.
    fmt = fmt_base
    for j in range(1, n):                
      fmt = fmt + ' ' + str(j) + ':%.3f'
    x = np.hstack((dummy, x))               # now x has n+1 columns
    outfile = outfile_base + str(k) + L2REXT
    print 'writing to %s' % outfile
    np.savetxt(outfile, x, fmt=fmt)
  store.close()