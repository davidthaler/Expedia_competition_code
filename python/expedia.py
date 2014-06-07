import numpy as np
import pandas as pd
import sqlite3
import pandas.io.sql as sql
import pdb
import scratch
import cPickle
import rank_metrics
from sklearn.ensemble import RandomForestRegressor
from time import time 
import target_feature

DATAPATH = '../data/'
DATABASE = 'expedia.db'
L2REXT = '.l2r'
SQLEXT = '.sql'
TRAIN_QIDS = 'unique_qid_train'

ZERO_IS_MISSING_COLS = ['prop_starrating', 'prop_review_score', 'prop_log_historical_price']

SELECT_X_COLS = 'srch_id, mth, yr, site_id, visitor_location_country_id, visitor_hist_starrating, visitor_hist_adr_usd, prop_id, prop_starrating, prop_review_score, prop_brand_bool, prop_location_score1, prop_location_score2, prop_log_historical_price, price_usd, promotion_flag, srch_destination_id, srch_length_of_stay, srch_booking_window, srch_adults_count, srch_children_count, srch_room_count, srch_saturday_night_bool, srch_query_affinity_score, orig_destination_distance, random_bool, comp2_rate, comp3_rate, comp5_rate, comp8_rate, comp2_rate_percent_diff, comp3_rate_percent_diff, comp5_rate_percent_diff, comp8_rate_percent_diff, comp2_inv, comp3_inv, comp5_inv, comp8_inv'

IMPUTE_MEDIAN_COLS = ['prop_starrating', 'prop_review_score', 'prop_log_historical_price']

SELECT_Y_COLS = 'srch_id, prop_id, position, rel'

COMP_COLS_KILL = ['comp2_rate', 'comp3_rate', 'comp5_rate', 'comp8_rate',
                  'comp2_rate_percent_diff', 'comp3_rate_percent_diff',
                  'comp5_rate_percent_diff', 'comp8_rate_percent_diff']
                  
RF_XCOLS = 'prop_starrating, prop_review_score, prop_brand_bool, prop_location_score1, prop_location_score2,prop_log_historical_price,price_usd,promotion_flag,random_bool,srch_id'
  
#For submit
SCORE_PATH = '../output/%s-%d.csv'
COMBINED_PATH =  '../output/combined-%d.csv'
SUBMIT_PATH = '../submissions/submit%d.csv'
HEADER = 'SearchId,PropertyId\n' 

def numpify(df, fill_with=np.nan):
  """
  Takes a pandas.DataFrame that came from sqlite and replaces its "NULL"
  values with numpy.nan or a given fill_with value. 
  Columns that had "NULL" will be of dtype object, so we convert to numeric.
  """
  df.replace('NULL', fill_with, inplace=True)
  df = df.convert_objects(convert_numeric=True)   #No inplace version of this, sorry.
  return df

  
def get_sql_str(table, qids, cols=None):
  qids.sort()
  if cols is None:
    cols = '*'
  sql_str = 'select %s from %s where srch_id in (' % (cols, table)
  for q in qids:
    sql_str = sql_str + str(q) + ','
  sql_str = sql_str[0:-1] + ')'
  return sql_str

  
def comp_cols(x):
  x['comp2'] = x.comp2_rate * x.comp2_rate_percent_diff
  x['comp3'] = x.comp3_rate * x.comp3_rate_percent_diff
  x['comp5'] = x.comp5_rate * x.comp5_rate_percent_diff
  x['comp8'] = x.comp8_rate * x.comp8_rate_percent_diff
  for k in COMP_COLS_KILL:
    del x[k]
  
def process_x(x):
  #This clean funtion doesn't help much, replace w/fillna
  #clean(x)
  x.fillna(0, inplace=True)
  comp_cols(x)
   
def get_train_val(ntrain, nval, r_seed=19):
  # ntrain, nval will be number of whole queries used
  datafile = DATAPATH + DATABASE

  with sqlite3.connect(datafile) as conn:
    all_qids = sql.read_frame('select * from unique_qid_train', conn)
    all_qids = all_qids['srch_id'].values          #leaves an ndarray
    np.random.seed(r_seed)
    np.random.shuffle(all_qids)
    q_tr = all_qids[:ntrain]
    q_val = all_qids[ntrain:ntrain + nval]
    # 1 of next 2
    #sql_str = get_sql_str('train', q_tr)
    sql_str = get_sql_str('train', q_tr, SELECT_X_COLS)
    xtr = sql.read_frame(sql_str, conn)
    sql_str = get_sql_str('labels', q_tr)
    ytr = sql.read_frame(sql_str, conn)
    # ditto
    #sql_str = get_sql_str('train', q_val)
    sql_str = get_sql_str('train', q_val, SELECT_X_COLS)
    xval = sql.read_frame(sql_str, conn)
    sql_str = get_sql_str('labels', q_val)
    yval = sql.read_frame(sql_str, conn)
    
    #NB: this replaces SQL "NULL" values with numpy.nan.
    xtr = numpify(xtr)
    xval = numpify(xval)
    ytr = numpify(ytr)
    yval = numpify(yval)
  return xtr, xval, ytr, yval

def get_tr_batch(qids):
  datafile = DATAPATH + DATABASE
  with sqlite3.connect(datafile) as conn:
    sql_str = get_sql_str('train', qids, SELECT_X_COLS)
    x = sql.read_frame(sql_str, conn)
    sql_str = get_sql_str('labels', qids, SELECT_Y_COLS)
    y = sql.read_frame(sql_str, conn)
    x = numpify(x)
    y = numpify(y)
    return x,y

def trainRF(pickle_file, 
            r_seed, 
            ntrees=50, 
            nq_train=4000, 
            nq_val=None, 
            nq_features=None):
  start = time()
  datafile = DATAPATH + DATABASE
  print 'reading qids...'
  with sqlite3.connect(datafile) as conn:
    all_qids = sql.read_frame('select * from unique_qid_train', conn)
    all_qids = all_qids['srch_id'].values          
    np.random.seed(r_seed)
    np.random.shuffle(all_qids)
    print 'making target features...'
    qids = all_qids[nq_train + nq_val:nq_train + nq_val + nq_features]
    sql_str = get_sql_str('labels', qids, 'prop_id, rel')
    df = sql.read_frame(sql_str, conn)
  xf = target_feature.make_frame(df, 'prop_id', 'rel', 100)
  del df
  qids = all_qids[:nq_train]
  print 'fetching training set...'
  x,y = get_tr_batch(qids)
  print 'processing...'
  x = processRF(x, xf)
  print 'training...'
  rf = RandomForestRegressor(n_estimators=ntrees, 
                             min_samples_split=10,
                             min_samples_leaf=10,
                             max_features=0.3,
                             verbose=1)
  rf.fit(x, y.rel)
  del x
  del y
  if pickle_file is not None:
    picklepath = '../artifacts/' + pickle_file + '.pkl'
    with open(picklepath, 'w') as f:
      cPickle.dump(rf, f)
    print 'wrote model to %s' % picklepath
  if nq_val is not None:
    qids = all_qids[nq_train:nq_train + nq_val]
    print 'fetching validation set...'
    x,y = get_tr_batch(qids)
    print 'processing...'
    x = processRF(x, xf)
    print 'predicting...'
    pred = rf.predict(x)
    ndcg = rank_metrics.unsorted_ndcg(x.srch_id, y.rel, pred, 38)
    print 'NDCG val: %.3f' % ndcg
    et = time() - start
  print 'elapsed time: %f' % et
  return rf , xf                        
  
  
def processRF(x, f=None):
  process_x(x)
  scratch.enhance(x)
  f=None                                                #NB: hacked out to check something
  if f is not None:
    x = target_feature.merge_frame(x,f,'prop_id')
  return x
    
#TODO: predictRF doesn't take f or make the prop_id target feature

def predictRF(rf, submit_num, n_queries_chunk=20000):
  datafile = DATAPATH + DATABASE
  outpath = COMBINED_PATH % submit_num
  submitpath = SUBMIT_PATH % submit_num
  all_pred = np.array([], np.float64)
  with open(outpath, 'a') as f:
    f.write('SearchId,rel\n' )
    with sqlite3.connect(datafile) as conn:
      all_qids = sql.read_frame('select * from unique_qid_test', conn)
      all_qids = all_qids.srch_id.values
      idx = range(0,len(all_qids), n_queries_chunk)
      idx.append(len(all_qids))
      for k in range(len(idx) - 1):
        print 'predicting on test segment ' + str(k)
        qids = all_qids[idx[k]:idx[k+1]]
        sql_str = get_sql_str('test', qids, SELECT_X_COLS)       
        x_chunk = sql.read_frame(sql_str, conn)
        print 'loaded data segment ' + str(k)
        x_chunk = numpify(x_chunk)
        # adds relative features, handles comp_k's, fillna
        processRF(x_chunk) 
        pred = rf.predict(x_chunk)
        all_pred = np.concatenate((all_pred, pred))
        df_out = x_chunk[['srch_id']]
        df_out['rel'] = pred
        df_out.to_csv(f, header=False, index=False)
      df = sql.read_frame('select srch_id, prop_id from test', conn)
  df['rel'] = all_pred  
  df.sort_index(by=['srch_id', 'rel'], ascending=[1,0], inplace=True)
  del df['rel']
  df = df.values
  print 'writing...'
  with open(submitpath,'a') as f:
    f.write(HEADER)
    np.savetxt(f,df,fmt='%d',delimiter=',')

    
def runRF(submit_num, pickle_file, r_seed, ntrees, nq_train, nq_val=None):
  rf = trainRF(pickle_file, r_seed, ntrees, nq_train, nq_val)
  predictRF(rf, submit_num)


def write_letor(f, x, y=None):
  if type(f) is str:
    f = DATAPATH + f + L2REXT
  elif type(f) is not file:
    raise TypeError('f must be string or file')
  x = x.values
  # Make a dummy label vector for test, or turn y into a column vector for train.
  if y is None:
    y = np.ones((x.shape[0],1))
  else:
    y = y['rel'].values
    y.shape = (y.shape[0], 1)
  fmt = '%d qid:%d'
  # Tricky...If x has n columns then this iterates over 1...n-1 inclusive.
  # Column 0 is the srch_id, which is the query id in the LETOR format.
  # The features are supposed to be 1-based.
  for j in range(1, x.shape[1]):
    fmt = fmt + ' ' + str(j) + ':%.3f'
  x = np.hstack((y,x))
  np.savetxt(f, x, fmt=fmt)

def make_test(file_base, n_queries_chunk=20000):
  datafile = DATAPATH + DATABASE
  with sqlite3.connect(datafile) as conn:
    all_qids = sql.read_frame('select * from unique_qid_test', conn)
    all_qids = all_qids.srch_id.values
    #Tricky : note that we can just append length(all_qids)
    #because we can never get len(all_qids) in the range.
    idx = range(0,len(all_qids), n_queries_chunk)
    idx.append(len(all_qids))
    #Tricky again: we want to slice from idx[k] to idx[k+1]
    #so we need to stop at the next-to-last element of idx.
    #This is 0...<index of 2nd to last element of idx>, inclusive.
    for k in range(len(idx) - 1):
      print 'starting test segment ' + str(k)
      qids = all_qids[idx[k]:idx[k+1]]
      sql_str = get_sql_str('test', qids, SELECT_X_COLS)
      x_chunk = sql.read_frame(sql_str, conn)
      print 'loaded data segment ' + str(k)
      x_chunk = numpify(x_chunk)
      process_x(x_chunk)
      #processRF(x_chunk)                           # Modified in last-day hail Mary....
      filestr = file_base + str(k)
      write_letor(filestr, x_chunk)
      print 'wrote test file ' + filestr
      
def submit(file_base, count, submit_num):
  #usage: submit('clean1Mtest-clean1M, 13, 5)
  #Will use the output files clean1Mtest-clean1M-k.csv
  #for k=0..13 inclusive to write submit5.csv
  datafile = DATAPATH + DATABASE
  submitfile = SUBMIT_PATH % submit_num
  with sqlite3.connect(datafile) as conn:
    with open(submitfile, 'a') as f:
      f.write(HEADER)
      for k in range(count+1):
        scorefile = SCORE_PATH % (file_base, k)
        df = pd.io.parsers.read_table(scorefile, header=None, names=['qid', 'pos', 'score'])
        #pdb.set_trace()
        qids = df.qid.values
        qids = np.unique(qids)
        sql_str = get_sql_str('test', qids, 'srch_id, prop_id')
        print 'reading...' + str(k)
        x = sql.read_frame(sql_str, conn)
        x.index = df.index
        x['score'] = df['score']
        x.sort_index(by=['srch_id', 'score'], ascending=[1,0], inplace=True)
        del x['score']
        x = x.values
        print 'writing...' + str(k)
        np.savetxt(f,x,fmt='%d',delimiter=',')
        

def write_tr_batches(base_name, 
                     n_queries_train, 
                     n_queries_val, 
                     r_seed=42,
                     ordered_only=False,
                     n_queries_chunk=10000):
  train_filename = DATAPATH + base_name + 'train' + L2REXT
  val_filename = base_name + 'val'
  datafile = DATAPATH + DATABASE
  with sqlite3.connect(datafile) as conn:
    all_qids = sql.read_frame('select * from unique_qid_train', conn)
  all_qids = all_qids['srch_id'].values          
  np.random.seed(r_seed)
  np.random.shuffle(all_qids)
  qids = all_qids[n_queries_train:n_queries_train + n_queries_val]
  x,y = get_tr_batch(qids)
  process_x(x)
  write_letor(val_filename,x,y)
  
  idx = range(0, n_queries_train, n_queries_chunk)
  idx.append(n_queries_train)
  with open(train_filename,'a') as f:
    for k in range(len(idx) - 1):
      qids = all_qids[idx[k]:idx[k+1]]
      x, y = get_tr_batch(qids)
      process_x(x)
      if ordered_only:
        tr_ordered_idx = (x.random_bool == 0)
        tr_ordered_idx[:len(tr_ordered_idx)/3]=1
        write_letor(f, x.iloc[tr_ordered_idx, :], y.iloc[tr_ordered_idx, :])
      else:
        write_letor(f,x,y)
    
def quick_ordered_tr(base_name, nq_train, nq_val):
  train_file = base_name + '-orderedtrain'
  val_file = base_name + '-mixedval'
  xtr, xval, ytr, yval = get_train_val(nq_train, nq_val)
  print 'data loaded'
  process_x(xtr)
  process_x(xval)
  tr_ordered_idx = (xtr.random_bool == 0)
  tr_ordered_idx[:len(tr_ordered_idx)/3]=1
  print 'writing'
  write_letor(train_file, xtr.iloc[tr_ordered_idx, :], ytr.iloc[tr_ordered_idx, :])
  print 'wrote training data to ' + train_file + L2REXT
  write_letor(val_file, xval, yval)
  print 'wrote validation data to ' + val_file + L2REXT

def quick_split(base_name, nq_train, nq_val):
  # 
  train_file = base_name + 'train'
  val_file = base_name + 'val'
  xtr, xval, ytr, yval = get_train_val(nq_train, nq_val)
  print 'data loaded'
  process_x(xtr)
  process_x(xval)
  tr_rand_idx = (xtr.random_bool.values == 1)
  val_rand_idx = (xval.random_bool.values == 1)
  print 'writing random sets'
  write_letor('rand' + train_file, xtr.iloc[tr_rand_idx,:], ytr.iloc[tr_rand_idx,:])
  print 'wrote random training data to ' + 'rand' + train_file + L2REXT
  write_letor('rand' + val_file, xval.iloc[val_rand_idx,:], yval.iloc[val_rand_idx,:])
  print 'wrote random validation data to ' + 'rand' + val_file + L2REXT
  print 'writing ordered sets'
  write_letor('ord' + train_file, xtr.iloc[~tr_rand_idx,:], ytr.iloc[~tr_rand_idx,:])
  print 'wrote ordered training data to ' + 'ord' + train_file + L2REXT
  write_letor('ord' + val_file, xval.iloc[~val_rand_idx,:], yval.iloc[~val_rand_idx, ])
  print 'wrote ordered validation data to ' + 'ord' + val_file + L2REXT



def quick_tr_val(base_name, ntrain, nval, r_seed=42):
  train_file = base_name + 'train'
  val_file = base_name + 'val'
  xtr, xval, ytr, yval = get_train_val(ntrain, nval, r_seed)
  print 'data loaded'
  # Modified to use processRF in last-day try
  processRF(xtr)
  processRF(xval)
  print 'writing'
  write_letor(train_file, xtr, ytr)
  print 'wrote training data to ' + train_file + L2REXT
  write_letor(val_file, xval, yval)
  print 'wrote validation data to ' + val_file + L2REXT  
    
    