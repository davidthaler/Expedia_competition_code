import numpy as np
import pandas as pd
import pdb

SCORE_PATH = '../output/test-2M-%d.csv'
STORE_PATH =  '../data/test.h5'
SUBMIT_PATH = '../submissions/submit%d.csv'
HEADER = 'SearchId,PropertyId\n' 
CHUNK = 1000000

def submit(submit_num):
  store = pd.HDFStore(STORE_PATH)
  submitfile = SUBMIT_PATH % submit_num
  with open(submitfile, 'a') as f:
    f.write(HEADER)
    for k,x in enumerate(store.select('df', columns=['srch_id','prop_id'], chunksize=CHUNK)):
      scorefile = SCORE_PATH %k
      df = pd.io.parsers.read_table(scorefile, header=None, names=['qid', 'pos', 'score'])
      x.index = df.index
      x['score'] = df['score']
      x.sort_index(by=['srch_id', 'score'], ascending=[1,0], inplace=True)
      #pdb.set_trace()
      del x['score']
      x = x.values
      np.savetxt(f,x,fmt='%d',delimiter=',')
  store.close()
  
    