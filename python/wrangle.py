import numpy as np
import pandas as pd
from pandas.io import sql
from pandas.io.parsers import read_csv
from pandas.io.parsers import read_table
import sqlite3
import pdb

SCORE_PATH = '../output/%s-%d.csv'
OUTPATH = '../output/combined-%s.csv'
HEADER = 'SearchId,rel\n' 
SUBMIT_HEADER = 'SearchId,PropertyId\n' 
SUBMIT_PATH = '../submissions/submit%d.csv'

def concatenate_predictions(base_file, max_file_num):
  outfile = OUTPATH % base_file
  with open(outfile, 'a') as f_out:
    f_out.write(HEADER)
    for k in range(max_file_num + 1):
      scorefile = SCORE_PATH % (base_file, k)
      with open(scorefile) as f_in:
        df_in = read_table(scorefile, 
                              header=None, names=['qid', 'pos', 'score'])
        del df_in['pos']
        df_in.to_csv(f_out, header=False, index=False)

def average_predictions(base_files, submit_num):
  with sqlite3.connect('../data/expedia.db') as conn:
    df0 = sql.read_frame('select srch_id, prop_id from test', conn)
  print 'db read'
  df0['rel'] = 0
  for basename in base_files:
    base_path = OUTPATH % basename
    df = read_csv(base_path, usecols=['rel'])
    print '%s read' % basename
    df.rel = df.rel - df.rel.mean()
    df.rel = df.rel/df.rel.std()
    df0['rel'] = df0['rel'] + df['rel']
  df0.sort_index(by=['srch_id', 'rel'], ascending=[1,0], inplace=True)
  del df0['rel']
  df0 = df0.values
  print 'writing'
  submit_file = SUBMIT_PATH % submit_num
  with open(submit_file, 'w') as f:
    f.write(SUBMIT_HEADER)
    np.savetxt(f,df0,fmt='%d',delimiter=',')  