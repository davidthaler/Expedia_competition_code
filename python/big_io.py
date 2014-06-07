import numpy as np
import pandas as pd
import pdb
from pandas.io.parsers import read_csv

H5_EXT = '.h5'
CSV_EXT = '.csv'

def csv2hdf(path, 
            infile, 
            outfile, 
            na_values='NULL', 
            usecols=None):
            
  outpath = path + '/' + outfile + H5_EXT
  store = pd.HDFStore(outpath)
  inpath = path + '/' + infile + CSV_EXT
  
  #pdb.set_trace()
  
  reader = read_csv(inpath, 
                    iterator=True,  
                    chunksize=100000, 
                    na_values=na_values, 
                    usecols=usecols)
  
  for chunk in reader:
    store.append('df', chunk)