import numpy as np
import pandas as pd
from pandas.io.parsers import read_csv

def dawes_ranking(full_data_path, full_submit_path=None, nrows=None):
   test = read_csv(full_data_path, 
                   na_values='NULL', 
                   usecols=['srch_id',
                            'prop_id',
                            'prop_starrating',
                            'prop_review_score',
                            'prop_location_score1',
                            'prop_location_score2',
                            'price_usd',
                            'click_bool',
                            'booking_bool'],
                            nrows=nrows)
   # low prices are better
   test.price_usd = -test.price_usd          
   # Pandas reads prop_starrating as an int.
   test.prop_starrating = test.prop_starrating.astype(np.float64)
   test.fillna(0, inplace=True)
   gp = test.groupby('srch_id')
   f = lambda z: (z - z.mean())/z.std()           
   data_cols = test.columns.values[2:7]
   for c in data_cols:
     test[c] = gp[c].transform(f)
   # zscoring could re-introduce NaNs
   test.fillna(0, inplace=True)              
   # 2 vars each for loc. & quality, but only 1 for price, so we re-weight
   test.price_usd = 2 * test.price_usd       
   test['pred'] = test[data_cols].mean(1)
   test.sort_index(by=['srch_id', 'pred'], ascending=[1,0], inplace=True)
   test.rename(columns={'srch_id':'SearchId', 'prop_id':'PropertyId'}, inplace=True)
   if full_submit_path is not None:
     test[['SearchId','PropertyId']].to_csv(full_submit_path, index=False)
   return test