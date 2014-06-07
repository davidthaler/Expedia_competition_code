#NOTE: The two functions below are for row reduction - 
#using only the rows at the top of the list, or those above
# the lowest clicked item. This does not seem to work.

#NB: dead code
def filterRows(z):
  idx = z[z.click_bool == 1].position.max()
  if np.isnan(idx):
    return 0*z.position
  return z.position <= np.max((5, 1 + idx))
    
#NB: dead code
def reduce_rows(y):
 # returns the index of the rows that are no lower than 1 item
 # below the lowest click
  gp_y = y.groupby('srch_id')
  idx = gp_y.apply(filterRows)
  return idx
  
  
def clean(x):
  x[ZERO_IS_MISSING_COLS].replace(0, np.nan, inplace=True)
  x['no_prop_starrating'] = x['prop_starrating'].isnull().astype(int)
  x['no_prop_review_score'] = x['prop_review_score'].isnull().astype(int)
  x['no_prop_log_historical_price'] = x['prop_log_historical_price'].isnull().astype(int)
  x['no_prop_location_score2'] = x['prop_location_score2'].isnull().astype(int)
  gp = x.groupby('srch_id')
  f = lambda z: z.fillna(z.median())
  x[IMPUTE_MEDIAN_COLS] = gp[IMPUTE_MEDIAN_COLS].transform(f)
  x.fillna(0, inplace=True)
