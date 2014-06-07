import numpy as np
import pandas as pd
import pdb

# NB: This destroys df.
def make_frame(df, group_col, feature_col, smoothing, delete_count=False):
  df = df[[group_col, feature_col]]
  df[feature_col] = df[feature_col] - df[feature_col].mean()
  g = df.groupby(group_col)
  # This makes f a 1-column DataFrame, with column name <feature_col>,
  # which holds the group totals of the feature values. 
  # Its index is the unique values of the grouping variable.
  f = g.sum()
  f['ct'] = g.size()
  f['fval'] = f[feature_col]/(f['ct'] + smoothing)
  del f[feature_col]
  if delete_count:
    del f['ct']
  return f


def merge_frame(x, f, join_col_x):
  # For this to work, f has to have been grouped by the join variable already.
  # That leaves it as f's index.
  xm = pd.merge(x, f, how='left', left_on=join_col_x, right_index=True, copy=False)
  xm.fillna(0, inplace=True)
  return xm
  
  
  
  
  