import numpy as np
import pandas as pd

class HDF_utils(object):

  def __init__(self, store_path, rootname):
    """
    Initialize object with the data store accessed.
    
    Parameters
    ----------
    
    store_path: the full path to the HDF data store accessed by this object.
    rootname: the name of the root object in the store.
    """
    self.store = pd.HDFStore(store_path)
    self.root = rootname
  
  def showColStats(self, colname):
    """
    Get a description and counts for a named column from the HDFstore.
    NB: this pulls that whole column into main memory
    
    Parameters
    ----------
    colname: the name of the column to describe
    """
    col = self.store.select_column(self.root, colname)
    desc = col.describe()
    n_unique = col.nunique()
    