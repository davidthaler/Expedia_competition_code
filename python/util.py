import pandas as pd
import numpy as np
import json 

BASE_PATH = '/Users/davidthaler/Documents/Kaggle/expedia/'
DATA_PATH = BASE_PATH + 'data/'
SAMPLE_PATH = DATA_PATH + 'sample.csv.gz'


  
  
def load_sample():
  return pd.read_csv(SAMPLE_PATH)


def basic_clean():
  x = load_sample()
  y = 4 * x.booking_bool + x.click_bool
  del x['booking_bool']
  del x['click_bool']
  del x['gross_booking_usd']
  del x['position']
  del x['date_time']
  x.fillna(0, inplace=True)
  return x, y