
'''
About this code:
written by Takanori Takeuchi (tt2292@nyu.edu)
This is just an example of how to use the 'get_df' function I created under 'util' folder
'''

# modules
import numpy as np
import pandas as pd
import pandas_datareader.data as web
from datetime import datetime

# import the 'Econdb' class
from util.econdb import Econdb

# set up parameters
start = datetime(1996, 1, 1) # Here I restrict data from 1996 for cross-country comparison: longer data available for some countries (like US)
end = datetime(2022, 12, 1) 
countries = ['US', 'JP', 'DE', 'CA', 'FR', 'IT', 'UK'] # G7 members (EU not included)
indicators = ['RGDP', 'CPI', 'IP', 'URATE', 'POLIR']

# create the class econdb
econdb = Econdb(start, end, countries, indicators)

# download the monthly data
df_G7_m = econdb.df_m()
df_G7_m.head(10)

# or you can download quarterly average data 
df_G7_q = econdb.df_q()
df_G7_q.head(10)