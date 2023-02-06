
'''
About this code:
written by Takanori Takeuchi (tt2292@nyu.edu)
This code creates a 'get_df' function and 'Econdb' class which returns a data frame downloaded from Econdb with API
See the link below for the detailed information of Econdb
https://www.econdb.com/home
'''

# modules
import numpy as np
import pandas as pd
import pandas_datareader.data as web
from datetime import datetime

# create a function
def get_df(start, end, countries, indicators):

    # empty data frame to accomodate the downloaded data
    df_econ = pd.DataFrame()

    # repeatedly download the data through API and store in the data frame
    for country in countries:
        temp2 = pd.DataFrame()
        for indicator in indicators:
            temp = web.DataReader('ticker=' + indicator + country, 'econdb', start, end) # download data with API
            temp.columns = [indicator]
            temp2 = pd.concat([temp2, temp], join='outer', axis = 1)
        temp2 = temp2.assign(country = country, date = temp2.index)
        df_econ = pd.concat([df_econ, temp2], join = 'outer')
        df_econ = df_econ.reset_index(drop = True)
    
    # return the data frame
    return df_econ

# class Econdb:
class Econdb:
    def __init__(self, start, end, countries, indicators):
        self.start = start
        self.end = end
        self.countries = countries
        self.indicators = indicators

    def df_m(self):
        df_m = get_df(self.start, self.end, self.countries, self.indicators)
        return df_m

    def df_q(self):
        df_m = get_df(self.start, self.end, self.countries, self.indicators)
        # set date as the index
        df_m.set_index(pd.to_datetime(df_m['date']), inplace = True)
        # calculate the quarterly average
        df_q = df_m.groupby('country').resample('Q', label = 'left').mean()
        return df_q


