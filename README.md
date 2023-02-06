## Description of the codes

### createDB.R
Run this code first to download macroeconomic data including
- GDP 
- private consumption
- private investment

all of which are in real, quartery and seasonally adjusted. Data source differs according to countries.
- US: FRED
- JP: Cabinet office of Japan (see the excel file for more details)
- EU countries: Eurostat through Econdb

Data for G7 countries except for Canada will be available by running this code. 
Note that I obtain the data from Econdb through API intended for Python. 
Thus I translate the following python code (util/Econdb.py) into R using retuculate package.

### util/Econdb.py
Gets data through Econdb. As you can see, "Econdb" class has two functions.
- df_m: returns the monthly data frame
- df_q: returns the quarterly average data frame even if monthly series are included

Most of the codes are assumed to be used in R, but if you are familiar with Python, then you can use this class directly. 
See "usage_example.py" for an example of how to use this class.

### filter.R
This is the main code to filter the data. Available filtering methods now includes
- HP filter
- Simple Kalman filter

As for the simple Kalman filter, the state equations
```math
y_t^* = y_{t-1}^* + g_{t-1}    
```
```math
g_t = g_{t-1} + v_t
```
and the observation equation
```math
y_t = y_t^* + w_t
```
are assumed. Error terms are normally distributed, and the variances are estimated by the maximum likelihood estimation.  
Filtering can be conducted through plot_hp and plot_kalman functions defined in "util/hpfilter.R" and "util/kalmanfilter.R", respectively.  
Both functions accepy the following inputs.
- df: data frame which contains the series to be filtered \<tibble\>
- ctry: the country \<character\>
- indicator: specify the name of series \<character\>
- freq (HP filter only): frequency of the series, should be 4 or 12 \<double\>
- save: specify if you want to save the figures in "6/output" folder \<bool\>
- out.type: specify if you want "data", "figure", "both" or "neither" as the output \<chracter\>  

More sophisticated Kalman filtering method proposed by Holston-Laubach-Williams(2017) will be added soon.
