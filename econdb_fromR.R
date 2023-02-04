
# ------------------------------------------------------------------------------
# About this code
# written by Takanori Takeuchi(tt2292@nyu.edu)
# how to recycle the python code used in the util folder
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("reticulate")
  library("lubridate")

# Have you saved the default option for python interpreter in R?
# If not, then find the directory of your python and run the following code
# use_python("C:/Anaconda3/envs/ML/python.exe")

# download data using the python module-----------------------------------------

# load the python module I created under util folder
  source_python("util/econdb.py")

# set up the parameters
  start <- as.Date("1996-1-1")
  end <- as.Date("2022-12-31")
  countries <- c('US', 'JP', 'DE', 'CA', 'FR', 'IT', 'UK')
  indicators <- c('RGDP', 'CPI', 'IP', 'URATE', 'POLIR')

# load the Econdb class
  econdb <- Econdb(start, end, countries, indicators)

# get the monthly data
  df_m <- 
    econdb$df_m() %>% 
    mutate(date = ymd(as.Date(date)))
  
# get the quarterly data
  df_q <- 
    econdb$df_q()

  df_q <- 
    df_q %>% 
    cbind(py_to_r(attributes(df_q)$pandas.index$to_frame())) %>% 
    mutate(date = ymd(as.Date(date)))
  
# that's all! you can handle the data in R now!
  
