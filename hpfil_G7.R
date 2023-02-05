
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
  library("mFilter")
  library("ggthemes")
  theme_set(theme_solarized())
  
# load the function
  source("util/hpfilter.R")

# download data using the python module-----------------------------------------
  
# Have you saved the default option for python interpreter in R?
# If not, then find the directory of your python and run the following code
# use_python("C:/Anaconda3/envs/ML/python.exe")

# load the python module I created under util folder
  source_python("util/econdb.py")

# set up the parameters
  start <- as.Date("1996-1-1")
  end <- as.Date("2022-12-31")
  countries <- c('US', 'JP', 'DE')
  indicators <- c('RGDP', 'RPRC')

# load the Econdb class
  econdb <- Econdb(start, end, countries, indicators)

# get the quarterly data
  df_q <- 
    econdb$df_q()

  df_q <- 
    df_q %>% 
    cbind(py_to_r(attributes(df_q)$pandas.index$to_frame())) %>% 
    mutate(date = ymd(as.Date(date))) 

# conduct the HP filtering------------------------------------------------------
  
  plot_hp(df = df_q,
          ctry = "DE",
          indicator = "RGDP", 
          freq = 4, 
          save = TRUE)
    