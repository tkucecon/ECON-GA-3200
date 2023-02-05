
# ------------------------------------------------------------------------------
# About this code
# written by Takanori Takeuchi(tt2292@nyu.edu)
# Kalman Filtering 
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("reticulate")
  library("lubridate")
  library("MARSS")
  library("ggthemes")
  theme_set(theme_solarized())
  
  # load the function
  source("util/safunc.R")
  source("util/kalmanfilter.R")
  
# download data using the python module-----------------------------------------
  
# load the python module I created under util folder
  source_python("util/econdb.py")

# set up the parameters
  start <- as.Date("1996-1-1")
  end <- as.Date("2022-12-31")
  countries <- c('US', 'JP')
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

# conduct Kalman filtering------------------------------------------------------
  
  plot_kalman(df = df_q,
              ctry = "JP",
              indicator = "RGDP", 
              freq = 4, 
              save = TRUE, 
              out.type = "figure")
  