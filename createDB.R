
# ------------------------------------------------------------------------------
# About this code
# written by Takanori Takeuchi(tt2292@nyu.edu)
# Creates database for GDP and its components
# Readme: There are three sources of data for this dataset
# 1. FRED for the US
# 2. Cabinet Office of Japan for Japan (see 4.data folder for details)
# 3. Eurostat through Econdb for the Euro countries
# ↑ They are all constructed as seasonally adjusted quarterly real data
# 4. Econdb for other countries
#    Note that Econdb data covers the broad economic data worldwide
#    but not seasonally adjusted!
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

  # library
  library("tidyverse")
  library("reticulate")
  library("lubridate")
  library("readxl")
  library("tidyquant")

  # load the function
  source("util/safunc.R")

# 1. download data from FRED----------------------------------------------------

  # tickers for real GDP, private consumption and private investment
  tickers <- c("GDP", "PCEC", "GPDI")
  
  # download the data
  df.us <- 
    tq_get(tickers, get = "economic.data", from = "1994-01-01") %>% 
    mutate(price = price / 4) %>% # original data is annualized
    spread(key = "symbol", value = "price") %>% 
    mutate(country = "US")

# 2. Load the data for Japan----------------------------------------------------
  
  # load the data
  df.jp <- 
    read_excel("../4_data/xlsx/Japan.xlsx", sheet = "R", col_names = TRUE) %>% 
    mutate(GDP  = GDP / 4, 
           PCEC = PCEC / 4,
           GPDI = GPDI / 4) %>% 
    select(-gap.official) %>% 
    mutate(country = "JP")

# 3. Load the data from Eurostat through Econdb---------------------------------

  # Econdb is intended for Python use, so run the Python code through R
  
  # Have you saved the default option for python interpreter in R?
  # If not, then find the directory of your python and run the following code
  # use_python("C:/Anaconda3/envs/ML/python.exe")
  
  # Import Python Library
  
  
  # set up the parameters
  start <- as.Date("1994-1-1")
  end <- as.Date("2022-12-31")
  countries <- c('DE', 'FR', 'UK', 'IT')
  indicators <- c('NAMQ_10_GDP.19FD519F51M54F5.Q.', # Final consumption expenditure
                  'NAMQ_10_GDP.19FD619F51M54F5.Q.', # Final consumption expenditure of general government
                  'NAMQ_10_GDP.19FDD19F51M54F5.Q.', # gross capital formation
                  'NAMQ_10_GDP.19FD319F51M54F5.Q.') # gross domestic product
  
  # load the Econdb class
  econdb <- Econdb(start, end, countries, indicators)
  
  # download the data
  df.euro <- 
    econdb$df_m() %>% 
    mutate(date = ymd(as.Date(date))) %>% 
    mutate(GDP  = NAMQ_10_GDP.19FD319F51M54F5.Q.,
           PCEC = NAMQ_10_GDP.19FD519F51M54F5.Q. - NAMQ_10_GDP.19FD619F51M54F5.Q.,
           GPDI = NAMQ_10_GDP.19FDD19F51M54F5.Q.) %>% 
    select(date, country, GDP, PCEC, GPDI)
  
# 4. download data from Econdb--------------------------------------------------

  # I don't use this chunk for now but you can remove the comment out anytime 
  
  # # For other countries, you can download any variables you like from Econdb
  # countries <- c('DE', 'CA', 'FR', 'UK', 'IT')
  # indicators <- c('RGDP', 'RPRC')
  # 
  # # load the Econdb class
  # econdb <- Econdb(start, end, countries, indicators)
  # 
  # # get the quarterly data
  # df_q <-
  #   econdb$df_q()
  # 
  # df_q <-
  #   df_q %>%
  #   cbind(py_to_r(attributes(df_q)$pandas.index$to_frame())) %>%
  #   mutate(date = ymd(as.Date(date)))

# Merge the data----------------------------------------------------------------
  
  # Now merge all the data and save as the master db for filtering
  df.gdp <- 
    rbind(df.us, df.jp, df.euro)
  
  # save in the data folder
  save(df.gdp, file = "../4_data/df_gdp.rda")
  