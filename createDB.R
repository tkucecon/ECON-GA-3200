
# ------------------------------------------------------------------------------
# About this code
# Creates database for GDP and its components
# Readme: There are three sources of data for this dataset
# 1. FRED for the US
# 2. Cabinet Office of Japan for Japan (see 4.data folder for details)
# 3. Eurostat through Econdb for the Euro countries
# ↑ They are all constructed as seasonally adjusted quarterly real data
# 4. Econdb for other indices or countries
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

# 1. download data from FRED----------------------------------------------------

  # tickers for real GDP, private consumption and private investment
  tickers <- c("GDPC1", "PCEC96", "GPDIC1")
  
  # download the data
  df.us <- 
    tq_get(tickers, get = "economic.data", from = "1994-01-01") %>% 
    mutate(price = price / 4) %>% # original data is annualized
    spread(key = "symbol", value = "price") %>% 
    rename(GDP = GDPC1,
           PCEC = PCEC96,
           GPDI = GPDIC1)
  
  # data contains monthly consumption after 2002: should be quarter average
  df.us <- 
    df.us %>% 
    mutate(year    = year(date),
           quarter = quarter(date)) %>% 
    group_by(year, quarter) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(date = as.Date(paste(year, quarter * 3 - 2, 1, sep = "-"))) %>% 
    mutate(country = "US") %>% 
    select(date, country, GDP, PCEC, GPDI)
    
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
  
  # Import the file I used
  source_python("util/econdb.py")
  
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
# For other indices, you can download any variables you like from Econdb
  
  # set up the parameters
  start <- as.Date("1994-1-1")
  end <- as.Date("2022-12-31")
  countries <- c('US', 'JP', 'DE', 'FR', 'UK', 'IT', 'EA')
  indicators <- c('CPI', # consumer price index
                  'PCE', # personal consumption expenditure price index
                  # 'URATE', # unemployment rates
                  'POLIR', # policy rate (short term rate)
                  'Y10YD'# 10-year yield (long term rate)
                  )

  # load the Econdb class
  econdb <- Econdb(start, end, countries, indicators)

  # get the quarterly data
  df.rate <-
    econdb$df_q()
  df.rate <-
    df.rate %>%
    cbind(py_to_r(attributes(df.rate)$pandas.index$to_frame())) %>%
    mutate(date = ymd(as.Date(date)) + 1)
  
  # get the policy rates of EU area
  df.eapolir <- 
    df.rate %>% 
    filter(country == "EA") %>% 
    rename(POLIR.EA = POLIR) %>% 
    select(date, POLIR.EA)
  
  eu.countries <- c('DE', 'FR', 'UK', 'IT')
  
  # process the data 
  df.rate <-
    df.rate %>% 
    mutate(PRICE = ifelse(country =="US", PCE, CPI)) %>% 
    mutate(PI = PRICE / lag(PRICE, n = 4) * 100 - 100) %>% 
    left_join(df.eapolir, by = "date") %>% 
    mutate(POLIR = ifelse(country %in% eu.countries, 
                          POLIR.EA, POLIR)) %>% 
    select(date, country, PI, POLIR, Y10YD) %>% 
    filter(country != "EA")
    
# Merge the data----------------------------------------------------------------
  
  # Now merge all the GDP data 
  df.gdp <- 
    rbind(df.us, df.jp, df.euro)
  
  # then merge the quarterly data
  df.macro <- 
    left_join(df.gdp, df.rate, by = c('country', 'date'))

  # check if "4_data" folder exists
  # if not, create a folder
  if(!dir.exists("../4_data")){
    dir.create("../4_data")
  }
  
  # save as the master db for filtering in the 4_data folder
  save(df.macro, file = "../4_data/df_macro.rda")
  