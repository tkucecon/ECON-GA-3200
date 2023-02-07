
# ------------------------------------------------------------------------------
# About this code
# written by Takanori Takeuchi(tt2292@nyu.edu)
# HP filtering and Kalman filtering
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("mFilter")
  library("ggthemes")
  theme_set(theme_solarized())
  
# load the function
  source("util/hpfilter.R")
  source("util/kalmanfilter.R")
  
# load the GDP data-------------------------------------------------------------
  
  load("../4_data/df_macro.rda")

# conduct the filtering---------------------------------------------------------
  
  # single run: HP filtering
  plot_hp(df = df.macro,
          ctry = "JP",
          indicator = "GDP", 
          freq = 4, 
          save = TRUE, 
          out.type = "figure")
  
  # single run: Kalman filtering
  plot_kalman(df = df.macro,
              ctry = "JP",
              indicator = "GDP", 
              save = FALSE, 
              out.type = "figure")
  
  # check the countries
  countries <- 
    df.macro %>% 
    select(country) %>% 
    distinct() %>% 
    unlist() %>% 
    as.vector()
  
  # run for all countries: HP filtering
  sapply(X = countries, 
         FUN = plot_hp,
         df = df.macro,
         indicator = "GDP", 
         freq = 4, 
         save = TRUE, 
         out.type = "neither")
    
  # run for all countries: Kalman filtering
  sapply(X = countries, 
         FUN = plot_kalman,
         df = df.macro,
         indicator = "GDP", 
         save = TRUE, 
         out.type = "neither")
  