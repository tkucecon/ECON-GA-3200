
# ------------------------------------------------------------------------------
# About this code
# Conduct all the filtering
# 1. HP filtering
# 2. simple Kalman filtering 
# 3. Holston-Laubach-Williams(2017) state-space estimation
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("mFilter")
  library("ggthemes")
  library("tis")
  library("nloptr")
  theme_set(theme_solarized())
  
# load the function
  source("util/hpfilter.R")
  source("util/kalmanfilter.R")
  source("util/holston_laubach_williams.R")
  
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
              save = TRUE, 
              out.type = "figure")
  
  # single run: Holston-Laubach-Williams estimation
  plot_hwl(df = df.macro,
           ctry = "JP",
           smoothing = FALSE,
           save = TRUE,
           out.type = "figure")
  source("util/rm_hlw.R")

# conduct the filtering at once?------------------------------------------------
  
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
  