
# ------------------------------------------------------------------------------
# About this code
# written by Takanori Takeuchi(tt2292@nyu.edu)
# seasonal adjustment
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  options(warn = -1)

# create a function
  
sa_adj <- function(ts.nsa){
  
  # conduct seasonal adjustment
  ts.stl <- stl(ts.nsa, s.window = "periodic")
  
  # Save the seasonally adjusted data
  ts.sa <- ts.stl$time.series[,2] + ts.stl$time.series[,3]
}


    