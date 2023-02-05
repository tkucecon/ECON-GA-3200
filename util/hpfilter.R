
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
  
  options(warn = -1)

# create a function
  
plot_hp <- function(df, ctry, indicator, freq, save){
  
  # save lambda according to the frequency
  if (freq == 4) {
    lamb <- 1600
  } else if (freq == 12) {
    lamb <- 14400
  } else {
    print("the data should be monthly or quarterly!")
  }
  
  # remove the seasonal component
  ts.nsa <- 
    df %>% 
    drop_na() %>% 
    filter(country == ctry) %>% 
    select(indicator) %>%
    unlist() %>% 
    as.numeric() %>% 
    ts(frequency = freq)
  
  ts.stl <- stl(ts.nsa, s.window = "periodic")
  
  # Save the seasonally adjusted data
  ts.sa <- ts.stl$time.series[,2] + ts.stl$time.series[,3]
  
  # Conduct HP filter
  ts.filtered <- hpfilter(log(ts.sa), freq = lamb, type = "lambda")
  
  # find the date
  ts.date <- 
    df %>% 
    filter(country == ctry) %>% 
    select(date)
  
  # Save the data
  df.plot <- 
    ts.date %>% 
    cbind(actual = log(ts.sa),
          trend = ts.filtered$trend, 
          cycle = ts.filtered$cycle * 100) %>% 
    rename(trend = 'Series 1')
  
  # Plot
  g.cycle <- 
    df.plot %>% 
    ggplot(aes(x = date, y = cycle)) +
    geom_line() +
    geom_hline(yintercept = 0) +
    labs(y = "% deviation from trend")
  
  g.compare <- 
    df.plot %>% 
    select(-cycle) %>% 
    gather(key = "key", value = "value", actual, trend) %>% 
    ggplot(aes(x = date, y = value, color = key)) + 
    geom_line() + 
    labs(color = NULL) + 
    theme(legend.position = c(0.15, 0.85))
  
  if (save) {
    ggsave(plot = g.cycle,   width = 5, height = 4, filename = paste("../6_outputs/", ctry, indicator, "_cyc.pdf", sep = ""))
    ggsave(plot = g.compare, width = 5, height = 4, filename = paste("../6_outputs/", ctry, indicator, "_comp.pdf", sep = ""))
  }
  
  return(list(g.cycle, g.compare))
}


    