
# ------------------------------------------------------------------------------
# About this code
# written by Takanori Takeuchi(tt2292@nyu.edu)
# conduct HP filtering
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("mFilter")
  library("ggthemes")
  theme_set(theme_solarized())
  
  options(warn = -1)
  
  # safunc needed to run this code:
  # source("safunc.R")

# create a function
  
plot_hp <- function(df, ctry, indicator, freq, save, out.type){
  
  # drop NA rows in advance
  df <- 
    df %>% 
    drop_na()
  
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
    filter(country == ctry) %>% 
    select(indicator) %>%
    unlist() %>% 
    as.numeric() %>% 
    ts(frequency = freq)

  ts.sa <- sa_adj(ts.nsa)
  
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
  g.gap <- 
    df.plot %>% 
    ggplot(aes(x = date, y = cycle)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    labs(y = "% deviation from trend")
  
  g.trend <- 
    df.plot %>% 
    select(-cycle) %>% 
    gather(key = "key", value = "value", actual, trend) %>% 
    ggplot(aes(x = date, y = value, color = key)) + 
    geom_line(linewidth = 0.7) + 
    labs(color = NULL) + 
    theme(legend.position = c(0.15, 0.85))
  
  if (save) {
    ggsave(plot = g.gap,   width = 5, height = 4, filename = paste("../6_outputs/hp_", ctry, indicator, "_gap.pdf", sep = ""))
    ggsave(plot = g.trend, width = 5, height = 4, filename = paste("../6_outputs/hp_", ctry, indicator, "_trend.pdf", sep = ""))
  }
  
  if (out.type == "data") {
    return(df.plot)
  } else if (out.type == "figure") {
    return(list(g.gap, g.trend))
  } else if (out.type == "both"){
    return(list(df.plot, g.gap, g.trend))
  } else {
    print("out.type should be data, figure or both.")
  }
}


    