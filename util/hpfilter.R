
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
  
# create a function
  
plot_hp <- function(df, ctry, indicator, freq, save, out.type){
  
  # save lambda according to the frequency
  if (freq == 4) {
    lamb <- 1600
  } else if (freq == 12) {
    lamb <- 14400
  } else {
    print("the data should be monthly or quarterly!")
  }
  
  # convert the data into ts values
  ts.actual <- 
    df %>% 
    filter(country == ctry) %>% 
    select(indicator) %>%
    drop_na() %>% 
    unlist() %>% 
    as.numeric() %>% 
    log() %>% 
    ts(frequency = freq)

  # Conduct HP filter
  ts.filtered <- hpfilter(ts.actual, freq = lamb, type = "lambda")
  
  # find the date
  ts.date <- 
    df %>% 
    select(date, country, indicator) %>% 
    filter(country == ctry) %>% 
    drop_na() %>% 
    select(date)
  
  # Save the data
  df.plot <- 
    ts.date %>% 
    cbind(actual = ts.actual,
          trend  = ts.filtered$trend, 
          cycle  = ts.filtered$cycle * 100) %>% 
    rename(trend = 'Series 1') %>% 
    as_tibble()
  
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
    labs(y = "log of GDP", color = NULL) + 
    theme(legend.position = c(0.25, 0.85))
  
  if (save) {
    if(!dir.exists(paste("../6_outputs/", ctry, sep = ""))){
      dir.create(paste("../6_outputs/", ctry, sep = ""))
    }
    ggsave(plot = g.gap,   width = 3, height = 4, filename = paste("../6_outputs/", ctry, "/hp_", ctry, indicator, "_gap.pdf", sep = ""))
    ggsave(plot = g.trend, width = 3, height = 4, filename = paste("../6_outputs/", ctry, "/hp_", ctry, indicator, "_trend.pdf", sep = ""))
  }
  
  if (out.type == "data") {
    return(df.plot)
  } else if (out.type == "figure") {
    return(list(g.gap, g.trend))
  } else if (out.type == "both"){
    return(list(df.plot, g.gap, g.trend))
  } else if (out.type == "neither"){
    print(paste(ctry, indicator, "...complete", sep = " "))
  } else {
    print("out.type should be data, figure, both or neither.")
  }
}


    