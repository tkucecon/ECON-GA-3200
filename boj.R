
# ------------------------------------------------------------------------------
# About this code
# Comparison of the official output gap and the estimates
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("readxl")
  library("tis")
  library("nloptr")
  library("ggthemes")
  theme_set(theme_solarized())
  
# load the function
  source("util/hpfilter.R")
  source("util/holston_laubach_williams.R")
  
# load the GDP data-------------------------------------------------------------
  
  load("../4_data/df_macro.rda")

# gather data-------------------------------------------------------------------
  
  # result of HP filtering
  df.hpfiltered <- 
    plot_hp(df = df.macro,
            ctry = "JP",
            indicator = "GDP", 
            freq = 4, 
            save = FALSE, 
            out.type = "data") %>% 
    rename(trend.hp = trend, 
           gap.hp = cycle)
  
  # read official potential output data
  df.official <- 
    read_excel("../4_data/xlsx/Japan.xlsx", sheet = "R", col_names = TRUE) %>% 
    select(date, gap.official)

  # result of HP filtering
  df.hlw <- 
    plot_hwl(df = df.macro,
             ctry = "JP",
             smoothing = FALSE,
             save = FALSE,
             out.type = "data") %>% 
    select(date, potential, gap) %>% 
    rename(trend.hlw = potential,
           gap.hlw = gap)
    
  source("util/rm_hlw.R")
  
  # merge all the data
  df.plot <- 
    df.hpfiltered %>% 
    inner_join(df.hlw, by = "date") %>% 
    inner_join(df.official, by = "date")

  # plot the output gap
  g.gap <- 
    df.plot %>% 
    select(date, gap.hp, gap.hlw, gap.official) %>% 
    rename(`HP filter` = gap.hp, `HLW (2017)` = gap.hlw, `BOJ` = gap.official) %>% 
    gather(key = "key", value = "value", -date) %>% 
    ggplot() + 
    geom_line(aes(x = date, y = value, color = key), linewidth = 0.7) + 
    geom_hline(yintercept = 0, linewidth = 0.7) +
    theme(legend.position = c(0.2, 0.2)) + 
    labs(y = "output gaps, %", color = NULL)

  # save
  ggsave(plot = g.gap, width = 5, height = 4, filename = "../6_outputs/JP/comparison_JPGDP_gap.pdf")
  
         