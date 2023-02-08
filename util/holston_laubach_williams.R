
# ------------------------------------------------------------------------------
# About this code
# originally comes from FRB NY website
# see the url for detailed explanation
# https://www.newyorkfed.org/research/policy/rstar
# edited by Takanori Takeuchi(tt2292@nyu.edu)
# Now accepts our macro dataset to conduct HLW estimation
# ------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
# Load required packages and source all programs to be used in HLW estimation.
#------------------------------------------------------------------------------#

library("tidyverse")
library("lubridate")

#------------------------------------------------------------------------------#
# create a function
#------------------------------------------------------------------------------#

plot_hwl <- function(df, ctry, smoothing, save, out.type){
  
  # Source all R programs; see code guide for details of each
  source("HLW_Code/calculate.covariance.R")
  source("HLW_Code/format.output.R")
  source("HLW_Code/kalman.log.likelihood.R")
  source("HLW_Code/kalman.standard.errors.R")
  source("HLW_Code/kalman.states.R")
  source("HLW_Code/kalman.states.wrapper.R")
  source("HLW_Code/log.likelihood.wrapper.R")
  source("HLW_Code/median.unbiased.estimator.stage1.R") 
  source("HLW_Code/median.unbiased.estimator.stage2.R")
  source("HLW_Code/rstar.stage1.R")
  source("HLW_Code/rstar.stage2.R")
  source("HLW_Code/rstar.stage3.R")
  source("HLW_Code/run.hlw.estimation.R") 
  source("HLW_Code/unpack.parameters.stage1.R") 
  source("HLW_Code/unpack.parameters.stage2.R") 
  source("HLW_Code/unpack.parameters.stage3.R") 
  source("HLW_Code/utilities.R")
  
  # I assume that run.se is FLASE for now (maybe update later)
  run.se <- FALSE
  
  # inflation expectation should be the 4-quarter moving average of inflation
  # create inflation expectation and remove the NA rows.
  df <- 
    df %>% 
    mutate(E.PI = (PI + lag(PI, n = 1) + lag(PI, n = 2) + lag(PI, n = 3)) / 4) %>% 
    select(date, GDP, country, PI, POLIR, E.PI) %>% 
    drop_na()
  
  # keep only the country of interest
  df.est <- 
    df %>% 
    filter(country == ctry)
  
  # check the start and the end of the samples
  sample.sdate <- min(df.est$date)
  sample.edate <- max(df.est$date)
  
  # Set the start and end dates of the estimation sample
  # format is c(year,quarter)
  sample.start <- c(year(sample.sdate)+1, (month(sample.sdate)+2)/3)
  sample.end   <- c(year(sample.edate),(month(sample.edate)+2)/3)
  
  # The estimation process uses data beginning 4 quarters prior to the sample start
  data.start    <- shiftQuarter(sample.start, -4)
  
  # Set start index for y
  g.pot.start.index <<- 1 + ti(shiftQuarter(sample.start,-3),'quarterly')-ti(data.start,'quarterly')
  
  # Set number of iterations for Monte Carlo standard error procedure
  niter <- 5000
  
  #------------------------------------------------------------------------------#
  # run estimation and save the output
  #------------------------------------------------------------------------------#
  
  # Upper bound on a_3 parameter (slope of the IS curve)
  a3.constraint <<- -0.25
  
  # Lower bound on b_2 parameter (slope of the Phillips curve)
  b2.constraint <<- 0.1
  
  # data inputs
  log.output             <- log(df.est$GDP) 
  inflation              <- df.est$PI
  inflation.expectations <- df.est$E.PI
  nominal.interest.rate  <- df.est$POLIR
  real.interest.rate     <- nominal.interest.rate - inflation.expectations
  
  
  # Run HLW estimation
  est.result <- run.hlw.estimation(log.output = log.output, 
                                   inflation  = inflation, 
                                   real.interest.rate = real.interest.rate, 
                                   nominal.interest.rate = nominal.interest.rate,
                                   a3.constraint = a3.constraint, 
                                   b2.constraint = b2.constraint, 
                                   run.se = run.se)
  
  # One-sided (filtered) estimates
  est.filtered <- 
    cbind(est.result$out.stage3$rstar.filtered,
          est.result$out.stage3$trend.filtered,
          est.result$out.stage3$z.filtered,
          est.result$out.stage3$output.gap.filtered,
          est.result$out.stage3$potential.filtered) %>% 
    as_tibble()
  colnames(est.filtered) <- c("rstar","g","z","gap", "potential")
  
  # smoothed estimates
  est.smoothed <- 
    cbind(est.result$out.stage3$rstar.smoothed,
          est.result$out.stage3$trend.smoothed,
          est.result$out.stage3$z.smoothed,
          est.result$out.stage3$output.gap.smoothed,
          est.result$out.stage3$potential.smoothed) %>% 
    as_tibble()
  colnames(est.smoothed) <- c("rstar","g","z","gap", "potential")
  
  # create a data frame to be ploteed
  # if smoothing == TRUE: smoothed estimates
  # if smoothing == FALSE: one-sided estimates
  if (smoothing) {
    df.plot <- 
      cbind(df.est[-(1:4), ], est.smoothed) %>% 
      mutate(actual = log(GDP)) %>% 
      select(date, country, actual, potential, gap, rstar, g, z)
  } else {
    df.plot <- 
      cbind(df.est[-(1:4), ], est.filtered) %>% 
      mutate(actual = log(GDP)) %>% 
      select(date, country, actual, potential, gap, rstar, g, z)
  }
  
  # Plot the graphs
  g.gap <- 
    df.plot %>% 
    ggplot(aes(x = date, y = gap)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    labs(y = "% deviation")
  
  g.potential <- 
    df.plot %>% 
    select(date, potential, actual) %>% 
    gather(key = "key", value = "value", actual, potential) %>% 
    ggplot(aes(x = date, y = value, color = key)) + 
    geom_line(linewidth = 0.7) + 
    labs(y = "log of GDP", color = NULL) + 
    theme(legend.position = c(0.15, 0.85))
  
  g.rstar <- 
    df.plot %>% 
    ggplot(aes(x = date, y = rstar)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    labs(y = "natural r, %")
  
  # return the output
  if (save) {
    if(!dir.exists(paste("../6_outputs/", ctry, sep = ""))){
      dir.create(paste("../6_outputs/", ctry, sep = ""))
    }
    ggsave(plot = g.gap,       width = 5, height = 4, filename = paste("../6_outputs/", ctry, "/hlw_", ctry, "GDP_gap.pdf",   sep = ""))
    ggsave(plot = g.potential, width = 5, height = 4, filename = paste("../6_outputs/", ctry, "/hlw_", ctry, "GDP_trend.pdf", sep = ""))
    ggsave(plot = g.rstar,     width = 5, height = 4, filename = paste("../6_outputs/", ctry, "/hlw_", ctry, "_rstar.pdf", sep = ""))
  }
  
  if (out.type == "data") {
    return(df.plot)
  } else if (out.type == "figure") {
    return(list(g.gap, g.potential, g.rstar))
  } else if (out.type == "both"){
    return(list(df.plot, g.gap, g.potential, g.rstar))
  } else if (out.type == "neither"){
    print(paste(ctry, "HLW estimate ...complete", sep = " "))
  } else {
    print("out.type should be data, figure, both or neither.")
  }
  
}