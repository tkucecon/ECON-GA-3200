
# ------------------------------------------------------------------------------
# About this code
# written by Takanori Takeuchi(tt2292@nyu.edu)
# Kalman Filtering with a simple assumption about the DGP, which is

  # state equation
    # y_t^* = y_t-1^* + g_t-1
    # g_t   = g_t-1 + v_t
  
  # observation equation
    # y_t = y_t^* + w_t
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("MARSS")
  library("ggthemes")
  theme_set(theme_solarized())
  
  options(warn = -1)
  
  # safunc needed to run this code:
  # source("safunc.R")
  
# create a function
  
plot_kalman <- function(df, ctry, indicator, freq, save, out.type){
    
  # drop NA rows in advance
  df <- 
    df %>% 
    drop_na()
  
  # remove the seasonal component
  ts.nsa <- 
    df %>% 
    filter(country == ctry) %>% 
    select(indicator) %>%
    unlist() %>% 
    as.numeric() %>% 
    ts(frequency = freq)
  
  # and save as a matrix
  ts.sa <- 
    sa_adj(ts.nsa) %>% 
    log() %>% 
    as.matrix() %>% 
    t()
  
# model setup
  
  # coefficients of state equation
  B1 <- matrix(c(1, 1, 0, 1), 2, 2, byrow = TRUE)
  # no time trends
  U1 <- matrix(0, 2, 1)
  # variances of the state equation
  Q1 <- matrix(list(0, 0, 0, "v"), 2, 2, byrow = TRUE)
  
  # coefficients of observation equation
  Z1 <- matrix(c(1, 0), 1, 2, byrow = TRUE)
  # no time trends
  A1 <- matrix(0, 1, 1)
  # variances of the observation equation
  R1 <- matrix('w', 1, 1, byrow = TRUE)

  # guess for the initial condition
  pi1 <- matrix(c(log(ts.sa[1]), 0), 2, 1, byrow = TRUE)
  V1  <- diag(1, 2)
  
  # combine all the model specifications
  model.list <- list(B = B1, U = U1, Q = Q1, 
                     Z = Z1, A = A1, R = R1, 
                     x0 = pi1, V0 = V1, tinitx = 0)

  # fit the model
  fit <- MARSS(y = ts.sa, model = model.list, fit = TRUE)

  # create a data frame
  df.plot <- 
    df %>% 
    filter(country == ctry) %>% 
    select(date) %>%
    cbind(as_tibble(t(fit$states)), 
          as_tibble(t(fit$states.se)), 
          t(ts.sa)) 
  
  colnames(df.plot) <- c("date", "potential", "g", "potential.se", "g.se", "actual")
  
  df.plot <- 
    df.plot %>% 
    mutate(g    = 100 * g, 
           g.se = 100 * g.se)
  
  # plot to compare y* and y 
  g.potential <- 
    df.plot %>% 
    mutate(upper = potential + potential.se * 1.96,
           lower = potential - potential.se * 1.96) %>% 
    select(date, potential, actual, upper, lower) %>% 
    gather(key = "key", value = "value", actual, potential) %>% 
    ggplot() + 
    geom_ribbon(aes(x = date,
                    ymin = lower,
                    ymax = upper),
                fill = "gray",
                alpha = 0.5) + 
    geom_line(aes(x = date, y = value, color = key), linewidth = 0.7) + 
    theme(legend.position = c(0.2, 0.85)) + 
    labs(y = expression(paste({y[t]}, "*", sep = "")), color = NULL)

  # plot the output gap
  g.gap <- 
    df.plot %>% 
    mutate(upper = potential + potential.se * 1.96,
           lower = potential - potential.se * 1.96) %>% 
    mutate(gap       = (actual - potential) * 100,
           upper.gap = (actual - lower) * 100,
           lower.gap = (actual - upper) * 100)%>% 
    select(date, gap, upper.gap, lower.gap) %>% 
    ggplot() + 
    geom_ribbon(aes(x = date,
                    ymin = lower.gap,
                    ymax = upper.gap),
                fill = "gray",
                alpha = 0.5) + 
    geom_line(aes(x = date, y = gap), linewidth = 0.7) + 
    geom_hline(yintercept = 0, linewidth = 0.5) +
    labs(y = expression(paste({w[t]}, ", % deviation", sep = "")))
    
  if (save) {
    ggsave(plot = g.gap,       width = 5, height = 4, filename = paste("../6_outputs/kalman_", ctry, indicator, "_gap.pdf", sep = ""))
    ggsave(plot = g.potential, width = 5, height = 4, filename = paste("../6_outputs/kalman_", ctry, indicator, "_potential.pdf", sep = ""))
  }
  
  if (out.type == "data") {
    return(df.plot)
  } else if (out.type == "figure") {
    return(list(g.gap, g.potential))
  } else if (out.type == "both"){
    return(list(df.plot, g.gap, g.potential))
  } else {
    print("out.type should be data, figure or both.")
  }
}