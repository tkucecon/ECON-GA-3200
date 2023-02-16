
# ------------------------------------------------------------------------------
# About this code
# replicate the BOE paper
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("haven")
  library("ggthemes")
  theme_set(theme_solarized())
  
  library("skimr")
  library("tidymodels")

# ------------------------------------------------------------------------------
# load the JST data
# ------------------------------------------------------------------------------
  
  # download the JST data
  df.JST.original <- 
    read_dta("http://data.macrohistory.net/JST/JSTdatasetR5.dta")
  
  # process the data
  df.JST <- 
    df.JST.original %>% 
    group_by(iso) %>% 
    mutate(crisis.f1 = lead(crisisJST, n = 1),
           crisis.f2 = lead(crisisJST, n = 2)) %>%
    mutate(crisis = if_else(
      crisis.f1 == 1 | crisis.f2 == 1, 1, 0
    )) %>% 
    mutate(crisis = as.factor(crisis)) %>% 
    mutate(slope = ltrate - stir, # slope of the yield curve
           tloans.pgdp = tloans / gdp, # credit per GDP
           money.pgdp = money / gdp, # money per GDP
           ca.pgdp = ca / gdp, # current account per GDP
           dsr   = tloans * ltrate / gdp # debt service ratio
             ) %>% 
    mutate(pi = cpi / lag(cpi, n = 1) * 100 - 100, # growth rate of the price level
           equity = eq_capgain * 100, # growth rate of the equity price
           rcon = rconpc / lag(rconpc, n = 1) * 100 - 100, # growth rate of the consumption
           credit = (tloans.pgdp - lag(tloans.pgdp)) * 100, # credit: differences of GDP-ratios
           money = (money.pgdp - lag(money.pgdp, n = 1)) * 100, # money: differences of GDP-ratios
           pdebt = (debtgdp - lag(debtgdp, n = 1)) * 100, # public debt: differences of GDP-ratios
           dsr = (dsr - lag(dsr, n = 1)) * 100, # debt service ratio: differences of GDP-ratios
           iy = (iy - lag(iy, n = 1)) * 100, # investment: differences of GDP-ratios
           ca = (ca.pgdp - lag(ca.pgdp)) * 100 # current account: differences of GDP-ratios
           ) %>% 
    mutate(crisis.ex = if_else(
      crisisJST == 1 | 
        lag(crisisJST, n = 1) == 1 | 
        lag(crisisJST, n = 2) == 1 | 
        lag(crisisJST, n = 3) == 1 | 
        lag(crisisJST, n = 4) == 1,
      1, 0
    )) %>% 
    filter(crisis.ex != 1) %>% # exclude the crisis year and the four subsequent years
    filter(!(year >= 1933 & year <= 1939)) %>% # exclude the great depression period
    filter(!(year >= 1914 & year <= 1918)) %>% # exclude the WW1 period
    filter(!(year >= 1939 & year <= 1945)) %>%  # exclude the WW2 period
    select(year, country, iso, crisis, slope, credit, equity,
           dsr, rcon, iy, ca, pdebt, money, pi) %>% # keep only the relevant variables
    na.omit()
  
  # set the seed
  set.seed(123)
  
  # split the data
  df.JST.split <- 
    df.JST %>% 
    initial_split(strata = crisis) 
  
  df.JST.train <- 
    training(df.JST.split)
  
  df.JST.test <- 
    testing(df.JST.split)

# ------------------------------------------------------------------------------
# Create a set of workflows
# ------------------------------------------------------------------------------
  
  # create a set of formulas
  JST.formulas <- 
    as.formula(crisis ~ slope + credit + equity +
               dsr + rcon + iy + ca + pdebt + money + pi)

  # define the spec of logistic regression
  lr_spec <- 
    logistic_reg() %>% 
    set_engine("glm") %>% 
    set_mode(mode = "classification")

  # fit the model
  lr.fit <- 
    lr_spec %>% 
    fit(JST.formulas, data = df.JST.train)
  
  # evaluate the model
  augment(lr.fit, new_data = df.JST.test) %>%
    roc_curve(crisis, .pred_1) %>% 
    autoplot()
  
  augment(lr.fit, new_data = df.JST.test) %>%
    roc_auc(crisis, .pred_1) 
  
  augment(lr.fit, new_data = df.JST.test) %>% 
    conf_mat(truth = crisis, estimate = .pred_class)
    