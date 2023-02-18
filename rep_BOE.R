
# ------------------------------------------------------------------------------
# About this code
# replication of Bluwstein et al. (2020)
# https://ideas.repec.org/p/boe/boeewp/0848.html
# seems there are several versions... (ECB working paper and so on)
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
  library("themis")

# ------------------------------------------------------------------------------
# Download the JST data
# ------------------------------------------------------------------------------
  
  # download the JST data
  df.JST.original <- 
    read_dta("http://data.macrohistory.net/JST/JSTdatasetR5.dta")
  
  # check the entire sample size and compare with the original paper
  df.JST.original %>% 
    select(year, country, crisisJST) %>% 
    skim()

  # it seems to be updated from the BOE paper
  # ... 18 developed countries are included: recent data about Ireland is now included
  # ... 2017 data is now available
  
# ------------------------------------------------------------------------------
# preprocess the data to be consistent with the BOE paper
# ------------------------------------------------------------------------------
  
  # process the domestic variables following the definition of the BOE paper
  df.JST.domestic <- 
    df.JST.original %>% 
    # group by the country
    group_by(country) %>% 
    # define the outcome variable: 1 and 2 year before the crisis is the target
    mutate(crisis.f1 = lead(crisisJST, n = 1),
           crisis.f2 = lead(crisisJST, n = 2)) %>%
    mutate(crisis = if_else(
      crisis.f1 == 1 | crisis.f2 == 1, 1, 0
    )) %>% 
    # factorize the outcome variable: not numeric
    mutate(crisis = as.factor(crisis)) %>% 
    # define some features
    mutate(slope       = ltrate - stir,        # slope of the yield curve
           tloans.pgdp = tloans / gdp,         # credit per GDP
           money.pgdp  = money / gdp,          # money per GDP
           ca.pgdp     = ca / gdp,             # current account per GDP
           dsr         = tloans * ltrate / gdp # debt service ratio
             ) %>% 
    # define some variables as 2-year difference of GDP-ratio * 100
    mutate(
      credit = (tloans.pgdp - lag(tloans.pgdp, n = 2)) * 100, # credit
      dsr    = (dsr         - lag(dsr,         n = 2)) * 100, # debt service ratio
      iy     = (iy          - lag(iy,          n = 2)) * 100, # investment
      pdebt  = (debtgdp     - lag(debtgdp,     n = 2)) * 100, # public debt
      money  = (money.pgdp  - lag(money.pgdp,  n = 2)) * 100, # money
      ca     = (ca.pgdp     - lag(ca.pgdp,     n = 2)) * 100  # current account
    ) %>% 
    # define some variables as 2-year growth rate of index
    mutate(pi     = cpi    / lag(cpi,    n = 2) * 100 - 100, # growth rate of the price level
           rcon   = rconpc / lag(rconpc, n = 2) * 100 - 100, # growth rate of the consumption
           equity = ((1 + eq_tr) * (1 + lag(eq_tr, n = 1)) - 1) * 100, # growth rate of the equity price
           ) %>% 
    # create a dummy to exclude the crisis year and four subsequent years
    mutate(crisis.ex = if_else(
      crisisJST == 1 | 
        lag(crisisJST, n = 1) == 1 | 
        lag(crisisJST, n = 2) == 1 | 
        lag(crisisJST, n = 3) == 1 | 
        lag(crisisJST, n = 4) == 1,
      1, 0
    )) %>% 
    # exclude the crisis year and the four subsequent years
    filter(crisis.ex == 0) %>% 
    # exclude some unusual periods
    filter(!(year >= 1933 & year <= 1939)) %>% # exclude the great depression period
    filter(!(year >= 1914 & year <= 1918)) %>% # exclude the WW1 period
    filter(!(year >= 1939 & year <= 1945)) %>%  # exclude the WW2 period
    # reset the grouping
    ungroup() %>% 
    # keep only the relevant variables
    select(year, country, crisis, slope, credit, equity,
           dsr, rcon, iy, ca, pdebt, money, pi) %>% 
    # drop rows containing NA values
    na.omit() 
  
  # df.JST.domestic now includes only the domestic data
  # calculate global variables: global yield curve slope and global credit
  
  # obtain the name of all the countries in the data
  countries.JST <- 
    df.JST.domestic %>% 
    select(country) %>% 
    unique() %>% 
    unlist() %>% 
    as.vector()

  # create an empty data frame to stack the data
  df.JST.global <- 
    data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
  colnames(df.JST.global) <- c("country", "year", "slope.global", "credit.global")
  
  # repeat the process for all the countries
  for (target.country in countries.JST) {
    # create global variables as the average of other countries
    df.JST.global.tmp <- 
      # the base data is domestic data
      df.JST.domestic %>% 
      # exclude the target country from the data
      filter(country != target.country) %>% 
      # group by each year
      group_by(year) %>% 
      # calculate the global factor as the average of other countries
      summarise(slope.global  = mean(slope),
                credit.global = mean(credit)) %>% 
      # leave the name of the target country
      mutate(country = target.country) %>% 
      ungroup() %>% 
      select(country, year, slope.global, credit.global)
    
    # stack the data into the global data frame
    df.JST.global <- 
      rbind(df.JST.global, df.JST.global.tmp)  
  }
  
  # merge the domestic and global data into a single master data frame
  df.JST <- 
    df.JST.domestic %>% 
    # keep only the samples available in the domestic data frame: left join
    left_join(df.JST.global, by = c("year", "country"))
  
  # done! this procedure leaves us 1260 observations
  
# ------------------------------------------------------------------------------
# split the data into training and testing samples
# ------------------------------------------------------------------------------
  
  # set the seed
  set.seed(2292)
  
  # split the data: stratify with the crisis (it's a rare event!)
  df.JST.split <- 
    df.JST %>% 
    initial_split(strata = crisis) 
  
  # split into training and testing samples
  # now the proportion is 75:25 (adjustable if needed)
  df.JST.train <- 
    training(df.JST.split)
  
  df.JST.test <- 
    testing(df.JST.split)
  
# ------------------------------------------------------------------------------
# Define recipes to process the features
# ... to improve the accuracy and interpretability of the model
# ------------------------------------------------------------------------------
  
  # define a base formula
  formula.base <- 
    as.formula(crisis ~ credit + credit.global + slope + slope.global + 
                 pi + money + equity + rcon + pdebt + iy + ca + dsr)
  
  # create a base recipe 
  recipe.base <- 
    recipe(x = df.JST.train, formula = formula.base) %>% 
    # scale all the predictors to be distributed ~ N(0,1) 
    step_scale(all_predictors()) 
  
  # important! This classification is highly biased toward normal regime,
  # which leads to a terrible prediction power of the crisis
  # Thus here I employed some algorithms to deal with this imbalance
  # otherwise the result will be terrible...

  # create a recipe using down sampling
  recipe.downsampling <- 
    recipe.base %>% 
    # conduct down sampling to deal with the unbalanced data
    # here under ratio is 1 (default)... can be modified
    step_downsample(all_outcomes(), seed = 2292, skip = TRUE)

  # create a recipe using SMOTE algorithm
  # this is a kind of up sampling, but more advanced (imputes the data from knn)
  recipe.smote <- 
    recipe.base %>% 
    # conduct smote: over ratio is adjustable
    step_smote(all_outcomes(), seed = 2292, skip = TRUE, 
               over_ratio = 0.25, neighbors = 5)
  

# ------------------------------------------------------------------------------
# Define models for the prediction
# ------------------------------------------------------------------------------
  
  # logistic regression for classification
  lr.spec <- 
    logistic_reg() %>% 
    set_engine("glm") %>% 
    set_mode(mode = "classification")
  
# ------------------------------------------------------------------------------
# Set the workflow
# ------------------------------------------------------------------------------
  
  # define the workflow
  wflow.JST <-
    workflow() %>% 
    # add the recipes here
    add_recipe(recipe = recipe.base) %>% 
    # add the models here
    add_model(spec = lr.spec)
  
# ------------------------------------------------------------------------------
# Fit the model
# ------------------------------------------------------------------------------
  
  # fit the model
  fitted.JST <- 
    wflow.JST %>% 
    fit(data = df.JST.train)

# ------------------------------------------------------------------------------
# Evaluate the result
# ------------------------------------------------------------------------------

  # create a data frame containing both true data and prediction
  df.JST.result <- 
    augment(fitted.JST, new_data = df.JST.test)
  
  # check the confusion matrix
  df.JST.result %>% 
    conf_mat(truth = crisis, estimate = .pred_class)
  # ... seems most of the heating is missed (why?)
  
  # check the ROC
  roc_curve(df.JST.result, crisis, .pred_1) %>% 
    autoplot()

  # check ROC AUC
  roc_auc(df.JST.result, crisis, .pred_1)
  
  
  