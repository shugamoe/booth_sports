# Utility Functions for Working with Expected Points Data

library(tidyverse)
library(purrr)

EXP_DAT <- read.csv("../correc_raw_fdowns_nscore_half_and_reset.csv")

season_splits <- function(exp_pts_df, seas_col = "Season"){
  require(tidyverse)
  require(rlang)
  seas_col = sym(seas_col)
  (splits <- list(`2000-2016` = exp_pts_df,
                 `2000-2010` = exp_pts_df %>%
                   filter(UQ(seas_col) <= 2010),
                 `2011-2015` = exp_pts_df %>%
                   filter(UQ(seas_col) >= 2011 & UQ(seas_col) <= 2015),
                 `2016` = exp_pts_df %>%
                   filter(UQ(seas_col) == 2016)
  ))
}

enp_m1 <- function(exp_pts, approach, seas_name = "Season", yfog_name = "Yfog",
                   nsth_name = "Net_Score_to_Half",
                   ...){
  require(glue)
  require(tidyverse)
  require(purrr)
  
  form_app <- function(df){
    require(rlang)
    yfog_name = sym(yfog_name)
    nsth_name = sym(nsth_name)
    
    seas_range = range(df[[seas_name]])
    seas_range = glue(seas_range[1], "-", seas_range[2])
    enp_name = glue("enp_fm1", "_", seas_range)
    
    df %>%
      group_by(!!yfog_name) %>%
      summarise(!!enp_name := mean(!!nsth_name))
  }
  
  reg_app <- function(df, yfog_name = "Yfog", nsth_name = "Net_Score_to_Half",
                      seas_name = "Season"){
    require(rlang)
    require(stringr)
    require(glue)
    yfog_name <- sym(yfog_name)
    nsth_name <- sym(nsth_name)
    
    seas_range = range(df[[seas_name]])
    seas_range = glue(seas_range[1], "-", seas_range[2])
    enp_name = glue("enp_rm1-{seas_range}")
    
    mod_formula <- as.formula(glue("{nsth_name} ~ factor({yfog_name}) - 1"))
    model <- lm(mod_formula, data = df)
    
    yards <- as.numeric(str_extract(names(coef(model)), "[0-9]{1,2}"))
    tibble(Yfog = yards,
           !!enp_name := coef(model))
  }
  
  if (startsWith(approach, "f")){
    calc_enp_func <- form_app
  } else if (startsWith(approach, "r")){
    calc_enp_func <- reg_app
  } else {
    print("(f)ormula or (r)egression approach?")
    return()
  }
  
  if (class(exp_pts) == "list"){
    exp_pts %>%
      map(~ calc_enp_func(., ...))
  } else {
    calc_enp_func(exp_pts, ...)
  }
}

enp_m2 <- function(exp_pts, approach, seas_name = "Season", yfog_name = "Yfog",
                   nsth_name = "Net_Score_to_Half",
                   ...){
  require(glue)
  require(tidyverse)
  require(purrr)
  
  form_app <- function(df){
    (calc_form_m2(df))
  }
  
  reg_app <- function(df, yfog_name = "Yfog", nsth_name = "Net_Score_to_Half",
                      seas_name = "Season"){
    browser()
    require(rlang)
    require(stringr)
    require(glue)
    yfog_name <- sym(yfog_name)
    nsth_name <- sym(nsth_name)
    
    seas_range = range(df[[seas_name]])
    seas_range = glue(seas_range[1], "-", seas_range[2])
    enp_name = glue("enp_fm2-{seas_range}")
    
    mod_formula <- as.formula(glue("{nsth_name} ~ factor({yfog_name}) - 1"))
    model <- lm(mod_formula, data = df)
    
    yards <- as.numeric(str_extract(names(coef(model)), "[0-9]{1,2}"))
    tibble(Yfog = yards,
           !!enp_name := coef(model))
  }
  
  if (startsWith(approach, "f")){
    calc_enp_func <- form_app
  } else if (startsWith(approach, "r")){
    calc_enp_func <- reg_app
  } else {
    print("(f)ormula or (r)egression approach?")
    return()
  }
  
  if (class(exp_pts) == "list"){
    (exp_pts %>%
      map(~ calc_enp_func(., ...)))
  } else {
    (calc_enp_func(exp_pts, ...))
  }
}

make_csv_dat <- function(raw_exp_dat, method_num, approach, components = F,
                         write = T){
  require(tidyverse)
  require(magrittr)
  require(glue)
  method_func <- switch(method_num, enp_m1, enp_m2, enp_m3)
  csv_dat <- raw_exp_dat %>%
    season_splits() %>%
    method_func(., approach = approach) %>%
    reduce(merge)
  
  if (components){
    csv_dat <- csv_dat %>%
      select(matches("^comp|Yfog"))
    comp <- "_comp"
  } else {
    csv_dat <- csv_dat %>%
      select(matches("^enp|Yfog"))
    comp <- ""
  }
  
  
  if (write){
      write.csv(csv_dat, glue("enp_{approach}m{method_num}{comp}.csv"),
                row.names = F)
  }
  csv_dat
}

make_plot_dat <- function(raw_exp_dat, method_num, approach, yfog_name = "Yfog",
                          components = F,
                          ...){
  require(tidyverse)
  require(magrittr)
  require(glue)
  require(reshape2)
  method_func <- switch(method_num, enp_m1, enp_m2, enp_m3)
  
  plot_dat <- raw_exp_dat %>%
    season_splits() %>%
    method_func(., approach, ...) %>%
    reduce(merge)
  
  if (components){
    comp_only <- plot_dat %>%
      select(starts_with("comp"))
    cols_of_int <- names(comp_only)
    val_name <- glue("comp_{approach}m{method_num}")
  } else {
    enp_only <- plot_dat %>%
      select(starts_with("enp"))
    cols_of_int <- names(enp_only)
    val_name <- glue("enp_{approach}m{method_num}")
  }
  # browser()
  
  plot_dat %>%
    melt(id.var = yfog_name, variable.name = "split", measure.vars = cols_of_int,
         value.name = val_name)
}

#######################
###### Clustering #####
#######################
cl   <- function(dat,fm, cluster){
           require(sandwich, quietly = TRUE)
           require(lmtest, quietly = TRUE)
           M <- length(unique(cluster))
           N <- length(cluster)
           K <- fm$rank
           dfc <- (M/(M-1))*((N-1)/(N-K))
           uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
           vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
           coeftest(fm, vcovCL)
}

calc_precision <- function(data, cluster_var = character(0), method = "cl"){
  require(stringr)
  require(glue)
  model <- lm(Net_Score_to_Half ~ factor(Yfog) - 1, data = data)
  
  if (length(cluster_var) > 0){
    if (method == "cl"){
      se <- cl(data, model, data[[cluster_var]])[, 2]
    } else {
      se <- sqrt(diag(cluster.vcov(model, data[[cluster_var]])))  
    }
  } else {
    se <- summary(model)$coefficients[, 2]
    # if (length(se) == 96){
    #   browser()
    # }
    cluster_var <- "None"
  }
  
  yards <- as.numeric(str_extract(names(se), "[0-9]{1,2}"))
  min_date <- min(data$Season)
  max_date <- max(data$Season)
  year_name <- if (min_date == max_date){
    glue("{max_date}")
  } else {
    glue("{min_date} - {max_date}")
  }
  tibble(Year = rep(year_name, length(yards)),
         Yfog = yards,
         Cluster_var = rep(cluster_var, length(yards)),
         SE = se)
}