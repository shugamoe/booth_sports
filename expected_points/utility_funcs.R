# Utility Functions for Working with Expected Points Data

library(tidyverse)
library(purrr)

EXP_DAT <- read.csv("../correc_raw_fdowns_nscore_half_and_reset.csv")
EXP_DAT_WKO <- read.csv("../corec_raw_koff_nscore_half_and_reset.csv") %>%
  mutate(Yfog = 0) %>%
  bind_rows(EXP_DAT)

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
                   nsth_name = "Net_Score_to_Half", source_dir = "../",
                   model_only = F,
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
                      seas_name = "Season", source_dir = ""){
    require(rlang)
    require(stringr)
    require(glue)
    print("Hey, using this regression from John's new notes!")
    dep_var <- "Net_Score_to_Half"
    
    if (!(100 %in% unique(df$Yfog))){
      indep_vars <- str_c("D_", 1:99, collapse = " + ")
    } else if (all(0 == unique(df$Yfog))) {
      indep_vars <- "D_100"
    } else {
      indep_vars <- str_c("D_", 1:100, collapse = " + ")
    }
    form <- as.formula(glue("{dep_var} ~ {indep_vars} - 1"))
    
    fm <- lm(form, data = df)
    
    yards <- as.numeric(str_extract(names(coef(fm)), "[0-9]{1,3}"))
    seas_range = range(df$Season)
    seas_range = glue(seas_range[1], "-", seas_range[2])
    enp_name = glue("enp_rm1-{seas_range}")
    
    if (model_only){
      fm
    } else {
      tibble(Yfog = yards,
             !!enp_name := coef(fm))
    }
    
    # yfog_name <- sym(yfog_name)
    # nsth_name <- sym(nsth_name)
    # 
    # seas_range = range(df[[seas_name]])
    # seas_range = glue(seas_range[1], "-", seas_range[2])
    # enp_name = glue("enp_rm1-{seas_range}")
    # 
    # mod_formula <- as.formula(glue("{nsth_name} ~ factor({yfog_name}) - 1"))
    # model <- lm(mod_formula, data = df)
    # 
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

make_m1_99 <- function(exp_pts, clust, year_start, year_end,
                       cluster_var = "Half_num"){
  
  exp_pts <- exp_pts %>%
    filter(Yfog >= year_start,
           Yfog <= year_end)
  
  fm <- enp_m1(exp_pts, "r", model_only = T)
  
  if (clust){
    vc_mat <- cluster.vcov(fm, data[[cluster_var]])
    se <- sqrt(diag(vc_mat))  
  } else {
    se <- summary(fm)$coefficients[, 2]
    vc_mat <- vcov(fm)
  }
  
  # Calculate SE for V_1, ..., +- V_100
  who_var <- diag(vc_mat)[100]
  D_vars <- diag(vc_mat[-100, -100])
  wd_covars <- vc_mat[100, -100]
  
  resid_who_cov <- cov(data$WHO_t, resid(fm))
  who_var <- var(data$WHO_t)
  resid_var <- var(resid(fm))
  
  se_sub_WHO <- sqrt(D_vars + who_var - 2 * wd_covars)
  se_add_WHO <- sqrt(D_vars + who_var + 2 * wd_covars)

  seas_range = range(data$Season)
  seas_range = glue(seas_range[1], "-", seas_range[2])
  
  rv_tibble_99 <- tibble(
    !!glue("nobs_{seas_range}") := nobs$n[-100],
    !!glue("v99_{reg_type}_{clus_type}_{seas_range}") := coefficients(fm)[-100],
    !!glue("v100_{reg_type}_{clus_type}_{seas_range}") := coefficients(fm)[100],
    !!glue("v_minus_{reg_type}_{clus_type}_{seas_range}") := coefficients(fm)[-100] - 
      coefficients(fm)[100],
    !!glue("se_v_minus_v100_{reg_type}_{clus_type}_{seas_range}") := se_sub_WHO,
    !!glue("v_plus_v100_{reg_type}_{clus_type}_{seas_range}") := coefficients(fm)[-100] + 
      coefficients(fm)[100],
    !!glue("se_v_plus_v100_{reg_type}_{clus_type}_{seas_range}") := se_add_WHO,
    !!glue("covars_{reg_type}_{clus_type}_{seas_range}") := wd_covars,
    !!glue("yfog_vars_{reg_type}_{clus_type}_{seas_range}") := D_vars,
    !!glue("WHO_var_{reg_type}_{clus_type}_{seas_range}") := who_var
  )
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
                         write = T, name = NULL){
  require(tidyverse)
  require(magrittr)
  require(glue)
  require(stringr)
  
  method_func <- switch(method_num, enp_m1, enp_m2, enp_m3)
  csv_dat <- raw_exp_dat %>%
    season_splits() %>%
    method_func(., approach = approach) %>%
    reduce(merge)
  
  if (components){
    csv_dat <- csv_dat %>%
      select(matches("comp|Yfog"))
    names(csv_dat) <- str_replace(names(csv_dat), "_comp_", "")
    names(csv_dat) <- str_replace(names(csv_dat), "comp_", "")
    names(csv_dat) <- str_replace(names(csv_dat), "_comp", "")
                                  
    comp <- "_comp"
  } else {
    csv_dat <- csv_dat %>%
      select(matches("^enp|Yfog"))
    comp <- ""
  }
  
  if (method_num == 2 && approach == "f" && !components){
    new_csv_dat <- tibble(Yfog = 1:100,
                         `enp_comp_2000-2016` = 
                           c(csv_dat$`enp_fm2_comp_2000-2016`, mean(csv_dat$`enp_comp_ko_V_k_rec_2000-2016`)),
                         `enp_comp_2000-2010` = 
                           c(csv_dat$`enp_fm2_comp_2000-2010`, mean(csv_dat$`enp_comp_ko_V_k_rec_2000-2010`)),
                         `enp_comp_2011-2015` = 
                           c(csv_dat$`enp_fm2_comp_2011-2015`, mean(csv_dat$`enp_comp_ko_V_k_rec_2011-2015`)),
                         `enp_comp_2016-2016` = 
                           c(csv_dat$`enp_fm2_comp_2016-2016`, mean(csv_dat$`enp_comp_ko_V_k_rec_2016-2016`))
    )
    csv_dat <- new_csv_dat
    names(csv_dat) <- str_replace(names(csv_dat), "_comp", "")
  }
  
  if (approach == "f"){
    approach <-  "formula"
  } else if (approach == "r"){
    approach <-  "regression"
  }
  
  if (write){
    if (!is.null(name)){
      write.csv(csv_dat, glue("M{method_num}_{name}.csv"),
                row.names = F)
    } else {
      write.csv(csv_dat, glue("M{method_num}_{approach}{comp}.csv"),
                row.names = F)
    }
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
      select(matches("^enp_[r|f]"))
    cols_of_int <- names(enp_only)
    val_name <- glue("enp_{approach}m{method_num}")
  }
  # browser()
  
  plot_dat %>%
    melt(id.var = yfog_name, variable.name = "split", measure.vars = cols_of_int,
         value.name = val_name)
}

csv_to_plot_dat <- function(df){
  
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

calc_precision <- function(data, cluster_var = character(0)){
  require(stringr)
  require(glue)
  require(multiwayvcov)
  
  dep_var <- "Net_Score_to_Half"
  
  if (!(100 %in% unique(data$Yfog))){
    indep_vars <- str_c("D_", 1:99, collapse = " + ")
  } else if (all(100 == unique(data$Yfog))) {
    indep_vars <- "D_100"
  } else {
    indep_vars <- str_c("D_", 1:100, collapse = " + ")
  }
  form <- as.formula(glue("{dep_var} ~ {indep_vars} - 1"))
  
  model <- lm(form, data = data)
  
  if (length(cluster_var) > 0){
    se <- sqrt(diag(cluster.vcov(model, data[[cluster_var]])))  
  } else {
    se <- summary(model)$coefficients[, 2]
    # if (length(se) == 96){
    #   browser()
    # }
    cluster_var <- "None"
  }
  
  yards <- as.numeric(str_extract(names(se), "[0-9]{1,3}"))
  min_date <- min(data$Season)
  max_date <- max(data$Season)
  year_name <- if (min_date == max_date){
    glue("{max_date}")
  } else {
    glue("{min_date} - {max_date}")
  }
  rt <- tibble(Year = rep(year_name, length(yards)),
         Yfog = yards,
         Cluster_var = rep(cluster_var, length(yards)),
         SE = se)
  print(dim(rt))
  rt
}
