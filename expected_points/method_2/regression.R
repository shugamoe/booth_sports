# R file for Method 2 Regression Approach
make_off_def_key <- function(side, key_type){
  require(glue)
  
  make_key <- function(number = NULL, rt_num = NULL){
    
    if (key_type == "fdown"){
      key <- as_tibble(cbind(diag(number, 99, 99), rep(0, 99))) %>%
                  set_names(paste0(rep(glue("{side}_"), 100), 1:100)) %>%
                  bind_cols(Yfog = 1:99, Reset_Team_to_Score = rep(rt_num, 99), .)
    } else if (key_type == "koff"){
      key <- set_names(
        c(0, rt_num, rep(0, 99), number),
        c("Yfog", "Reset_Team_to_Score", paste0(rep(glue("{side}_"), 100), 1:100))
        )
    }
  }
  rt_order <- c(-1, 0, 1)
  
  if (side == "OFF"){
    num_order <- c(0, 0, 1)
  } else if (side == "DEF"){
    num_order <- c(-1, 0, 0)
  }
  
  if (key_type == "fdown"){
    key <- pmap_df(list(number = num_order, rt_num = rt_order), make_key)
  } else if (key_type == "koff"){
    key <- pmap(list(number = num_order, rt_num = rt_order), make_key) %>%
      reduce(bind_rows)
  } else if (key_type == "all"){
    bind_rows(make_off_def_key(side, "fdown"),
              make_off_def_key(side, "koff"))
  }
}

make_who_key <- function(){
  require(tidyverse)
  fdown_base <- diag(0, 297, 100)
  koff_base <- cbind(diag(0, 3, 99), rep(1, 3))
  
  who_fdown <- (fdown_base + make_off_def_key("OFF", "fdown")[, -c(1, 2)] +
    make_off_def_key("DEF", "fdown")[, -c(1, 2)]) %>%
    set_names(paste0("WHO_", 1:100)) %>%
    bind_cols(Yfog = rep(1:99, 3), Reset_Team_to_Score = c(rep(-1, 99), 
                                                           rep(0, 99),
                                                           rep(1, 99)), .)
  
  who_koff <- (koff_base + make_off_def_key("OFF", "koff")[, -c(1, 2)] + 
    make_off_def_key("DEF", "koff")[, -c(1, 2)]) %>%
    set_names(paste0("WHO_", 1:100)) %>%
    bind_cols(Yfog = rep(0, 3), Reset_Team_to_Score = c(-1, 0, 1), .)
  
  bind_rows(who_fdown, who_koff)
}

make_master_key <- function(){
  require(tidyverse)
  
  D_key = as_tibble(rbind(diag(1, 100, 100), 
            diag(1, 100, 100),
            diag(1, 100, 100)
            )) %>%
    set_names(paste0(rep("D_", 100), 1:100)) %>%
    bind_cols(Yfog = rep(c(1:99, 0), 3), Reset_Team_to_Score = c(rep(-1, 100), 
                                                           rep(0, 100), 
                                                           rep(1, 100)), .)
  
  WHO_key <- make_who_key()
  WHO_key$WHO_t <- rowSums(WHO_key[-c(1, 2)]) # Don't add yfog and Reset Team
  WHO_key <- WHO_key %>% select(Yfog, Reset_Team_to_Score, WHO_t)
  
  D_key %>%
    merge(WHO_key) %>%
    arrange(Yfog)
}


make_req_df <- function(year_start = 2000, year_stop = 2016, source_dir = ""){
  require(glue)
  require(tidyverse)
  FDOWN_EXP <- read.csv(glue(
    "{source_dir}expected_points/correc_raw_fdowns_nscore_half_and_reset.csv")) %>%
    mutate(exp_type = "first_down") %>%
    filter(Season >= year_start, Season <= year_stop)
  
  KOFF_EXP <- read.csv(
    glue("{source_dir}expected_points/corec_raw_koff_nscore_half_and_reset.csv")) %>%
    mutate(exp_type = "kickoff") %>%
    filter(Season >= year_start, Season <= year_stop)
  
  rv_df <- bind_rows(FDOWN_EXP, KOFF_EXP) %>%
    select(-matches("RUSH|PUNT|FGXP|CONV|PUNT|NOPL|KOFF|PASS|FGXP|ONSD|Armchair_dsq|Drive_num|Drive_start"),
           -X) %>%
    mutate(Yfog = ifelse(is.na(Yfog), 0, Yfog)) %>%
    merge(make_master_key())
}

run_m2_reg <- function(iv = T, clust = T, cluster_var = "Half_num", 
                       year_start, year_stop,
                       source_dir = "", all_or_nn){
  require(stringr)
  require(tidyverse)
  require(glue)
  library(AER)
  require(multiwayvcov)

  dep_var <- "Net_Score_to_Reset"
  ind_vars <- str_c(paste0(c(rep("D_", 99), "WHO_t"), c(1:99, "")),
                    collapse = " + ")
  instruments <- str_c("D_", 1:100, collapse = " + ")
  
  data = make_req_df(year_start, year_stop)
  if (iv){
    m2_form <- as.formula(glue("{dep_var} ~ {ind_vars} - 1"))
    fm <- ivreg(m2_form, 
                instruments = as.formula(glue("~ {instruments}")),
                data = data)
  } else {
    m2_form <- as.formula(glue("{dep_var} ~ {ind_vars} - 1"))
    fm <- lm(m2_form, data = data)
  }
  
  nobs <- data %>% 
    group_by(Yfog) %>%
    summarise(n = n()) %>%
    mutate(Yfog = ifelse(Yfog == 0, 100, Yfog)) %>%
    arrange(Yfog)
    
  if (clust){
    vc_mat <- cluster.vcov(fm, data[[cluster_var]])
    se <- sqrt(diag(vc_mat))  
  } else {
    se <- summary(fm)$coefficients[, 2]
    vc_mat <- vcov(fm)
  }
  
  reg_type <- if (iv){
    "iv"
  } else {
    "ols"
  }
  clus_type <- if (clust){
    "clust" 
  } else {
    "no_clust"
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
  
  # If cluster_var is half num we want to avoid column name redundancy (see
  # below)
  if (cluster_var == "Half_num"){
    cluster_var <-  "" 
  } else {
    cluster_var <- paste(cluster_var, "_")
  }
  
  rv_tibble_all <- tibble(
    #TODO(jcm) Looks like these glues have to be strings beforehand?
    Yfog = 1:100,
    !!glue("v_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := coefficients(fm),
    !!glue("se_v_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := se,
    !!glue("nobs_{seas_range}") := nobs$n)
  
  rv_tibble_99 <- tibble(
    !!glue("nobs_{seas_range}") := nobs$n[-100],
    !!glue("v99_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := coefficients(fm)[-100],
    !!glue("v100_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := coefficients(fm)[100],
    !!glue("v_minus_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := coefficients(fm)[-100] - 
      coefficients(fm)[100],
    !!glue("se_v_minus_v100_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := se_sub_WHO,
    !!glue("v_plus_v100_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := coefficients(fm)[-100] + 
      coefficients(fm)[100],
    !!glue("se_v_plus_v100_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := se_add_WHO,
    !!glue("D_99_vars_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := D_vars,
    !!glue("D_100_var_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := who_var,
    !!glue("covars_{reg_type}_{clus_type}_{cluster_var}{seas_range}") := wd_covars
  )
  
  rv_vars <- tibble(
    !!glue("resid_who_covar_{seas_range}") := resid_who_cov,
    !!glue("who_var_{seas_range}") := who_var,
    !!glue("resid_var_{seas_range}") := resid_var
  )
  
  # write.csv(rv_tibble_all, glue("{source_dir}m2_all_{reg_type}_{clus_type}_{cluster_var}.csv"),
  #           row.names = F)
  # write.csv(rv_tibble_99, glue("{source_dir}m2_99_{reg_type}_{clus_type}_{cluster_var}.csv"),
  #           row.names = F)
  
  if (all_or_nn == "all"){
    rv_tibble_all
  } else if (all_or_nn == "nn") {
    rv_tibble_99
  } else if (all_or_nn == "var"){
    rv_vars
  }
}

main_a <- function(){
  require(purrr)
  # Do wrong things only for full sample
  # m2_99 only needs IV clust, no wrong stuff
  
  inp_list_all <- list(iv = c(T, T, T, T, T, F, F),
       clust = c(T, T, T, T, F, T, F),
       cluster_var = "Half_num",
       source_dir = "expected_points/method_2/",
       year_start = c(2000, 2000, 2011, 2016, 2000, 2000, 2000),
       year_stop = c(2016, 2010, 2015, 2016, 2016, 2016, 2016),
       all_or_nn = "all"
  )
       
  inp_list_nn <- list(iv = c(T, T, T, T),
       clust = c(T, T, T, T),
       cluster_var = "Half_num",
       source_dir = "expected_points/method_2/",
       year_start = c(2000, 2000, 2011, 2016),
       year_stop = c(2016, 2010, 2015, 2016),
       all_or_nn = "nn"
   ) 
  
  inp_list_var <- list(iv = c(T, T, T, T),
     clust = c(T, T, T, T),
     cluster_var = "Half_num",
     source_dir = "expected_points/method_2/",
     year_start = c(2000, 2000, 2011, 2016),
     year_stop = c(2016, 2010, 2015, 2016),
     all_or_nn = "var"
  )
  
  var <- inp_list_var %>%
    pmap(run_m2_reg) %>%
    reduce(merge)
  
  all <- inp_list_all %>% 
    pmap(run_m2_reg) %>%
    reduce(merge) %>%
    arrange(Yfog) %>%
    select(Yfog,
           `nobs_2000-2016`,
           `v_iv_clust_2000-2016`,
           `se_v_iv_clust_2000-2016`,

           `nobs_2000-2010`,
           `v_iv_clust_2000-2010`,
           `se_v_iv_clust_2000-2010`,
           
           `nobs_2011-2015`,
           `v_iv_clust_2011-2015`,
           `se_v_iv_clust_2011-2015`,

           `nobs_2016-2016`,
           `v_iv_clust_2016-2016`,
           `se_v_iv_clust_2016-2016`,
           
           `v_ols_clust_2000-2016`,
           `se_v_ols_clust_2000-2016`,
           
           `v_ols_no_clust_2000-2016`,
           `se_v_ols_no_clust_2000-2016`,
           
           `v_iv_no_clust_2000-2016`,
           `se_v_iv_no_clust_2000-2016`
           )
  
  nn <- inp_list_nn %>%
    pmap(run_m2_reg) %>%
    reduce(cbind) %>%
    mutate(Yfog = 1:99) %>%
    select(Yfog, everything())
  
  write.csv(all, "expected_points/method_2/M2_regression_all.csv", row.names = F)
  write.csv(nn, "expected_points/method_2/M2_regression_99.csv", row.names = F)
  write.csv(var, "expected_points/method_2/M2_regression_var.csv", row.names = F)
}

main_b <- function(){
  require(glue)
  source("expected_points/method_2/form_m2_funcs.R")
  rdf <- make_req_df()
  V_k_rec <- calc_form_m2(rdf, T)
  
  dep_var <- glue("Net_Score_to_Reset - {V_k_rec} * WHO_t")
  indep_vars <- str_c("D_", 1:99, collapse = " + ")
  
  form <- as.formula(glue("{dep_var} ~ {indep_vars} - 1"))
  fm <- lm(form, data = rdf)
  se <- summary(fm)$coefficients[, 2]
  
  nobs <- rdf %>% 
    group_by(Yfog) %>%
    summarise(n = n()) %>%
    mutate(Yfog = ifelse(Yfog == 0, 100, Yfog)) %>%
    arrange(Yfog)
  
  seas_range = range(rdf$Season)
  seas_range = glue(seas_range[1], "-", seas_range[2])
  
  verify_tibble <- tibble(
    Yfog = 1:99,
    !!glue("v_verify_{seas_range}") := coefficients(fm),
    !!glue("se_v_verify_{seas_range}") := se,
    !!glue("nobs_{seas_range}") := nobs$n[-100])
  
  write.csv(verify_tibble, "expected_points/method_2/M2_regression_verify.csv", row.names = F)
}

m2_regression_rmd <- function(){
  g3_inp_list <- list(iv = c(T, T, T),
         clust = c(T, T, F),
         cluster_var = c("Armchair_gid", "Half_num", "Armchair_pid"),
         source_dir = "expected_points/method_2/",
         year_start = c(2000, 2000, 2000),
         year_stop = c(2016, 2016, 2016),
         all_or_nn = "all"
     ) 
  
  g3_se <- g3_inp_list %>%
      pmap(run_m2_reg) %>%
      reduce(cbind)
  write.csv(g3_se, "expected_points/method_2/M2_g3_se_99.csv", row.names = F)
  # g4_inp_list <- list(iv = c(T, T, T),
  #        clust = c(T, T, T),
  #        cluster_var = c("Armchair_gid", "Half_num", "Armchair_pid"),
  #        source_dir = "expected_points/method_2/",
  #        year_start = c(2000, 2000, 2000),
  #        year_stop = c(2016, 2016, 2016),
  #        all_or_nn = "nn"
  #    ) 
  # g3_se <- g3_inp_list %>%
  #     pmap(run_m2_reg) %>%
  #     reduce(cbind) %>%
  #     mutate(Yfog = 1:99) %>%
  #     select(Yfog, everything())
  # write.csv(nn, "expected_points/method_2/M2_g3_se_99.csv", row.names = F)
}

# comp_vals <- function(){
#   m2_form <- read.csv("expected_points/method_2/enp_fm2.csv")
#   m2_reg <- read.csv("expected_points/method_2/m2_all_iv_clust.csv")
#   
#   View(tibble(form = m2_form$enp_fm2_comp_2000.2016,
#               reg = m2_reg$coef_iv_clust_2000.2016)
#        )
# }
