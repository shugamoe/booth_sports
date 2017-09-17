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
  
  se_sub_WHO <- sqrt(D_vars + who_var - 2 * wd_covars)
  se_add_WHO <- sqrt(D_vars + who_var + 2 * wd_covars)

  seas_range = range(data$Season)
  seas_range = glue(seas_range[1], "-", seas_range[2])
  
  rv_tibble_all <- tibble(
    #TODO(jcm) Looks like these glues have to be strings beforehand?
    Yfog = 1:100,
    !!glue("coef_{reg_type}_{clus_type}_{seas_range}") := coefficients(fm),
    !!glue("se_coef_{reg_type}_{clus_type}_{seas_range}") := se,
    !!glue("nobs_{reg_type}_{clus_type}_{seas_range}") := nobs$n)
  
  rv_tibble_99 <- tibble(
    !!glue("nobs_{reg_type}_{clus_type}_{seas_range}") := nobs$n[-100],
    !!glue("coef_sub_WHO_{reg_type}_{clus_type}_{seas_range}") := coefficients(fm)[-100] - 
      coefficients(fm)[100],
    !!glue("se_coef_sub_WHO_{reg_type}_{clus_type}_{seas_range}") := se_sub_WHO,
    !!glue("coef_add_WHO_{reg_type}_{clus_type}_{seas_range}") := coefficients(fm)[-100] + 
      coefficients(fm)[100],
    !!glue("se_coef_add_WHO_{reg_type}_{clus_type}_{seas_range}") := se_add_WHO,
    !!glue("covars_{reg_type}_{clus_type}_{seas_range}") := wd_covars,
    !!glue("yfog_vars_{reg_type}_{clus_type}_{seas_range}") := D_vars,
    !!glue("WHO_var_{reg_type}_{clus_type}_{seas_range}") := who_var
  )
  
  # write.csv(rv_tibble_all, glue("{source_dir}m2_all_{reg_type}_{clus_type}.csv"),
  #           row.names = F)
  # write.csv(rv_tibble_99, glue("{source_dir}m2_99_{reg_type}_{clus_type}.csv"),
  #           row.names = F)
  
  if (all_or_nn == "all"){
    rv_tibble_all
  } else {
    rv_tibble_99
  }
}

main <- function(){
  require(purrr)
  inp_list <- list(iv = c(T, F, T, F),
       clust = c(T, T, F, F),
       cluster_var = rep("Half_num", 4),
       source_dir = rep("expected_points/method_2/", 4),
       year_start = c(2000, 2000, 2011, 2015),
       year_stop = c(2016, 2011, 2015, 2016),
       all_or_nn = rep("all", 4))
  
  all <- inp_list %>% 
    pmap(run_m2_reg) %>%
    reduce(merge)
  
  
  inp_list[["all_or_nn"]] = rep("", 4)
  nn <- inp_list %>%
    pmap(run_m2_reg) %>%
    reduce(cbind) %>%
    mutate(Yfog = 1:99) %>%
    select(Yfog, everything())
  
  write.csv(all, "expected_points/method_2/m2_all.csv", row.names = F)
  write.csv(nn, "expected_points/method_2/m2_99.csv", row.names = F)
}

comp_vals <- function(){
  m2_form <- read.csv("expected_points/method_2/enp_fm2.csv")
  m2_reg <- read.csv("expected_points/method_2/m2_all_iv_clust.csv")
  
  View(tibble(form = m2_form$enp_fm2_comp_2000.2016,
              reg = m2_reg$coef_iv_clust_2000.2016))
}
