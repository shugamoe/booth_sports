# R file for Method 2 Regression Approach
make_off_def_key <- function(side, key_type){
  require(glue)
  
  make_key <- function(number = NULL, rt_num = NULL){
    
    if (key_type == "fdown"){
      key <- as_tibble(cbind(diag(number, 99, 99), rep(0, 99))) %>%
                  set_names(paste0(rep(glue("{side}_"), 100), 1:100)) %>%
                  bind_cols(yfog = 99: 1, Reset_Team_to_Score = rep(rt_num, 99), .)
    } else if (key_type == "koff"){
      key <- set_names(
        c(0, rt_num, rep(0, 99), number),
        c("yfog", "Reset_Team_to_Score", paste0(rep(glue("{side}_"), 100), 1:100))
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
  }
}

make_who_key <- function(){
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
  
  #TODO(jcm) Order by Yfog just for easier visual inspection
  bind_rows(who_fdown, who_koff)
}

make_keys <- function(){
  require(tidyverse)
  D_key = as_tibble(
  diag(1, 100, 100)
  ) %>%
    set_names(paste0(rep("D_", 100), 1:100)) %>%
    bind_cols(yfog = 99:0, .)

  OFF_fd_key = as_tibble(
    cbind(diag(1, 99, 99), rep(0, 99))) %>%
    set_names(paste0(rep("OFF_", 100), 1:100)) %>%
    bind_cols(yfog = 99:1, Reset_Team_to_Score = rep(1, 99), .)
  
  OFF_koff_key = set_names(
    c(0, 1, rep(0, 99), 1),
    c("yfog", "Reset_Team_to_Score", paste0(rep("OFF_", 100), 1:100)))
    
  DEF_fd_key = as_tibble(
    cbind(diag(-1, 99, 99), rep(0, 99))) %>%
    set_names(paste0(rep("DEF_", 100), 1:100)) %>%
    bind_cols(yfog = 99:1, Reset_Team_to_Score = rep(1, 99), .)
  
  DEF_koff_key = set_names(
    c(0, 1, rep(0, 99), -1),
    c("yfog", "Reset_Team_to_Score", paste0(rep("DEF_", 100), 1:100)))
    
  WHO_fd_key = row_bind(OFF_fd_key + DEF_fd_key - 
                          cbind(matrix(99:1), rep(1, 99), diag(0, 99, 99)),
                        OFF_fd_key + DEF_fd_key - 
                          cbind(matrix(99:1), rep(2, 99), diag(0, 99, 99)))
  browser()
}


create_req_df <- function(source_dir = ""){
  require(glue)
  require(tidyverse)
  FDOWN_EXP <- read.csv(glue(
    "{source_dir}expected_points/correc_raw_fdowns_nscore_half_and_reset.csv")) %>%
    mutate(exp_type = "first_down") 
  
  KOFF_EXP <- read.csv(
    glue("{source_dir}expected_points/corec_raw_koff_nscore_half_and_reset.csv")) %>%
    mutate(exp_type = "kickoff")
  
  bind_rows(FDOWN_EXP, KOFF_EXP) %>%
    select(-matches("RUSH|PUNT|FGXP|CONV|PUNT|NOPL|KOFF|PASS|FGXP|ONSD|Armchair_dsq|Drive_num|Drive_start"),
           -X) %>%
    mutate(Yfog = ifelse(is.na(Yfog), 0, Yfog))
}
