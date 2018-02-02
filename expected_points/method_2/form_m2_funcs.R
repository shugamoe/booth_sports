# Formula Approach Method 2

YFOG_DEFAULT <- tibble(Yfog = 1:99, other = NA)

# extracts yrdline stat from the tidy df
eys <- function(tidy_df){
  require(dplyr)
  non_yfog_col <- names(tidy_df)[2]
  
  # if (dim(tidy_df)[1] < 99){
  #   browser()
  # }
  
  fill_in_df <- YFOG_DEFAULT %>%
    rename(!!non_yfog_col := other)
  
  # If the df doesn't have Yfog 1-99 fill in the value with NA
  tidy_df <- tidy_df %>%
    merge(fill_in_df, all.y = T, by = "Yfog") %>%
    transmute(Yfog = Yfog,
              !!non_yfog_col := UQ(sym(glue("{non_yfog_col}.x"))))
  
  # Kinda redudant here making sure Yfog is in the transmute and pulling it out 
  # later but hey.
  pull(tidy_df, -Yfog) 
}

# For Expected Net Points until End of Half Using V_K

# For Expected Net Points until End of Half using V_k recursive
calc_form_m2 <- function(exp_pts_df, vk_rec_only = F, rsts_name = "Reset_Team_to_Score",
                        nstr_name = "Net_Score_to_Reset",
                        fk_name = "Follow_Kickoff", seas_name = "Season"
){
  max_yard <- max(exp_pts_df$Yfog)
  require(rlang)
  require(magrittr)
  # Strings to symbols
  rsts_name = sym(rsts_name)
  fk_name = sym(fk_name)
  
  P_o <- exp_pts_df %>% 
    mutate(offense_score = ifelse(UQ(rsts_name) == 1, 1, 0)) %>% 
    group_by(Yfog) %>%
    summarise(P_o = mean(offense_score))
  
  P_d <- exp_pts_df %>% 
    mutate(defense_score = ifelse(UQ(rsts_name) == - 1, 1, 0)) %>% 
    group_by(Yfog) %>%
    summarise(P_d = mean(defense_score))
  
  P_none <- tibble(Yfog = 1:max_yard, P_none = 1 - eys(P_o) - eys(P_d))
  
  enp_o_oscore_yd <- exp_pts_df %>%
    filter(UQ(rsts_name) == 1) %>%
    group_by(Yfog) %>%
    summarise(enp_o_oscore_yd = mean(UQ(sym(nstr_name))))
  
  enp_o_dscore_yd <- exp_pts_df %>%
    filter(UQ(rsts_name) == -1) %>%
    group_by(Yfog) %>%
    summarise(enp_o_dscore_yd = mean(UQ(sym(nstr_name))))
  
  enp_o_noscore_yd <- exp_pts_df %>%
    filter(UQ(rsts_name) == 0) %>%
    group_by(Yfog) %>%
    summarise(enp_o_noscore_yd = mean(UQ(sym(nstr_name))))
    
  koff_df <- exp_pts_df %>%
    filter(Yfog == 0)
  
  ko_P_o <- koff_df %>%
    mutate(offense_score = ifelse(UQ(rsts_name) == 1, 1, 0)) %$%
    mean(offense_score)
  
  ko_P_d <- koff_df %>% 
    mutate(defense_score = ifelse(UQ(rsts_name) == -1, 1, 0)) %$%
    mean(defense_score)
  
  ko_P_none <- koff_df %>% 
    mutate(defense_score = ifelse(UQ(rsts_name) == 0, 1, 0)) %$%
    mean(defense_score)
  
  ko_enp_o_oscore_yd <- koff_df %>%
    filter(UQ(rsts_name) == 1)
  ko_enp_o_oscore_yd <- mean(ko_enp_o_oscore_yd[[nstr_name]])
  
  
  ko_enp_o_dscore_yd <- koff_df %>%
    filter(UQ(rsts_name) == -1)
  ko_enp_o_dscore_yd <- mean(ko_enp_o_dscore_yd[[nstr_name]])
                             
  ko_enp_o_noscore_yd <- koff_df %>%
    filter(UQ(rsts_name) == 0)
  
  ko_enp_o_noscore_yd <- mean(ko_enp_o_noscore_yd[[nstr_name]])
  
  (V_k_rec <- (ko_P_o * ko_enp_o_oscore_yd +
              ko_P_d * (ko_enp_o_dscore_yd) +
              (1 - ko_P_o - ko_P_d) * ko_enp_o_noscore_yd) /
              (1 + ko_P_o - ko_P_d))
  
  # Expected Net points naming
  seas_range = range(koff_df[[seas_name]])
  seas_range = glue(seas_range[1], "-", seas_range[2])
  enp_name = glue("enp_comp_{seas_range}")
  
  # Component naming
  po_name <- glue("comp_P_o_{seas_range}")
  pd_name <- glue("comp_P_d_{seas_range}")
  pn_name <- glue("comp_P_n_{seas_range}")
  enpo_ds_name <- glue("comp_enp_d_{seas_range}")
  enpo_os_name <- glue("comp_enp_o_{seas_range}")
  enpo_ns_name <- glue("comp_enp_n_{seas_range}")
  
  ko_po_name <- glue("comp_ko_P_o_{seas_range}")
  ko_pd_name <- glue("comp_ko_P_d_{seas_range}")
  ko_pn_name <- glue("comp_ko_P_n_{seas_range}")
  ko_enpo_ds_name <- glue("comp_ko_enp_o_dscore_yd_{seas_range}")
  ko_enpo_os_name <- glue("comp_ko_enp_o_oscore_yd_{seas_range}")
  ko_enpo_ns_name <- glue("comp_ko_enp_o_nscore_yd_{seas_range}")
  ko_V_k_m2 <- glue("enp_comp_ko_V_100_{seas_range}")
  
  enp_eoh_vk_rec <- tibble(Yfog = 1:max_yard,
                        !!enp_name := eys(P_o) * (eys(enp_o_oscore_yd) - 
                         V_k_rec) -  eys(P_d) * (-eys(enp_o_dscore_yd) - 
                         V_k_rec) + (1 - eys(P_o) - eys(P_d)) * 
                         (eys(enp_o_noscore_yd)),
                        !!po_name := eys(P_o),
                        !!pd_name := eys(P_d),
                        !!pn_name := eys(P_none),
                        !!enpo_os_name := eys(enp_o_oscore_yd),
                        !!enpo_ds_name := eys(enp_o_dscore_yd),
                        !!enpo_ns_name := eys(enp_o_noscore_yd),
                        !!ko_V_k_m2  := V_k_rec,
                        !!ko_po_name := ko_P_o,
                        !!ko_pd_name := ko_P_d,
                        !!ko_pn_name := ko_P_none,
                        !!ko_enpo_os_name := ko_enp_o_oscore_yd,
                        !!ko_enpo_ds_name := ko_enp_o_dscore_yd,
                        !!ko_enpo_ns_name := ko_enp_o_noscore_yd
                       )
  
  # print("calc_form_m2 returning!")
  if (vk_rec_only){
    V_k_rec
  } else {
    enp_eoh_vk_rec
  }
}