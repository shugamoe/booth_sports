# R file to create model for expected number of points for next score
library(purrrlyr)
library(dplyr)
library(readr)
library(MASS)
library(purrr)
library(magrittr)

source("expected_points/testing.R")

calc_min_in_half <- function(play_row){
  qtr <- play_row$qtr
  min <- play_row$min
  sec <- play_row$sec
  if (qtr %in% c(2, 4)){
    return(min + sec / 60)
  } else if (qtr %in% c(1, 3)){
    return(15 + min + sec / 60)
  } else {
    return(-1) # Not using overtime
  }
}

create_drive_num_key_single <- function(fpid, fpid_lead, uid){
  tibble(drive_num = rep(uid, fpid_lead - fpid),
         pid = fpid:(fpid_lead - 1))
}


GAMES_DF <- read_csv('nfl_00_16_corrected/GAME.csv') %>%
  dplyr::select(gid, h, seas, wk, ptsh, ptsv)
PLAYS_DF <- read_csv('nfl_00_16_corrected/PLAY.csv') %>%
  filter(qtr %in% c(1, 2, 3, 4)) %>%
  mutate(min_in_half = ifelse(qtr %in% c(2, 4), min + sec / 60, # Not using overtime
                             ifelse(qtr %in% c(1,3),     
                                    15 + min + sec / 60, NA)),
         min_in_game = ifelse(qtr %in% c(3,4), min_in_half, 
                             ifelse(qtr %in% c(1, 2), 30 + min_in_half, -1)),
         gid_lead = lead(gid, default = -1),
         
         half = ifelse(qtr %in% c(1, 2), 1, 2),
         half_lead = lead(half, default = 1), 
         
         min_in_game_lead = ifelse(gid == gid_lead, lead(min_in_game), 0),
         min_in_half_lead = ifelse(half == half_lead, lead(min_in_half), 0),
         len_clock = ifelse(gid == gid_lead, (min_in_game - min_in_game_lead) * 60, 60 * min_in_game),
         
         qtr_lead = lead(qtr, default = 1),
         min_lead = lead(min, default = 0),
         sec_lead = lead(sec, default = 0),
         qtr_play_end = ifelse(qtr_lead == qtr, qtr, 
                               ifelse(qtr_lead == 1, 4,
                                      qtr_lead - 1)),
         min_play_end = ifelse(qtr_lead == qtr, min_lead,
                               ifelse(half != half_lead, 0, min_lead)),
         sec_play_end = ifelse(qtr_lead == qtr, sec_lead,
                               ifelse(half != half_lead, 0, sec_lead)),
         
         type_lead = lead(type, default = "NONE"),
         
         
         # Pts offense/defense at the end of the play
         off_lead = ifelse(gid == gid_lead, lead(off), off),
         ptso_eop = ifelse(gid == gid_lead, 
                                ifelse(off == off_lead, lead(ptso), lead(ptsd)),
                             ifelse(pts >= 0, ptso + pts, ptso)),
         ptsd_eop = ifelse(gid == gid_lead, 
                                  ifelse(off == off_lead, lead(ptsd), lead(ptso)),
                             ifelse(pts <= 0, ptsd - pts, ptsd))
  ) %>%
  merge(GAMES_DF, ., by = 'gid')

source("expected_points/calc_play_type_info.R")
source("expected_points/game_tracker.R")
source("expected_points/rename_ex_score_info.R")

KOFFS_DF <- PLAYS_DF %>%
  filter(type == "KOFF") %>%
  mutate(half_lag_koff = lag(half, default = 1))

DRIVE_KEY <- read_csv("nfl_00_16_corrected/DRIVE.csv") %>%
  mutate(fpid_lead = lead(fpid, default = 742000)) %>%
  dplyr::select(fpid, fpid_lead, uid) %>%
  pmap_df(create_drive_num_key_single
          
          
          
  )

create_rplay_key_single <- function(pid,
                                    pid_last_reset,
                                    ptso_eop,
                                    ptsd_eop,
                                    off,
                                    def,
                                    pts,
                                    qtr_play_end,
                                    min_play_end,
                                    sec_play_end, min_in_half_lead,
                                    min_in_game_lead){
  tibble(pid_reset = pid,
         pid = (pid_last_reset + 1):pid,
         ptso_eop_reset = ptso_eop,
         ptsd_eop_reset = ptsd_eop,
         off_reset = off,
         def_reset = def,
         pts_reset = pts,
         qtr_reset = qtr_play_end,
         min_reset = min_play_end,
         sec_reset = sec_play_end,
         min_in_half_reset = min_in_half_lead,
         min_in_game_reset = min_in_game_lead,
         min_in_game_par = min_in_game_lead
         ) 
}
  
RPLAYS_KEY <- PLAYS_DF %>%
  filter(pts %in% c(-8, -7, -6, -3, 3, 6, 7, 8) |
           half != half_lead) %>%
  mutate(pid_last_reset = lag(pid, default = 0)) %>%
  dplyr::select(pid, pid_last_reset, ptso_eop, ptsd_eop, off, def, pts, 
                qtr_play_end, min_play_end, sec_play_end, min_in_half_lead, min_in_game_lead) %>%
  pmap_df(create_rplay_key_single)

create_eohplay_key_single <- function(pid,
                                    pid_last_eoh,
                                    ptso_eop,
                                    ptsd_eop,
                                    off){
  tibble(pid = (pid_last_eoh + 1):pid,
         ptso_eop_eoh = ptso_eop,
         ptsd_eop_eoh = ptsd_eop,
         off_eoh = off
         ) 
}

EOHPLAYS_KEY <- PLAYS_DF %>%
  filter(half != half_lead) %>%
  mutate(pid_last_eoh = lag(pid, default = 0)) %>%
  dplyr::select(pid, pid_last_eoh, ptso_eop, ptsd_eop, off) %>%
  pmap_df(create_eohplay_key_single)


# Determine Kickoff Type

# There are often 1 or more plays ahead of a TD, CONV, FGXP, or S
# before a FG.
#
# This function looks ahead of those plays and catches to catch
# the corresponding kickoff
catch_plays_ahead <- function(event_df, koff_type, merge_df, lahead = 4){
  if (koff_type != "S"){
    event_df %>%
      merge(PLAYS_DF %>% dplyr::select(pid, qtr)) %>%
      filter(qtr %in% c(1, 2, 3, 4)) %>%
      transmute(pid = pid + lahead,
                kickoff_type = koff_type) %>%
      merge(merge_df)
  } else if (koff_type == "S"){
    event_df %>%
      merge(PLAYS_DF %>% dplyr::select(pid, qtr)) %>%
      filter(qtr %in% c(1, 2, 3, 4)) %>%
      transmute(pid = pid + lahead,
                kickoff_type = koff_type) %>%
      merge(merge_df) %>%
      bind_rows(PLAYS_DF %>% 
                  filter(qtr %in% c(1, 2, 3, 4) & 
                                    abs(pts) == 2) %>%
                  transmute(pid = pid + lahead,
                    kickoff_type = "S")) %>%
      distinct() %>%
      merge(merge_df)
  }
}

create_koff_key <- function(merge_df = KOFFS_DF %>% 
                              dplyr::select(pid), lahead = 1,
                            koff_type_key = NULL,
                            all_key = F){
  print(sprintf("Looking ahead by %d", lahead))
  XP <- read_csv("nfl_00_16_corrected/FGXP.csv") %>%
    filter(fgxp == "XP") %>%
    catch_plays_ahead("TD", merge_df = merge_df, lahead = lahead)
  
  TD <- read_csv("nfl_00_16_corrected/TD.csv") %>%
   catch_plays_ahead("TD", merge_df = merge_df, lahead = lahead)
  
  
  CONV <- read_csv("nfl_00_16_corrected/CONV.csv") %>%
    catch_plays_ahead("TD", merge_df = merge_df, lahead = lahead)
  
  TD <- bind_rows(XP, TD, CONV) %>% 
    distinct()
  
  FG <- read_csv("nfl_00_16_corrected/FGXP.csv") %>%
    filter(fgxp == "FG") %>%
    catch_plays_ahead("FG", merge_df = merge_df, lahead = lahead)
  
    
  SAFETY <- read_csv("nfl_00_16_corrected/SAFETY.csv") %>%
    catch_plays_ahead("S", merge_df = merge_df, lahead = lahead)
    # SAFETY.csv is missing fumbles returned for safeties.
  
  HALF <- KOFFS_DF %>%
    dplyr::select(pid, half, half_lag_koff) %>%
    filter(half != half_lag_koff | 
             pid == min(KOFFS_DF$pid)) %>%
    transmute(pid = pid,
              kickoff_type = ifelse(is.na(half_lag_koff), "1STHF",
                                          ifelse(half > half_lag_koff, 
                                    ifelse(half == 2, "2NDHF", 
                                           ifelse(half < half_lag_koff, 
                                                  ifelse(half == 1, "1STHF",
                                                         NA), "1STHF")), "1STHF"))
                
    ) %>%
    merge(merge_df)
  
  KOFF_TYPE_KEY <- bind_rows(FG, SAFETY, HALF, TD)
  
  KOFF_CONFLICT <- KOFF_TYPE_KEY %>%
    group_by(pid) %>%
    filter(n() > 1) %>%
    filter(!(kickoff_type %in% c("1STHF", "2NDHF")))
  
  KOFF_TYPE_KEY <- KOFF_TYPE_KEY %>%
    filter(!((pid %in% KOFF_CONFLICT$pid) &
             !(kickoff_type %in% c("1STHF", "2NDHF"))))
  
  if (is.null(koff_type_key)){
    missing <- setdiff(merge_df$pid, KOFF_TYPE_KEY$pid)
  } else {
    new_key <- bind_rows(KOFF_TYPE_KEY, koff_type_key)
    missing <- setdiff(merge_df$pid, new_key$pid)
  }
  
  create_all_plays_koff_key <- function(pid, pid_next_koff,
                                        kickoff_type){
    tibble(pid_koff = pid,
           pid = (pid + 1):(pid_next_koff - 1),
           kickoff_type = kickoff_type,
           after_koff =  ifelse(kickoff_type %in% c("TD", "FG",
                                                    "1STHF", "2NDHF"),
                                1, 0))
    
  }
  
  
  print(length(missing))
  if (1731 %in% KOFF_TYPE_KEY$pid){
    browser()
  }
  if (length(missing) == 0){
    final_koff_key <- bind_rows(koff_type_key, KOFF_TYPE_KEY) %>%
      arrange(pid)
    if (all_key){
      print("Making Fdown ALl Key")
      final_koff_key <- final_koff_key %>%
        mutate(pid_next_koff = lead(pid, default = 742000)) %>%
        pmap_df(create_all_plays_koff_key)
    }
    final_koff_key 
  } else {
    if (is.null(koff_type_key)){
      create_koff_key(tibble(pid = missing), lahead = lahead + 1, 
                      koff_type_key = KOFF_TYPE_KEY,
                      all_key = all_key)
    } else {
      if (lahead == 4){
        browser()
      } 
      create_koff_key(tibble(pid = missing), lahead = lahead + 1, 
                      koff_type_key = new_key,
                      all_key = all_key)
    }
  }
}

KOFF_TYPE_KEY <- create_koff_key()
FDOWN_KOFF_KEY <- create_koff_key(all_key = T)

MASTER_KOFF_KEY <- list(KOFF_TYPE_KEY,
                 RPLAYS_KEY,
                 EOHPLAYS_KEY
                 ) %>%
  reduce(merge)

MASTER_FDOWN_KEY <- list(FDOWN_KOFF_KEY,
                         RPLAYS_KEY,
                         EOHPLAYS_KEY,
                         DRIVE_KEY) %>%
  reduce(merge)

write_fdown_points <- function(
  name = "expected_points/correc_raw_fdowns_nscore_half_and_reset.csv",
  debug = F){
  
  
  first_and_tens <- PLAYS_DF %>%
    dplyr::filter(dwn == 1,
         ytg == 10 | yfog >= 90,
         qtr %in% c(1, 2, 3, 4),
         def != off,
         type %in% c("PASS", "RUSH", "NOPL"))
  
  first_and_tens_next <- first_and_tens %>%
    merge(., MASTER_FDOWN_KEY, all.x = T) %>%
    mutate(
      Season = seas,
      Week = wk,
      Playoff = ifelse(seas == 2001, 
                       ifelse(Week >= 18, 1, 0), 
                       ifelse(Week >= 17, 1, 0)),
      Armchair_gid = gid,
      Armchair_pid = pid,
      Qtr = qtr,
      Min = min,
      Sec = sec,
      Time_left_in_half = min_in_half,
      Time_left_in_game = min_in_game,
      Pts_Off = ptso, Pts_Def = ptsd, Off = off,
      Def = def,
      Home = h,
      Net_Score_to_Half = ifelse(off == off_eoh,
                                        (ptso_eop_eoh - ptso) - (ptsd_eop_eoh - ptsd),
                                        (ptsd_eop_eoh - ptso) - (ptso_eop_eoh - ptsd)),
      Net_Score_to_Reset = ifelse(off == off_reset,
                                        (ptso_eop_reset - ptso) - (ptsd_eop_reset - ptsd),
                                        (ptsd_eop_reset - ptso) - (ptso_eop_reset - ptsd)),
      Reset_Team_to_Score = ifelse(off == off_reset, 
                                   ifelse(pts_reset > 0, 1,
                                          ifelse(pts_reset == 0, 0, -1)),
                                   ifelse(pts_reset < 0, 1,
                                          ifelse(pts_reset == 0, 0, -1))),
      Reset_qtr = qtr_reset,
      Reset_min = min_reset,
      Reset_sec = sec_reset,
      Time_to_reset = min_in_game - min_in_game_par,
      Time_left_in_half_at_Reset = min_in_half_reset,
      Time_left_in_game_at_Reset = min_in_game_reset,
      Kickoff_Type = kickoff_type,
      Offense_Won = ifelse(off == Home, ifelse(ptsh > ptsv, 1, 0),
                           ifelse(ptsv > ptsh, 0, 0)),
      Half_num = ifelse(Qtr %in% c(1,2), (Armchair_gid - 1) * 2 + 1,
                             Armchair_gid * 2)
    ) %>%
    distinct()
  
  FIRST_AFTER_KOFF_KEY <- first_and_tens_next %>%
    group_by(pid_koff) %>%
    summarise(pid = min(pid)) %>%
    mutate(follow_kickoff = 1) %>%
    dplyr::select(pid, follow_kickoff)
  
  first_and_tens_last <- first_and_tens_next %>%
    merge(FIRST_AFTER_KOFF_KEY, all.x = T) %>%
    mutate(follow_kickoff = ifelse(is.na(follow_kickoff), 0, follow_kickoff))
    
  
    
  if (debug){
    first_and_tens_last %>%
      view_high_reset_points()
  }
    
  first_and_tens_last <- first_and_tens_last %>%
    mutate(Yfog = yfog,
           Armchair_dsq = dseq,
           Drive_start = ifelse(dseq == 1, 1, 0),
           Follow_Kickoff = follow_kickoff, 
           Drive_num = drive_num 
           ) %>%
    by_row(calc_ex_score_info, game_tracker = GAME_TRACKER, .collate = "cols",
           .to = "ex_score_info") %>%
    rename_ex_score_info() %>%
    dplyr::select(matches("^[A-Z]", ignore.case = F))
    
  
  write.csv(first_and_tens_last, name, row.names = F)
  first_and_tens_last
}

view_high_reset_points <- function(df, thresh = 11){
  pids_of_int <- df$pid[
    which(abs(df$Net_Score_to_Reset) >= thresh)]
  View(
  df %>%
    filter(pid %in% pids_of_int) %>% dplyr::select(seas, wk,
                                                   qtr, min, sec,
      pid, pid_reset, ptso_eop_reset, ptsd_eop_reset,
      ptso, ptsd, off, off_reset
    )
  )
}


write_koff_exp_points <- function(
  name = "expected_points/corec_raw_koff_nscore_half_and_reset.csv",
  debug = F){

  KOFFS_DF_FINAL <- KOFFS_DF %>%
    merge(., MASTER_KOFF_KEY, all.x = T) %>%
    mutate(
      Season = seas,
      Week = wk,
      Playoff = ifelse(seas == 2001, 
                       ifelse(Week >= 18, 1, 0), 
                       ifelse(Week >= 17, 1, 0)),
      Armchair_gid = gid,
      Armchair_pid = pid,
      Qtr = qtr,
      Min = min,
      Sec = sec,
      Time_left_in_half = min_in_half,
      Time_left_in_game = min_in_game,
      Pts_Off = ptso,
      Pts_Def = ptsd,
      Off = off,
      Def = def,
      Home = h,
      Net_Score_to_Half = ifelse(off == off_eoh,
                                        (ptso_eop_eoh - ptso) - (ptsd_eop_eoh - ptsd),
                                        (ptsd_eop_eoh - ptso) - (ptso_eop_eoh - ptsd)),
      Net_Score_to_Reset = ifelse(off == off_reset,
                                        (ptso_eop_reset - ptso) - (ptsd_eop_reset - ptsd),
                                        (ptsd_eop_reset - ptso) - (ptso_eop_reset - ptsd)),
      Reset_Team_to_Score = ifelse(off == off_reset, 
                                   ifelse(pts_reset > 0, 1,
                                          ifelse(pts_reset == 0, 0, -1)),
                                   ifelse(pts_reset < 0, 1,
                                          ifelse(pts_reset == 0, 0, -1))),
      Reset_qtr = qtr_reset,
      Reset_min = min_reset,
      Reset_sec = sec_reset,
      Time_to_reset = min_in_game - min_in_game_par,
      Time_left_in_half_at_Reset = min_in_half_reset,
      Time_left_in_game_at_Reset = min_in_game_reset,
      Kickoff_Type = kickoff_type,
      Offense_Won = ifelse(off == Home, ifelse(ptsh > ptsv, 1, 0),
                           ifelse(ptsv > ptsh, 0, 0)),
      Half_num = ifelse(Qtr %in% c(1,2), (Armchair_gid - 1) * 2 + 1,
                             Armchair_gid * 2)
    )
  
  if (debug){
    KOFFS_DF_FINAL %>%
    view_high_reset_points()
  }
  KOFFS_DF_FINAL <- KOFFS_DF_FINAL %>%
    dplyr::select(matches("^[A-Z]", ignore.case = F))
  write.csv(KOFFS_DF_FINAL, name, row.names = F)
  tfinal <- read.csv(name)
  KOFFS_DF_FINAL
}

KOFFS_DF_FINAL <- write_koff_exp_points(debug = T)
FDOWN_DF_FINAL <- write_fdown_points(debug = T)