# R file to create model for expected number of points for next score
library(purrrlyr)
library(dplyr)
library(readr)
library(MASS)
library(purrr)
library(magrittr)

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

calc_net_scores <- function(play_row, off_of_int){
  # browser()
  if (off_of_int == play_row$off){
    return(play_row$pts)
  } else if (off_of_int == play_row$def){
    return(- play_row$pts)
  } else {
    if (play_row$pts != 0){
      print('welp...')
    }
    # browser()
    return(play_row$pts)
  }
}

create_drive_num_key_single <- function(fpid, fpid_lead, uid){
  tibble(Drive_num = rep(uid, fpid_lead - fpid),
         Armchair_pid = fpid:(fpid_lead - 1))
}

GAMES_DF <- read_csv('nfl_00_16/GAME.csv') %>%
  dplyr::select(gid, h, seas, wk)
PLAYS_DF <- read_csv('nfl_00_16/PLAY.csv') %>%
  mutate(min_in_half = ifelse(qtr %in% c(2, 4), min + sec / 60, # Not using overtime
                             ifelse(qtr %in% c(1,3),     
                                    15 + min + sec / 60, NA)),
        min_in_game = ifelse(qtr %in% c(3,4), min_in_half, 
                             ifelse(qtr %in% c(1, 2), 30 + min_in_half, -1)),
        gid_lead = lead(gid),
        min_in_game_lead = lead(min_in_game),
        type_lead = lead(type),
        len_clock = ifelse(gid == gid_lead, (min_in_game - min_in_game_lead) * 60, 60 * min_in_game)) %>% 
  merge(GAMES_DF, ., by = 'gid')

PLAY_TYPES <- unique(PLAYS_DF$type)

PLAYS_DF$min_in_half <- as.numeric(PLAYS_DF$min_in_half)

DRIVE_KEY <- read_csv("nfl_00_16/DRIVE.csv") %>%
  mutate(fpid_lead = lead(fpid, default = max(PLAYS_DF$pid))) %>%
  dplyr::select(fpid, fpid_lead, uid) %>%
  pmap_df(create_drive_num_key_single)

extract_game_plays_df <- function(plays_df, game_id){
  (plays_df %>% filter(gid == game_id))
}

# Function to make calculations more efficient; stores dataframes for each game
GAME_TRACKER <- sort(unique(PLAYS_DF$gid)) %T>%
 print("Making Game Tracker", null = .) %>%
 map(~ extract_game_plays_df(plays_df = PLAYS_DF,  game_id = .))

calc_play_type_info <- function(cur_off_to_half_df, reset_play){
  half_info_df <- PLAY_TYPES %>%
    map(~create_type_count(df = cur_off_to_half_df, desired_type = .,
                           suffix = "half")) %>%
    bind_cols()
  
  half_info_list <- as.list(colSums(half_info_df))
  
  cur_off_to_reset_df <- cur_off_to_half_df %>%
    dplyr::filter(pid <= reset_play$pid)
  reset_info_df <- PLAY_TYPES %>%
    map(~create_type_count(df = cur_off_to_reset_df, desired_type = .,
                           suffix = "reset")) %>%
    bind_cols()
  
  reset_info_list <- as.list(colSums(reset_info_df))
  
  if (nrow(cur_off_to_reset_df) != 0){
    last_off_play <- tail(cur_off_to_reset_df, 1)
    # Catch conversions after a reset
    if (last_off_play$type_lead == "CONV"){
      reset_info_list$CONV_count_reset = reset_info_list$CONV_count_reset + 1
      
      conv_play <- cur_off_to_half_df %>%
        dplyr::filter(pid == last_off_play$pid + 1,
                      type == "CONV")
      
      # It's possible a conversion was from an interception
      # returned for a TD
      if (nrow(conv_play) != 0){
        print(sprintf("Conversion caught (conv at %d)", last_off_play$pid + 1))
        reset_info_list$CONV_len_reset = reset_info_list$CONV_len_reset + conv_play$len 
        reset_info_list$CONV_len_clock_reset = reset_info_list$CONV_len_clock_reset + conv_play$len_clock 
      }
    }
  }
  
  rvec <- c(unlist(reset_info_list), unlist(half_info_list))
  if (length(rvec) != 48){
    browser()
  }
  rvec 
}

create_type_count <- function(df, desired_type, suffix){
  type_track <- sprintf("%s_count_%s", desired_type, suffix)
  type_len <- sprintf("%s_len_%s", desired_type, suffix)
  type_len_clock <- sprintf("%s_len_clock_%s", desired_type, suffix)
  df %>%
    transmute(!!type_track := ifelse(type == desired_type, 1, 0),
           !!type_len := ifelse(type == desired_type, len, 0),
           !!type_len_clock := ifelse(type == desired_type, 
                                      len_clock, 0))
}

calc_net_score_info <- function(play_row, game_tracker){
  # Get the current play, and all future plays within the same game that are in
  # the same half of the game.
  # print(sprintf("Calc net score info for pid: %d", play_row$pid))
  cur_game_plays <- game_tracker[[play_row$gid]] # Extract current game df
  search_df <- cur_game_plays %>%
    dplyr::filter(pid >= play_row$pid,
           (qtr == play_row$qtr) | 
             (qtr == ifelse(play_row$qtr %in% c(1, 3), play_row$qtr + 1, 
                            play_row$qtr))) %>%
    by_row(calc_net_scores, off_of_int = play_row$off,
                .collate = "cols", .to = "net_score") 
  
  # Get the pass or rush plays from the current offense
  cur_off_to_half <- search_df %>%
    filter(off == play_row$off)
  
  # We don't count a safety as a reset, only FGs and TD's + extra point(s)
  fg_td_only <- search_df %>%
    dplyr::filter(net_score %in% c(-8, -7 , -6, -3, 3, 6, 7, 8)) 
  
  # If there wasn't a TD + extra point(s) or FG, we want to add the last play 
  last_play <- tail(search_df, 1)
  if (nrow(fg_td_only) == 0){
    fg_td_only <- fg_td_only %>%
      bind_rows(last_play)
    reset_play <- last_play
   
    # Incorporate scores before reset point 
    before_reset_plays <- search_df %>%
      dplyr::filter(pid < reset_play$pid & pid >= play_row$pid)
    
    net_till_reset_score <- reset_play$net_score + sum(before_reset_plays$net_score)
    time_to_reset <- play_row$min_in_half
    reset_min_in_half <- 0
    # browser()
    
  # Otherwise, there was a TD + extra point(s) or FG before the half ended.
  } else {
    reset_play <- fg_td_only[1,]
    
    # Incoporate scores before reset point.
    before_reset_plays <- search_df %>%
      dplyr::filter(pid < reset_play$pid & pid >= play_row$pid)
    
    net_till_reset_score <- reset_play$net_score + sum(before_reset_plays$net_score)
    
    # If there was only one TD or FG before the half ended.
    play_after_reset <- search_df %>% 
      dplyr::filter(pid == reset_play$pid + 1)
    # If the TD or FG was the actually the last play of the game
    if (nrow(play_after_reset) == 0){
      time_to_reset <- play_row$min_in_half
      reset_min_in_half <- 0
    # If the TD or FG was not the last play of the half
    } else if (((reset_play$qtr == 2) & (play_after_reset$qtr == 3))){
      time_to_reset <- play_row$min_in_half
      reset_min_in_half <- 0
    # If the TD or FG was not the last play of the half
    } else { 
      time_to_reset <- play_row$min_in_half - play_after_reset$min_in_half 
      reset_min_in_half <- play_after_reset$min_in_half
    }
  }
  
  # It's possible that the last play of the half could be a safety. Let's check
  # if that's the case.
  if (net_till_reset_score %in% c(2, -2)){
    print('last play in half is safety? or safety between play and half?')
    print(sprintf('g: %i | p: %i', play_row$gid, play_row$pid))
  }
  if (reset_play$net_score < 0){
    Reset_Team_to_Score <- -1
  } else if (reset_play$net_score > 0){
    Reset_Team_to_Score <- 1
  } else {
    Reset_Team_to_Score <- 0
  }
  
  type_count_play_vec <- calc_play_type_info(cur_off_to_half,
                                             reset_play)
  net_till_half_score <- sum(search_df$net_score)
  
  net_score_info <- as.numeric(c(net_till_half_score,
                                  net_till_reset_score, 
                                  reset_min_in_half, 
                                  time_to_reset, 
                                  Reset_Team_to_Score,
                                  type_count_play_vec))
}



calc_koff_info <- function(play_row, game_tracker, test = FALSE){
  # print(sprintf("Koff for pid: %d", play_row$pid))
  if (test){
    browser()
  }
  # Create dummy for 1st and 10 following a kickoff and the kickoff typeg
  if (play_row$qtr %in% c(1, 2)){
    half <- 1
    play_half <- c(1, 2)
  } else {
    half <- 2
    play_half <- c(3, 4)
  }
  
  cur_game_plays <- game_tracker[[play_row$gid]]
  plays_before <- cur_game_plays %>%
    dplyr::filter(qtr %in% play_half,
                  pid < play_row$pid)
  
  if (nrow(plays_before) == 0){ # Special case in which case the kickoff that starts 1st or second half of game is not recorded
    kickoff_dummy <- "True"
    if (half == 1){
      kickoff_type <- "1STHF"
    } else {
      kickoff_type <- "2NDHF"
    }
    return(c(kickoff_dummy, kickoff_type))
  }
  
  kickoffs_before <- plays_before %>%
    filter(type %in% c("KOFF", "ONSD"))
  
  if (nrow(kickoffs_before) == 0){ # Part of special case above, but for first downs that come after.
    first_downs_before <- plays_before %>% # No kickoff to refer to, only whether or not there are other first downs before
      filter(dwn == 1, ytg == 10 | yfog >= 90)
   if (nrow(first_downs_before) == 0){
     kickoff_dummy <- "False"
     kickoff_type <- "NA"
   } else {
      kickoff_dummy <- "True"  
      if (half == 1){
        kickoff_type <- "1STHF"
      } else {
        kickoff_type = "2NDHF"
      }
   }
    return(c(kickoff_dummy, kickoff_type))
  } 
  
  last_kickoff <- tail(kickoffs_before, 1)
  plays_after_kickoff <- cur_game_plays %>%
    dplyr::filter(pid > last_kickoff$pid)
  play_before_kickoff <- plays_before %>%
    dplyr::filter(pid == last_kickoff$pid - 1) %>%
    head(1)
  
  if (nrow(play_before_kickoff) != 0){ # Only do if kickoff doesn't start half.
    # It's possible the last play before a kickoff is the extra point or conversion.
    # If this is the case, since we are using the `pts` variable to see what type
    # of score it was (if any), we must find the original touchdown play to make
    # sure that we see that 6, 7, or 8 points are scored further down.
    if (play_before_kickoff$type == "FGXP" && play_before_kickoff$pts == 0){
      maybe_field_goal <- play_before_kickoff 
    }
    buffer <- 2
    try( # It's possible that if there's a penalty before the kickoff at the beginning of the game 
      # that this can break, we simply let it break, and the stuff ahead appropriately marks
      # the kickoff as a first or second half one.
      expr = 
    while (play_before_kickoff$type %in% c("FGXP", "CONV", "NOPL") && 
           play_before_kickoff$pts == 0){
        play_before_kickoff <- plays_before %>%
          dplyr::filter(pid == max(last_kickoff$pid - buffer, 1)) %>%
          head(1)
        buffer <- buffer + 1
    }
    )
    try(
    if (play_before_kickoff$pts == 0){
      print(cur_game_plays %>% 
              filter(pid < play_row$pid + 15, pid > play_row$pid - 15) %>%
              dplyr::select(qtr, type, pts, pid))
      missed_fg <- TRUE
      return(c("True", "FG")) # Special case for missed field goal
    } else {
      missed_fg <- FALSE
    }
    )
  }
  first_down_after_kickoff <- cur_game_plays %>%
    dplyr::filter(pid > last_kickoff$pid &
                    pid <= play_row$pid) %>%
    dplyr::filter(dwn == 1,
           (ytg == 10) | (yfog > 90),
           qtr %in% c(1, 2, 3, 4),
           def != off,
           qtr %in% play_half
             ) %>%
    head(1)
  
  first_kickoff_of_half <- cur_game_plays %>%
    filter(qtr %in% play_half,
           type %in% c("KOFF", "ONSD"),
           gid == play_row$gid) %>%
    head(1)
  # browser() 
  if (nrow(play_before_kickoff) == 0){
    if (play_row$pid != first_down_after_kickoff$pid){
      kickoff_dummy <- "False"
      kickoff_type <- "NA"
    } else {
      kickoff_dummy <- "True"
      if (half == 1){
        kickoff_type <- "1STHF"
      } else if (half == 2){
        kickoff_type <- "2NDHF"
      }
    }
  } else {
    if (play_row$pid != first_down_after_kickoff$pid){
      kickoff_dummy <- "False"
      kickoff_type <- "NA"
    } else {
      kickoff_dummy <- "True"
      if (last_kickoff$pid == first_kickoff_of_half$pid){
        if (half == 1){
          kickoff_type <- "1STHF"
        } else if (half == 2){
          kickoff_type <- "2NDHF" 
        }
        
      } else {
        pts <- abs(play_before_kickoff$pts)
        if (pts == 3){
          kickoff_type <- "FG"
        } else if (pts %in% c(6, 7, 8)){
          kickoff_type <- "TD"
        } else if (pts == 2){
          kickoff_type <- "S"
        }
      }
    }
  }
  # browser()
  kickoff_info <- c(kickoff_dummy, kickoff_type)
  if (any(is.na(kickoff_info))){
    browser()  
  } else {
    kickoff_info
  }
}


make_raw_exp_scores_table <- function(test = FALSE, plays_df){
  if (test){
    gid_start <- 4463
  } else {
    gid_start <- 1
  }
  first_and_tens <- plays_df %>%
    dplyr::filter(dwn == 1,
         ytg == 10 | yfog > 90,
         qtr %in% c(1, 2, 3, 4),
         def != off,
         gid >= gid_start,
         type %in% c("PASS", "RUSH", "NOPL")) %>%
    dplyr::select(seas, wk, gid, pid, qtr, min_in_game, min_in_half, min, sec, h,
                  ptso, ptsd, off, def, yfog, dseq, type)
  
  first_and_tens <- first_and_tens %>%
    by_row(calc_net_score_info, game_tracker = GAME_TRACKER, .collate = "cols",
           .to = "ex_score_info")
  
  first_and_tens <- first_and_tens %>%
    by_row(calc_koff_info, game_tracker = GAME_TRACKER, .collate = "cols",
           .to = "koff_info") %>%
    mutate(koff_info1 = ifelse(koff_info1 == "True", 1, 0)) %>%
    dplyr::mutate(drive_start = ifelse(dseq == 1, 1, 0)) %>%
    
  print("make raw_exp_score table terminating")
  first_and_tens
}

convert_reset_time <- function(play_row){
  print(sprintf("Converting Reset Time for pid: %d", play_row$pid))
  # browser()
  full_time <- play_row$reset_min_in_game
  
  if (full_time > 45){
    qtr <- 1
  } else if (full_time > 30 & full_time <= 45){
    qtr <- 2
  } else if (full_time > 15 & full_time <= 30){
    qtr <- 3
  } else if (full_time >= 0 & full_time <= 15){
    qtr <- 4
  }
  
  qtr_time <- full_time - 15 * (4 - qtr)
  qtr_min <- floor(qtr_time)
 
  # Extract the second 
  qtr_sec_dec <- qtr_time - qtr_min
  if (qtr_sec_dec != 0){
    qtr_sec_frac <- fractions(qtr_sec_dec)
    num_denom <- as.numeric(unlist(strsplit(attr(qtr_sec_frac,"fracs"),"/")))
    tsf <- 60 / num_denom[2]
    qtr_sec <- num_denom[1] * tsf
  } else {
    qtr_sec <- 0
  }
    
  (reset_time_info <- c(qtr, qtr_min, qtr_sec))
}

make_off_won_binary <- function(play_row, game_tracker = GAME_TRACKER){
  print(sprintf("making binary for pid: %d", play_row$pid))
  cur_game <- play_row$gid 
  cur_off <- play_row$off  
  search_df <- game_tracker[[cur_game]] 
  
  final_play <- tail(search_df, 1)
  
  if (cur_off == final_play$off){
    return(ifelse(final_play$ptso > final_play$ptsd, 1, 0))
  } else if (cur_off == final_play$def){
    return(ifelse(final_play$ptsd > final_play$ptso, 1, 0))
  } else{
    print(final_play)
    browser() 
  }
}

first_and_tens <- make_raw_exp_scores_table(test = FALSE, plays_df = PLAYS_DF) %>%
  dplyr::mutate(reset_min_in_game = ifelse(qtr %in% c(1, 2), 30 + ex_score_info3,
                                    ifelse(qtr %in% c(3, 4), ex_score_info3,
                                           NA)),
         Playoff = ifelse(wk >= 17, 1, 0)) %T>%
  print("Converting reset time", null = .) %>%
  by_row(convert_reset_time, .collate = "cols", .to = "reset_time_info") %T>%
  print("Making won playoff binary", null = .) %>%
  by_row(make_off_won_binary, .collate = "cols", .to = "Offense_Won")

first_and_tens <- first_and_tens %>%
  rename(# Time variables
         Net_Score_to_Half = ex_score_info1,
         Net_Score_to_Reset = ex_score_info2,
         Min_Reset_to_Half = ex_score_info3,
         Time_to_Reset = ex_score_info4,
         Reset_Team_to_Score = ex_score_info5,
         
         # So many count, len, and len_clock,
         # Should find better way to do this
         # Could have used closures/quotes and what not, oh well
         KOFF_reset_count = ex_score_info6,
         KOFF_reset_len = ex_score_info7,
         KOFF_reset_len_clock = ex_score_info8,
         RUSH_reset_count = ex_score_info9,
         RUSH_reset_len = ex_score_info10,
         RUSH_reset_len_count = ex_score_info11,
         PASS_reset_count = ex_score_info12,
         PASS_reset_len = ex_score_info13,
         PASS_reset_len_clock = ex_score_info14,
         FGXP_reset_count = ex_score_info15,
         FGXP_reset_len = ex_score_info16,
         FGXP_reset_len_clock = ex_score_info17,
         PUNT_reset_count = ex_score_info18,
         PUNT_reset_len = ex_score_info19,
         PUNT_reset_len_clock = ex_score_info20,
         NOPL_reset_count = ex_score_info21,
         NOPL_reset_len = ex_score_info22,
         NOPL_reset_len_clock = ex_score_info23,
         ONSD_reset_count = ex_score_info24,
         ONSD_reset_len = ex_score_info25,
         ONSD_reset_len_clock = ex_score_info26,
         CONV_reset_count = ex_score_info27,
         CONV_reset_len = ex_score_info28,
         CONV_reset_len_clock = ex_score_info29,
         
         KOFF_half_count = ex_score_info30,
         KOFF_half_len = ex_score_info31,
         KOFF_half_len_clock = ex_score_info32,
         RUSH_half_count = ex_score_info33,
         RUSH_half_len = ex_score_info34,
         RUSH_half_len_count = ex_score_info35,
         PASS_half_count = ex_score_info36,
         PASS_half_len = ex_score_info37,
         PASS_half_len_clock = ex_score_info38,
         FGXP_half_count = ex_score_info39,
         FGXP_half_len = ex_score_info40,
         FGXP_half_len_clock = ex_score_info41,
         PUNT_half_count = ex_score_info42,
         PUNT_half_len = ex_score_info43,
         PUNT_half_len_clock = ex_score_info44,
         NOPL_half_count = ex_score_info45,
         NOPL_half_len = ex_score_info46,
         NOPL_half_len_clock = ex_score_info47,
         ONSD_half_count = ex_score_info48,
         ONSD_half_len = ex_score_info49,
         ONSD_half_len_clock = ex_score_info50,
         CONV_half_count = ex_score_info51,
         CONV_half_len = ex_score_info52,
         CONV_half_len_clock = ex_score_info53,
         
         Reset_qtr = reset_time_info1,
         Reset_min = reset_time_info2,
         Reset_sec = reset_time_info3,
         # Other stuff John wants renamed
         Season = seas,
         Week = wk,
         Armchair_gid = gid,
         Armchair_pid = pid,
         Qtr = qtr,
         Min = min,
         Sec = sec,
         Min_left_in_half = min_in_half,
         Min_left_in_game = min_in_game,
         Pts_Off = ptso,
         Pts_Def = ptsd,
         Off = off,
         Def = def,
         Home = h,
         Yfog = yfog,
         Armchair_dsq = dseq,
         Drive_start = drive_start,
         Min_Reset_to_GameEnd = reset_min_in_game,
         Follow_Kickoff = koff_info1,
         Kickoff_Type = koff_info2
         ) %>%
  # Changes John wants
  mutate(Follow_Kickoff = ifelse(Kickoff_Type == "S", 0, Follow_Kickoff),
         Kickoff_Type = ifelse(is.na(Kickoff_Type), "None", Kickoff_Type)) %>%
  merge(DRIVE_KEY, all.x = T) %>%
  mutate(Half_num = ifelse(Qtr %in% c(1,2), (Armchair_gid - 1) * 2 + 1,
                           Armchair_gid * 2)) %>%
  dplyr::select(
  Season,
  Week,
  Playoff,
  Armchair_gid,
  Armchair_pid,
  Qtr,
  Min,
  Sec,
  Min_left_in_half,
  Min_left_in_game,
  Pts_Off,
  Pts_Def,
  Off,
  Def,
  Home,
  Net_Score_to_Half,
  Net_Score_to_Reset,
  Reset_Team_to_Score,
  Reset_qtr,
  Reset_min,
  Reset_sec,
  Time_to_Reset,
  Min_Reset_to_Half,
  Min_Reset_to_GameEnd,
  Kickoff_Type,
  Offense_Won,
  Half_num,
  
  # Don't apply to kickoff expected points so they're separate
  Drive_start,
  Armchair_dsq,
  Drive_num,
  Follow_Kickoff,
  Yfog,
  
  KOFF_reset_count,
  KOFF_reset_len,
  KOFF_reset_len_clock,
  RUSH_reset_count,
  RUSH_reset_len,
  RUSH_reset_len_count,
  PASS_reset_count,
  PASS_reset_len,
  PASS_reset_len_clock,
  FGXP_reset_count,
  FGXP_reset_len,
  FGXP_reset_len_clock,
  PUNT_reset_count,
  PUNT_reset_len,
  PUNT_reset_len_clock,
  NOPL_reset_count,
  NOPL_reset_len,
  NOPL_reset_len_clock,
  ONSD_reset_count,
  ONSD_reset_len,
  ONSD_reset_len_clock,
  CONV_reset_count,
  CONV_reset_len,
  CONV_reset_len_clock,
 
  KOFF_half_count,
  KOFF_half_len,
  KOFF_half_len_clock,
  RUSH_half_count,
  RUSH_half_len,
  RUSH_half_len_count,
  PASS_half_count,
  PASS_half_len,
  PASS_half_len_clock,
  FGXP_half_count,
  FGXP_half_len,
  FGXP_half_len_clock,
  PUNT_half_count,
  PUNT_half_len,
  PUNT_half_len_clock,
  NOPL_half_count,
  NOPL_half_len,
  NOPL_half_len_clock,
  ONSD_half_count,
  ONSD_half_len,
  ONSD_half_len_clock,
  CONV_half_count,
  CONV_half_len,
  CONV_half_len_clock
  )
  # mutate(Reset_Perct_Rush = ifelse(is.na(Reset_Perct_Rush), 0, Reset_Perct_Rush),
  #        Half_Perct_Rush = ifelse(is.na(Half_Perct_Rush), 0, Half_Perct_Rush))
write_csv(first_and_tens, "expected_points/raw_fdowns_nscore_half_and_reset.csv")

# Drive Number and Half Number Varible Functions

test_extract_row <- function(pid_of_int, plays_df = PLAYS_DF){
  plays_df %>% filter(pid == pid_of_int) %>%
    tail(1)
}

test_show_context <- function(pid_of_int, plays_df = PLAYS_DF){
  df <- plays_df %>% filter(pid < pid_of_int + 15, pid > pid_of_int - 15) %>%
          dplyr::select(type, dwn, ytg, qtr, pts, pid, gid, off)
  list(df = df, pid = pid_of_int)
}
