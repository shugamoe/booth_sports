# R file to create model for expected number of points for next score
library(purrrlyr)
library(dplyr)
library(readr)
library(MASS)
library(purrr)

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

GAMES_DF <- read_csv('nfl_00_16/GAME.csv') %>%
  dplyr::select(gid, h, seas, wk)
PLAYS_DF <- read_csv('nfl_00_16/PLAY.csv') %>%
 mutate(min_in_half = ifelse(qtr %in% c(2, 4), min + sec / 60,
                             ifelse(qtr %in% c(1,3),     # -100 shouldn't happen
                                    15 + min + sec / 60, -100)),
        min_in_game = ifelse(qtr %in% c(3,4), min_in_half, 
                             ifelse(qtr %in% c(1, 2), 30 + min_in_half, -1))) %>% # Not using overtime
 merge(GAMES_DF, ., by = 'gid')
 PLAYS_DF$min_in_half <- as.numeric(PLAYS_DF$min_in_half)
 
extract_game_plays_df <- function(plays_df, game_id){
  (plays_df %>% filter(gid == game_id))
}

# Function to make calculations more efficient; stores dataframes for each game
GAME_TRACKER <- sort(unique(PLAYS_DF$gid)) %>%
 map(~ extract_game_plays_df(plays_df = PLAYS_DF,  game_id = .))

calc_net_score_info <- function(play_row, game_tracker){
  # Get the current play, and all future plays within the same game that are in
  # the same half of the game.
  cur_game_plays <- game_tracker[[play_row$gid]] # Extract current game df
  search_df <- cur_game_plays %>%
    dplyr::filter(pid >= play_row$pid,
           (qtr == play_row$qtr) | 
             (qtr == ifelse(play_row$qtr %in% c(1, 3), play_row$qtr + 1, 
                            play_row$qtr))) %>%
    by_row(calc_net_scores, off_of_int = play_row$off,
                .collate = "cols", .to = "net_score") 
  
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
  
  net_till_half_score <- sum(search_df$net_score)
  (net_score_info <- c(net_till_half_score, net_till_reset_score, reset_min_in_half, time_to_reset, Reset_Team_to_Score))
}

calc_koff_info <- function(play_row, game_tracker){
  print(play_row$pid)
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
  
  kickoffs_before <- plays_before %>%
    dplyr::filter(type %in% c("KOFF", "ONSD"))
  
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
              filter(pid < play_row$pid + 15, pid > play_row$pid - 15)) %>%
              dplyr::select(qtr, type, pts, pid) 
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
  (kickoff_info <- c(kickoff_dummy, kickoff_type))
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

make_raw_exp_scores_table <- function(test = FALSE, plays_df){
  if (test){
    gid_stop <- 5
  } else {
    gid_stop <- Inf
  }
  first_and_tens <- plays_df %>%
    dplyr::filter(dwn == 1,
         ytg == 10 | yfog > 90,
         qtr %in% c(1, 2, 3, 4),
         def != off,
         gid < gid_stop,
         type %in% c("PASS", "RUSH", "NOPL")) %>%
    dplyr::select(seas, wk, gid, pid, qtr, min_in_game, min_in_half, min, sec, h, ptso, ptsd, off, def, yfog, dseq, type)
  first_and_tens <- first_and_tens %>%
    by_row(calc_koff_info, game_tracker = GAME_TRACKER, .collate = "cols",
           .to = "koff_info") %>%
    mutate(koff_info1 = ifelse(koff_info1 == "True", 1, 0)) %>%
    by_row(calc_net_score_info, game_tracker = GAME_TRACKER, .collate = "cols",
           .to = "ex_score_info") %>%
    dplyr::rename(net_score_to_half = ex_score_info1,
           net_score_to_reset = ex_score_info2,
           time_to_reset = ex_score_info4,
           reset_min_in_half = ex_score_info3) %>%
    dplyr::mutate(drive_start = ifelse(dseq == 1, 1, 0)) %>%
    mutate_at(.vars = c("net_score_to_half", "net_score_to_reset",
                "time_to_reset", "reset_min_in_half"), .funs = as.numeric) %>%
    dplyr::mutate(reset_min_in_game = ifelse(qtr %in% c(1, 2), 30 + reset_min_in_half,
                                      ifelse(qtr %in% c(3, 4), reset_min_in_half,
                                             NA)),
           Playoff = ifelse(wk >= 17, 1, 0))
  first_and_tens
}

convert_reset_time <- function(row){
  # browser()
  full_time <- row$reset_min_in_game
  
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

first_and_tens <- make_raw_exp_scores_table(test = TRUE, plays_df = PLAYS_DF) %>%
  by_row(convert_reset_time, .collate = "cols", .to = "reset_time_info") %>% 
  by_row(make_off_won_binary, .collate = "cols", .to = "Offense_Won") %>%
  rename(# Time variables
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
         Net_Score_to_Half = net_score_to_half,
         Net_Score_to_Reset = net_score_to_reset,
         Time_to_Reset = time_to_reset,
         Min_Reset_to_Half = reset_min_in_half,
         Min_Reset_to_GameEnd = reset_min_in_game,
         Reset_Team_to_Score = ex_score_info5,
         Follow_Kickoff = koff_info1,
         Kickoff_Type = koff_info2
         ) %>%
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
  Yfog,
  Armchair_dsq,
  Drive_start,
  Net_Score_to_Half,
  Net_Score_to_Reset,
  Reset_Team_to_Score,
  Reset_qtr,
  Reset_min,
  Reset_sec,
  Time_to_Reset,
  Min_Reset_to_Half,
  Min_Reset_to_GameEnd,
  Follow_Kickoff,
  Kickoff_Type,
  Offense_Won)
write_csv(first_and_tens, 'raw_fdowns_nscore_half_and_reset.csv')

test_koff <- function(pid_of_int, plays_df = PLAYS_DF){
  check_dat <- plays_df %>%
    filter(pid < pid_of_int + 15,
           pid > pid_of_int - 15) %>%
    dplyr::select(pid, qtr, type, pts)
  check_dat
}