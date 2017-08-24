# Play Type Counts and Length Renaming
calc_ex_score_info <- function(play_row, game_tracker){
  cur_game_plays <- game_tracker[[play_row$gid]] # Extract current game df
  search_df <- cur_game_plays %>%
    dplyr::filter(pid >= play_row$pid,
           (qtr == play_row$qtr) | 
             (qtr == ifelse(play_row$qtr %in% c(1, 3), play_row$qtr + 1, 
                            play_row$qtr)))
  
  # Get the pass or rush plays from the current offense
  cur_off_to_half <- search_df %>%
    filter(off == play_row$off)
  
  reset_play <- PLAYS_DF %>% filter(pid == play_row$pid_reset)
  
  (type_count_play_vec <- calc_play_type_info(cur_off_to_half,
                                             reset_play))
}

rename_ex_score_info <- function(df){
  df %>%
    rename(# Time variables
           # So many count, len, and len_clock,
           # Should find better way to do this
           # Could have used closures/quotes and what not, oh well
           KOFF_reset_count = ex_score_info1,
           KOFF_reset_len = ex_score_info2,
           KOFF_reset_len_clock = ex_score_info3,
           RUSH_reset_count = ex_score_info4,
           RUSH_reset_len = ex_score_info5,
           RUSH_reset_len_count = ex_score_info6,
           PASS_reset_count = ex_score_info7,
           PASS_reset_len = ex_score_info8,
           PASS_reset_len_clock = ex_score_info9,
           FGXP_reset_count = ex_score_info10,
           FGXP_reset_len = ex_score_info11,
           FGXP_reset_len_clock = ex_score_info12,
           PUNT_reset_count = ex_score_info13,
           PUNT_reset_len = ex_score_info14,
           PUNT_reset_len_clock = ex_score_info15,
           NOPL_reset_count = ex_score_info16,
           NOPL_reset_len = ex_score_info17,
           NOPL_reset_len_clock = ex_score_info18,
           ONSD_reset_count = ex_score_info19,
           ONSD_reset_len = ex_score_info20,
           ONSD_reset_len_clock = ex_score_info21,
           CONV_reset_count = ex_score_info22,
           CONV_reset_len = ex_score_info23,
           CONV_reset_len_clock = ex_score_info24,
           
           KOFF_half_count = ex_score_info25,
           KOFF_half_len = ex_score_info26,
           KOFF_half_len_clock = ex_score_info27,
           RUSH_half_count = ex_score_info28,
           RUSH_half_len = ex_score_info29,
           RUSH_half_len_count = ex_score_info30,
           PASS_half_count = ex_score_info31,
           PASS_half_len = ex_score_info32,
           PASS_half_len_clock = ex_score_info33,
           FGXP_half_count = ex_score_info34,
           FGXP_half_len = ex_score_info35,
           FGXP_half_len_clock = ex_score_info36,
           PUNT_half_count = ex_score_info37,
           PUNT_half_len = ex_score_info38,
           PUNT_half_len_clock = ex_score_info39,
           NOPL_half_count = ex_score_info40,
           NOPL_half_len = ex_score_info41,
           NOPL_half_len_clock = ex_score_info42,
           ONSD_half_count = ex_score_info43,
           ONSD_half_len = ex_score_info44,
           ONSD_half_len_clock = ex_score_info45,
           CONV_half_count = ex_score_info46,
           CONV_half_len = ex_score_info47,
           CONV_half_len_clock = ex_score_info48
    )
}