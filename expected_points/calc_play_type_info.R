# Calculating Play Type Functions

PLAY_TYPES <- unique(PLAYS_DF$type)

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