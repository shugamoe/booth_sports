# Testing functions to debug

view_pid_slice <- function(df, pid_center, slice_size = 10){
  require(dplyr)
  View(
    df %>%
      filter(pid <= pid_center + slice_size &
               pid >= pid_center - slice_size)
  )
}

view_gid <- function(df, game_id){
  require(dplyr)
  View(
    df %>%
      filter(gid == game_id)
  )
}