# Game Tracker Creation

extract_game_plays_df <- function(plays_df, game_id){
  (plays_df %>% filter(gid == game_id))
}

# Function to make calculations more efficient; stores dataframes for each game
GAME_TRACKER <- sort(unique(PLAYS_DF$gid)) %T>%
 print("Making Game Tracker", null = .) %>%
 map(~ extract_game_plays_df(plays_df = PLAYS_DF,  game_id = .))