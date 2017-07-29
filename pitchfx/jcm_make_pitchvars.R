# Creates the variables/data to be used in jcm_clustered_replication.Rmd

add_vars_to_dataset <- function(year){
  require(tidyverse)
  
  data <- read_csv(sprintf("pitchfx/pitchfx_%d.csv", year)) %>%
    mutate(SAC_AB = ifelse(event %in% c("Sac Fly",
                                        "Sac Bunt",
                                        "Sac Fly DP",
                                        "Sacrifice Bunt DP"), 1, 0),
           Fringe_AB = ifelse(event %in% c("Runner Out",
                                           "Batter interference",
                                           "Catcher Interference"), 1, 0),
           BB_AB = ifelse(event %in% c("Hit By Pitch",
                                       "Walk", 
                                       "Intent Walk"), 1, 0),
           Total_Bases_AB = ifelse(event == "Single", 1,
                            ifelse(event %in% c("Double", "Fan Interference"), 2,
                            ifelse(event == "Triple", 3, 
                            ifelse(event == "Home Run", 4, 0)))),
           Total_Bases_BB_AB = Total_Bases_AB + BB_AB,
           
    )
}