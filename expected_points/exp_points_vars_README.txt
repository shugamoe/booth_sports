Season: Season in which the game took place (2000-2016)
Week: Week of the season in which the game took place.
Playoff: Boolean (0,1). Whether the game in question was a playoff game (Week 17+)
Armchair_gid: The game ID variable from armchairanalysis.com
Armchair_pid: The play ID variable from armchairanalysis.com 
Qtr: The qtr in which the play took place
Min: The minute at which the start of the play took place. Note that times during the 2000 season are at times measured inaccurately
Sec: The second at which the start of the play took place. See note for Min
Min_left_in_half: The minutes left in the half at the beginning of the play
Min_left_in_game: The minutes left in the game at the beginning of the play
Pts_Off: The Pts scored by the current offense at the beginning of the play
Pts_Def: The Pts scored by the current defense at the beginning of the play
Off: The current offense "XXX"
Def: The current defense "XXX"
Home: The hometeam
Yfog: Yards from on goal
Armchair_dsq: Drive sequence. The number indicating the current play of the drive
Drive_start: Boolean (0, 1). Whether or not the current play is the start of a drive
Net_Score_to_Half: The net score, from the offensive POV, the occurs from the current play to the end of the half
Net_Score_to_Reset: The net score, from the offensive POV, that occurs from the current play until a FG, TD, or end of the half
Reset_Team_to_Score: Categorical (-1,0,1). Indicates whether the defense scored a FG or TD to initiate a reset (-1), whether the offense did (1), or whether the reset was caused by the end of the half.
Reset_qtr: The quarter in which the reset play (FG, TD, end of half) occurred
Reset_min: The minute in which the reset play (FG, TD, end of half) occurred
Reset_sec: The second in which the reset play (FG, TD, end of half) occurred
Time_to_Reset: The time elapsed from the current play to the reset play (FG, TD, or end of half)
Min_Reset_to_Half: The time remaining in the half when the reset play occurred
Min_Reset_to_GameEnd: The time remaining in the game when the reset play occurred
Follow_Kickoff: Is this 1st and 10 (or goal) the first one following a kickoff? (Safeties not included)
Kickoff_Type: If it did follow a kickoff, did the kickoff come after a FG, TD, or the 1STHF or 2NDHF, or did it not follow a kickoff (None)? 
Offense_Won: Did the offense of this current 1st and 10 (or goal) win the game?
