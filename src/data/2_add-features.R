library(here)
library(dplyr)
library(tidyr)


# Read base data ----------------------------------------------------------

base_data <- readRDS(here("data", "interim", "base-data.rds"))


# First couple of features from base data ---------------------------------

## Utility functions

is_accurate_pass <- function(id) {
  id %in% c(1011, 1021, 1031, 1040, 1050, 1061, 1070)
}

is_inaccurate_pass <- function(id) {
  id %in% c(1012, 1022, 1032, 1062)
}

is_pass <- function(id) {
  is_accurate_pass(id) | is_inaccurate_pass(id)
}

is_shot <- function(id) {
  id %in% c(4010, 4020, 4030, 4040, 4050, 8010)
}

preceeded_by_dribble <- function(id, player, position) {
  # Base case: No
  v <- rep(0L, length(id))
  # Dribble
  idx <- which(id == 2051)
  # Succeeding event
  lead_idx <- idx + 1L
  # Events preceeded by dibble
  v[lead_idx] <- as.integer(player[lead_idx] == player[idx])
  # Events preceeded by dribble of GK
  idx <- which(v == 1L)
  v[idx] <- v[idx] + as.integer(position[idx - 1L] == 31)
  y <- rep("No", length(id))
  y[v == 1] <- "Player"
  y[v == 2] <- "Goalkeeper"
  
  y
}

preceeded_by_opening <- function(id, player) {
  # Base case: No
  v <- rep(0L, length(id))
  # Opening
  idx <- which(id == 28090)
  v[idx + 1] <- as.integer(player[idx + 1] == player[idx])
  
  v
}

ball_movement <- function(y1, y2) {
  # Distance within rows
  d_row <- abs(y1 - y2)
  d_row[is.na(d_row)] <- 0
  d_row <- cumsum(d_row)
  
  # Distance between rows
  d_lag <- abs(y1 - lag(y2))
  d_lag[is.na(d_lag)] <- 0
  d_lag <- cumsum(d_lag)
  
  # Within up to previous row + total distance between rows
  d_tot <- d_lag + lag(d_row)
  # First row in each group  will be NA
  d_tot[1] <- 0
  
  
  d_tot
}

is_cross <- function(id, player, x, y) {
  # Base case: No
  v <- rep(0L, length(id))
  # Crosses
  idx <- which(floor(id / 1000) == 26 | floor(id / 10) == 2804)
  v[idx - 1] <- as.integer(player[idx - 1] == player[idx] & x[idx - 1] == x[idx] & y[idx - 1] == y[idx])
  v[idx + 1] <- as.integer(player[idx + 1] == player[idx] & x[idx + 1] == x[idx] & y[idx + 1] == y[idx])
  v[!is_pass(id)] <- 0L
  
  v
}

pass_type <- function(id) {
  v <- rep(NA_character_, length(id))
  v[id %in% c(1021, 1022)] <- "Non attacking"
  # Assuming assists (1040) are attacking
  v[id %in% c(1011, 1012, 1040)] <- "Attacking"
  v[id %in% c(1061, 1062, 1070)] <- "Extra attacking"
  v[id %in% c(1031, 1032, 1050)] <- "Key"
  
  v
}

set_body_name <- function(x) {
  v <- rep(NA_character_, length(x))
  v[x %in% c(1, 2)] <- "Foot"
  v[x == 3] <- "Header"
  v[x == 4] <- "Body"
  v[x == 5] <- "Hand"
  
  v
}

set_attack_type <- function(x) {
  v <- rep(NA_character_, length(x))
  v[x == 1] <- "Positional attack"
  v[x == 2] <- "Counter attack"
  v[x == 3] <- "Corner attack"
  v[x == 4] <- "Free-kick attack"
  v[x == 5] <- "Penalty attack"
  v[x == 6] <- "Throw-in attack"
  
  v
}

set_standart <- function(x) {
  v <- rep(NA_character_, length(x))
  v[x == 1] <- "Open play"
  v[x == 2] <- "Throw-in"
  v[x == 3] <- "Indirect free kick"
  v[x == 4] <- "Free kick"
  v[x == 5] <- "Corner"
  v[x == 6] <- "Penalty"
  v[x == 7] <- "Interruption"
  
  v
}

## Create features

features <- base_data %>%
  # Add indicators
  mutate(
    dribble = preceeded_by_dribble(action_id, player_id, opponent_position_id),
    opening = preceeded_by_opening(action_id, player_id),
    cross = is_cross(action_id, player_id, pos_x, pos_y),
    pass_shot_from = set_body_name(body_id),
    attack_type = set_attack_type(attack_type_id),
    standart = set_standart(standart_id),
    match_second = half_start + second
  ) %>%
  # Added as indicator on event instead
  filter(
    action_id != 2051,
    action_id != 28090,
    floor(action_id / 1000) != 26,
    floor(action_id / 10) != 2804
  )

# Add possession_number to missing observations
idx <- which(is.na(features$possession_number))
tmp <- features[idx, ] %>%
  mutate(
    row_id = idx,
    ok = (row_id == (lag(row_id) + 1)),
    ok = ok & (team_id == lag(team_id)),
    ok = ifelse(is.na(ok), FALSE, ok),
    possession_number = cumsum(!ok) + 10000
  ) %>% select(-row_id, -ok)

features <- features %>%
  filter(!is.na(possession_number)) %>%
  bind_rows(tmp) %>%
  arrange(match_id, half, second, id) %>%
  group_by(match_id, half, possession_number) %>%
  mutate(
    # Home team?
    home_team = as.integer(team_id == home_team_id),
    # Nbr of passes within possession
    n_pass = cumsum(is_pass(action_id)),
    # Nbr of accurate passes
    n_accurate_pass = cumsum(is_accurate_pass(action_id)),
    # Shot nbr in possession
    shot_nbr = cumsum(is_shot(action_id)),
    # Nbr of shots in possession
    n_shots = max(shot_nbr),
    # Start coordinates of possession
    pos_start_x = first(pos_x),
    pos_start_y = first(pos_y),
    zone_start_id = first(zone_id),
    # Start second and elapsed time since start of possession
    second_start = first(second),
    ds = second - second_start,
    # Total vertical ball movement from starting x coordinate
    v_distance = ball_movement(pos_x, pos_dest_x),
    # Total horizontal ball movement from starting y coordinatte
    h_distance = ball_movement(pos_y, pos_dest_y),
    # Vertical movement of the ball from starting x coordinate
    attack_distance = pos_x - pos_start_x,
    # Speed vertical ball movement
    v_speed = ifelse(ds == 0, NA, v_distance / ds),
    # Speed horizontal ball movement
    h_speed = ifelse(ds == 0, NA, h_distance / ds),
    # Speed of attack
    attack_speed = ifelse(ds == 0, NA, attack_distance / ds)
  ) %>%
  # Add row identifier
  ungroup() %>%
  mutate(row_id = row_number())



# Keep shots --------------------------------------------------------------

shots <- features %>% filter(is_shot(action_id)) %>%
  select(-cross) %>%
  rename(shot_from = pass_shot_from) %>%
  mutate(goal = as.integer(action_id == 8010))



# Add additional features that need more computation ----------------------

### Related to shots

## Shot distance and angle

s <- 7.32
y1 <- (68 + s) / 2
y2 <- (68 - s) / 2
# law_of_cosines c^2 = a^2 + b^2 - 2*a*b*cos(C)
shots <- shots %>%
  mutate(
    a = sqrt( (105 - pos_x) ^ 2 + (y1 - pos_y) ^ 2),
    b = sqrt( (105 - pos_x) ^ 2 + (y2 - pos_y) ^ 2),
    shot_dist = ifelse(pos_y > y2 & pos_y < y1, 105 - pos_x, pmin(a, b)),
    shot_angle = acos(x = (a ^ 2 + b ^ 2 - s ^ 2) / (2 * a * b))
  ) %>%
  select(-a, -b)

rm(s, y1, y2)

## Correct foot

correct_foot <- function(score_foot, pref_foot) {
  # Adjust strings so they match
  foots <- c("Right", "Left")
  idx <- which(pref_foot %in% foots)
  pref_foot[idx] <- paste(pref_foot[idx], "foot")
  foots <- paste(foots, "foot")
  # Base case
  v <- rep("No", length(score_foot))
  # Correct foot?
  v[score_foot == pref_foot] <- "Yes"
  v[(score_foot %in% foots) & pref_foot == "Both"] <- "Yes"
  v[(score_foot %in% foots) & is.na(pref_foot)] <- "Unknown"
  # Other body part?
  v[!(score_foot %in% foots)] <- "Other"
  
  v
}

shots <- shots %>% mutate(correct_foot = correct_foot(body_name, pref_foot))

rm(correct_foot)

## Shot after reflection

shot_rebound <- function(id) {
  v <- rep(NA, length(id))
  v[id == 4010] <- "On target"
  v[id == 4030] <- "Wood"
  v[id %in% c(4040, 4050)] <- "Blocked"
  
  v
}

df <- features %>%
  # Remove challenges, shots blocked and free ball pickup as they can be
  # registered between shots
  filter(!(action_id %in% c(2010, 2020, 6010, 7000))) %>%
  mutate(lag_id = lag(action_id)) %>%
  filter(is_shot(action_id) & team_id == lag(team_id)) %>%
  select(row_id, lag_id) %>%
  mutate(shot_after_rebound = shot_rebound(lag_id)) %>%
  select(-lag_id)

shots <- shots %>% left_join(df, by = "row_id")

rm(shot_rebound, df)

## Shot after challenge

df <- features %>%
  mutate(
    lag_id = lag(action_id),
    shot_after_challenge = lag(action_name),
    ok = is_shot(action_id),
    ok = ok & lag_id %in% c(2010, 2020),
    ok = ok & team_id == lag(team_id),
    ok = ok & player_id == lag(player_id)
  ) %>%
  filter(ok) %>%
  select(row_id, shot_after_challenge)

shots <- shots %>% left_join(df, by = "row_id")

rm(df)


### Related to preceeding passes

## Preceeding 2 passes

drop_ids <- c(2010, 2020)  # Challenges don't interupt the sequence

passes_before_shot <- features %>%
  filter(!(action_id %in% drop_ids)) %>%
  mutate(
    ok = is_shot(action_id) | is_pass(action_id),
    # ok = action_id %in% c(accurate, inaccurate, shot_ids),
    ok = ok & (player_id == lag(opponent_id)),
    ok = ok & (team_id == lag(team_id)),
    ok = ok & (possession_number == lag(possession_number)),
    ok = ifelse(is.na(ok), FALSE, ok),
    pass_seq = cumsum(!ok)
  ) %>%
  group_by(pass_seq) %>%
  filter(sum(is_shot(action_id)) > 0) %>%
  arrange(desc(second)) %>%
  filter(row_number() <= 3) %>%
  mutate(row_id = first(row_id)) %>%
  filter(!is_shot(action_id)) %>%
  mutate(pass_nbr = row_number()) %>%
  ungroup() %>%
  mutate(
    # Length (already in dataset but seems weird)
    len = sqrt( (pos_dest_x - pos_x) ^ 2 + (pos_dest_y - pos_y) ^ 2),
    # Direction (already in dataset but is definitely weird)
    # 1 is forward, -1 ir backward and 0 is sideways pass (right or left)
    dir = ifelse(len == 0, NA, (pos_dest_x - pos_x) / len),
    # Pass type (including assist marked as attacking, key etc.)
    pass_type = pass_type(action_id)
  ) %>%
  select(row_id, pass_nbr, standart, len, dir, pos_x, pos_y, zone_id,
    pass_type, dribble, opening, pass_shot_from, cross) %>%
  rename(from = pass_shot_from) %>%
  gather(variable, value, -row_id, -pass_nbr) %>%
  mutate(new_var = paste("pass", pass_nbr, variable, sep = "_")) %>%
  select(-pass_nbr, -variable) %>%
  spread(new_var, value)

passes_before_shot <- passes_before_shot %>%
  mutate_at(.vars = grep("_dir|_len|_pos_x|_pos_y", names(.), value = T), as.numeric) %>%
  mutate_at(.vars = grep("_opening|_zone_id|_cross", names(.), value = T), as.integer)


shots <- shots %>% left_join(passes_before_shot, by = "row_id")

rm(drop_ids, passes_before_shot)


### Other features

## Game state

change_team <- function(action, team, team1, team2) {
  v <- team
  idx <- which(action == 8020)
  v[idx] <- ifelse(team[idx] == team1[idx], team2[idx], team1[idx])
  
  v
}

df <- features %>%
  filter(is_shot(action_id) | action_id == 8020) %>%
  select(row_id, match_id, team_id, home_team_id, away_team_id, action_id) %>%
  mutate(
    # If own goal, change team id so the goal is registered on the correct team
    team_id = change_team(action_id, team_id, home_team_id, away_team_id),
    # home goals
    hg = ifelse(action_id %in% c(8010, 8020) & team_id == home_team_id, 1L, 0L),
    # away goals
    ag = ifelse(action_id %in% c(8010, 8020) & team_id == away_team_id, 1L, 0L)   
  ) %>%
  group_by(match_id) %>% 
  mutate(
    hg = cumsum(hg),
    ag = cumsum(ag),
    gd = ifelse(team_id == home_team_id, hg - ag, ag - hg),
    # Game state seen from the scoring teams perspective
    game_state = ifelse(action_id %in% c(8010, 8020), gd - 1, gd)
  ) %>%
  ungroup() %>%
  select(row_id, game_state)

shots <- shots %>% inner_join(df, by = "row_id")

rm(df, change_team)


# Drop redundant variables ------------------------------------------------

keeps <- c(
  # Not features
  "match_id", "league", "season", "action_id", "action_name", "second", "team_id",
  "player_id", "position_id", "opponent_team_id", "opponent_id",
  "opponent_position_id", "attack_number", "assistant_id",
  # Features related to match
  "half", "match_second", "home_team", "game_state",
  # Features related to attack
  "attack_type", "n_pass", "n_accurate_pass", "n_shots", "pos_start_x",
  "pos_start_y", "zone_start_id", "second_start", "ds", "v_distance",
  "h_distance", "attack_distance", "v_speed", "h_speed", "attack_speed",
  # Features related to shot
  "shot_nbr", "pos_x", "pos_y", "zone_id", "shot_from", "correct_foot",
  "dribble", "opening", "standart", "shot_dist", "shot_angle",
  "shot_after_rebound", "shot_after_challenge",
  # Features related to preceeding passes
  "pass_1_dir", "pass_1_dribble", "pass_1_from", "pass_1_len", "pass_1_opening",
  "pass_1_pass_type", "pass_1_pos_x", "pass_1_pos_y", "pass_1_standart",
  "pass_1_zone_id", "pass_1_cross",
  "pass_2_dir", "pass_2_dribble", "pass_2_from", "pass_2_len", "pass_2_opening",
  "pass_2_pass_type", "pass_2_pos_x", "pass_2_pos_y", "pass_2_standart",
  "pass_2_zone_id", "pass_2_cross",
  # Target
  "goal"
  )

shots_features <- shots[keeps] %>%
  mutate_at(vars(half, game_state, n_shots, zone_start_id, zone_id), as.integer)

# Save output -------------------------------------------------------------

saveRDS(shots_features, here("data", "processed", "model-data.rds"))

rm(list = ls())
