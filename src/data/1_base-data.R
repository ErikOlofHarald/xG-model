library(here)
library(dplyr)

# Gathers all necessary data to construct features

# All event data
events <- readRDS(here("data", "interim", "events.rds"))

# Additional fixture information
fixtures <- readRDS(here("data", "interim", "fixtures.rds"))

# Information of players preferred foot
player_details <-readRDS(here("data", "raw", "player_details.rds"))
pref_foot <- player_details %>% select(id, foot_name) %>%
  rename(player_id = id, pref_foot = foot_name)

# Keep track of home and away team
home_team <- fixtures %>% select(id, team1_id, team2_id) %>%
  rename(home_team_id = team1_id, away_team_id = team2_id, match_id = id)

# season & tournamet
season <- fixtures %>% select(id, season_id, season_name, tournament_name) %>%
  rename(match_id = id, season = season_name, league = tournament_name)

# To keep track of total time in the second half
halfs <- events %>%
  group_by(match_id, half) %>%
  summarise(min_sec = min(second), max_sec = max(second)) %>%
  arrange(match_id, half) %>%
  group_by(match_id) %>%
  mutate(half_start = lag(max_sec), half_start = ifelse(is.na(half_start), 0, half_start)) %>%
  ungroup() %>%
  select(match_id, half, half_start)

# Drop unnecessary variables
drop_vars <- grep("_name$", names(events), value = TRUE)
drop_vars <- setdiff(drop_vars, c("action_name", "body_name"))
drop_vars <- c(drop_vars, "ts")

events_clean <- events %>%
  select(-one_of(drop_vars)) %>%
  # Add home and away team
  inner_join(home_team, by = "match_id") %>%
  # Add player preferred foot 
  left_join(pref_foot, by = "player_id") %>%
  # Second when first half ended
  inner_join(halfs, by = c("match_id", "half")) %>%
  # Season & league name
  inner_join(season, by = "match_id") %>% 
  # sort data
  arrange(match_id, half, second, id) %>%
  # remove unnecessary rows
  filter(
    !(action_id %in% c(2040, 2060)),              # Loss of ball/ball recovery
    !(action_id %in% c(3020, 3030, 3080, 3100)),  # Cards
    action_id != 10000,                           # Bad ball control
    floor(action_id / 1000) != 13,                # Goalkeeper actions
    floor(action_id / 1000) != 14,                # Substitution
    floor(action_id / 1000) != 15,                # Formations
    floor(action_id / 1000) != 16,                # Line-up positions
    floor(action_id / 1000) != 18,                # Match start, half time etc.
    floor(action_id / 1000) != 19,                # Misstakes
    floor(action_id / 1000) != 21,                # Possession
    action_id != 22000,                           # Ball receiving
    floor(action_id / 1000) != 23,                # Playing in goal scoring attack
    floor(action_id / 1000) != 25,                # Average positions
    !(action_id %in% c(28010, 28011, 28012)),     # Diagonal passes (labels on main passes)
    action_id != 28020,                           # Key interception
    action_id != 28030,                           # Pass behind player. Rarely filled in
    action_id != 28070,                           # Bicycle kick. Rarely filled in
    !(action_id %in% c(28050, 28060, 28080)),     # Players that created offside trap etc.
    action_id != 28100,                           # At high speed - Rarely related to shots and passes
    action_id != 28110,                           # Pass into offside
    floor(action_id / 1000) != 29,                # Points on the video
    action_id < 30000                             # action_id > 30000 not needed
  )

# Set all penalty kicks to the same coordinates as some are of and they should all be taken from the same spot
idx <- which(events_clean$standart_id == 6)
events_clean$pos_x[idx] <- (105 - 10.97)
events_clean$pos_y[idx] <- 34

# Indirect free kicks cannot be shots on goal, violation of rules
idx <- which(events_clean$standart_id == 3 & floor(events_clean$action_id / 1000) %in% c(4, 8))
events_clean$standart_id[idx] <- 4L

# Direct free kicks inside penalty area is a violation of rules, remove
idx <- with(events_clean, which(standart_id == 4 & pos_x > (105 - 16.5) & (pos_y > (34 - 16.5) & pos_y < (34 + 16.5))))
events_clean <- events_clean[-idx, ]

# Remove duplicates
events_clean <- events_clean %>%
  group_by(match_id, player_id, half, second, action_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

saveRDS(events_clean, here("data", "interim", "base-data.rds"))

rm(list = ls())
