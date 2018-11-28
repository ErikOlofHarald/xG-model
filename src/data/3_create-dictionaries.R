library(here)
library(dplyr)

events <- readRDS(here("data", "interim", "events.rds"))

# teams
dic_teams <- events %>%
  distinct(team_id, team_name) %>%
  filter(!is.na(team_id)) %>%
  group_by(team_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

# players
dic_players <- events %>%
  distinct(player_id, player_name) %>%
  filter(!is.na(player_id)) %>%
  group_by(player_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Output
saveRDS(dic_teams, here("data", "processed", "dic_teams"))
saveRDS(dic_players, here("data", "processed", "dic_players"))

rm(list = ls())
