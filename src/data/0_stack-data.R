library(here)
library(dplyr)

# Gathers all necessary data to construct features

# All event data
events <- bind_rows(
  readRDS(here("data", "raw", "events_2017.rds")),
  readRDS(here("data", "raw", "events_2018.rds")),
  readRDS(here("data", "raw", "events_other_leagues.rds"))
)

# Additional fixture information
fixtures_se <- bind_rows(
  readRDS(here("data", "raw", "fixtures_2017.rds")),
  readRDS(here("data", "raw", "fixtures_2018.rds"))
)
fixtures_se$match_date <- as.POSIXct(fixtures_se$match_date)
fixtures <- fixtures_se %>%
  bind_rows(readRDS(here("data", "raw", "fixtures_other_leagues.rds")))

# Output
saveRDS(events, here("data", "interim", "events.rds"))
saveRDS(fixtures, here("data", "interim", "fixtures.rds"))

rm(list = ls())
