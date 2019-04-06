
library(tidyverse)

#

balls <- readr::read_csv("T20/ball_info.csv")

lineups <- readr::read_csv("T20/team_lineups.csv")

matches <- readr::read_csv("T20/match_info.csv")

players <- readr::read_csv("T20/player_info.csv")

#

data <- balls %>%
  
  inner_join(
    
    matches %>%
      select(date = start_local,
             match_id,
             league = series,
             home = home_team_name,
             away = away_team_name,
             ground = ground_name,
             first_up = bat_first), by = c("match_id")
    
  ) %>%
  
  mutate(bat_order = ifelse(first_up == "Home",
                            ifelse(home == batting_team, "first", "second"),
                            ifelse(away == batting_team, "first", "second"))) %>%
  
  left_join(
    
    players %>%
      select(batter_id = player_id, names, bat_type), by = c("batsman" = "names")
    
  ) %>%
  
  left_join(
    
    players %>%
      select(bowler_id = player_id, names, bowl_type), by = c("bowler" = "names")
    
  ) %>%
  
  mutate(powerplay = ifelse(ball_no < 6, TRUE, FALSE)) %>%
  
  group_by(batsman, match_id) %>%
  
  mutate(first = min(ball_no)) %>%
  
  ungroup() %>%
  
  mutate(first = ifelse(first == ball_no, first, NA)) %>%
  
  group_by(match_id, batting_team) %>%
  
  mutate(batter_order = rank(first, ties.method = "first")) %>%
  
  ungroup() %>%
  
  group_by(batsman, match_id) %>%
  
  mutate(batter_order = min(batter_order, na.rm = T)) %>%
  
  ungroup() %>%
  
  select(-first) %>%
  
  mutate(w = ifelse(is.na(wicket_who), NA, ball_no)) %>%
  
  group_by(match_id, batting_team) %>%
  
  mutate(wicket_order = rank(w, ties.method = "first")) %>%
  
  ungroup() %>%
  
  mutate(wicket_order = ifelse(is.na(wicket_who), NA, wicket_order)) %>%
  
  mutate(w = ifelse(is.na(w), 0, 1)) %>%
  
  arrange(match_id, batting_team, ball_no, ball_id) %>%
  
  group_by(match_id, batting_team) %>%
  
  mutate(rolling_total = cumsum(score_value),
         rolling_wickets = cumsum(w)) %>%
  
  ungroup() %>%
  
  group_by(match_id, batting_team) %>%
  
  mutate(ball_order = rank(ball_no, ties.method = "min")) %>%
  
  ungroup() %>%
  
  group_by(match_id, batting_team) %>%
  
  mutate(team_total = sum(score_value, na.rm = T)) %>%
  
  ungroup() %>%
  
  mutate(away_total = ifelse(away == batting_team, team_total, NA),
         home_total = ifelse(home == batting_team, team_total, NA)) %>%
  
  group_by(match_id) %>%
  
  mutate(oppt_total = ifelse(away == batting_team, home_total, away_total)) %>%
  
  ungroup()
  

#
#
#

batter_stats <- data %>%
  
  group_by(batsman)
  
  