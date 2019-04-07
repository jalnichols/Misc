
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
  
  rename(first_ball = first) %>%
  
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
  
  ungroup() %>%
  
  arrange(match_id, batting_team, ball_id) %>%
  
  mutate(after_no_ball = ifelse(lag(play_type) == "no ball", TRUE, FALSE))
  

#
#
#

batter_stats <- data %>%
  
  mutate(batted_runs = ifelse(play_type %in% c("wide", "no ball", "leg bye"), NA, score_value),
         batted_balls = ifelse(is.na(batted_runs), 0, 1),
         batter_order = ifelse(is.na(first_ball), NA, batter_order),
         field_out = ifelse(is.na(wicket_how), NA, 
                           ifelse(wicket_how %in% c("caught"), 1,
                                  ifelse(wicket_how %in% c("retired not out (hurt)", "not out"), NA, 0)))
         ) %>%
  
  group_by(batsman) %>%
  
  summarize(runs = sum(batted_runs, na.rm = T),
            balls = sum(batted_balls, na.rm = T),
            sixes = sum(score_value == 6, na.rm = T),
            boundaries = sum(score_value %in% c(4,6), na.rm = T),
            dismissals = sum(w, na.rm = T),
            order_appearance = mean(batter_order, na.rm = T),
            singles = sum(batted_runs == 1, na.rm = T),
            field_outs = mean(field_out, na.rm = T),
            powerplays = mean(powerplay, na.rm = T)) %>%
  
  ungroup() %>%
  
  mutate(runs_ball = runs / balls,
         runs_over = runs_ball * 6,
         wickets_ball = dismissals / balls,
         singles_ball = singles / balls,
         boundaries_ball = boundaries / balls,
         sixes_ball = sixes / balls,
         balls_match = 1 / wickets_ball) %>%
  
  select(batsman, runs, balls, sixes, boundaries, dismissals, runs_ball, balls_match, 
         order_appearance, powerplays, runs_over, wickets_ball, field_outs, boundaries_ball, sixes_ball, singles_ball) %>%
  
  filter(balls > 499)

#

km <- kmeans(batter_stats[, 9:15], centers = 5)

pca <- prcomp(batter_stats[, 9:15], scale = TRUE)
  
pcad <- cbind(batter_stats, pca$x)

pca$rotation

summary(pca)