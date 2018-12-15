library(ggplot2) # Data visualization
library(ggrepel) # Labels on plots
library(dplyr)
library(RSQLite)
library(devtools)

#library(data.table)
#library(plotly)
library(cowplot)


setwd("/Users/zhengze/Desktop/Georgetown Course/ANLY 503/Take Home Final Individual")
system("ls")
con <- dbConnect(drv=SQLite(), dbname="database.sqlite")
# list all tables
dbListTables(con)

league <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
team   <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
match  <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))

# Restrict the objects to columns needed for the analysis
league <- select(league, id, name, country_id) %>% rename(league_id = id, league_name = name)
team   <- select(team, team_api_id, team_long_name, team_short_name)
match  <- select(match, league_id, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal)

# Calculate the points scored for each match -> match_points
match_points <- match %>% 
  mutate(home_team_points = if_else((home_team_goal > away_team_goal),3,if_else((home_team_goal == away_team_goal),1,0))) %>%
  mutate(away_team_points = if_else((home_team_goal > away_team_goal),0,if_else((home_team_goal == away_team_goal),1,3))) 

# Calculate average home team points per game
home_points <- match_points %>%
  select(league_id, team_api_id = home_team_api_id, home_team_points) %>%
  group_by(league_id, team_api_id) %>%
  summarize(avg_home_ppg = mean(home_team_points))

# Calculate average away team points per game
away_points <- match_points %>%
  select(league_id, team_api_id = away_team_api_id, away_team_points) %>%
  group_by(league_id, team_api_id) %>%
  summarize(avg_away_ppg = mean(away_team_points))

# Combine the data for the average home and away team points per game
all_points <- left_join(home_points, away_points, by = c("league_id", "team_api_id"))

# Add the average points scored per game (home and away)
# It's OK to take a simple average of the home and away averages as each team plays the same number of home and away games
all_points <- all_points %>%
  mutate(avg_ppg = (avg_home_ppg + avg_away_ppg)/2)

# Add the details of the league and the team to each record so that their names can be displayed on the plots
all_points <- left_join(all_points, league, by = "league_id")
all_points <- left_join(all_points, team, by = "team_api_id")


### First one
first_ggplot<-ggplot(all_points, aes(x = avg_home_ppg, y = avg_away_ppg, color = league_name)) + 
  geom_point() +
  xlim(0,3) +
  ylim(0,3) +
  labs(title = "Comparison of Average Points Per Game (PPG) for each Team\nin Home and Away Games",
       x = "Average Home PPG",
       y = "Average Away PPG") +
  theme(plot.title = element_text(hjust = 0.5))
first_ggplot
ggsave("Comparison of Average Points Per Game for each Team.png", width = 8, height = 6, dpi = 200)
#save_plot("Comparison of Average Points Per Game for each Team.jpg", first_ggplot)


### Second one
# Calculate the ratio of points won at home to points won away
home_away <- all_points %>%
  mutate(home_away_ratio = avg_home_ppg / avg_away_ppg) %>%
  select(team_long_name, league_id, league_name, avg_ppg, home_away_ratio) %>%
  as.data.frame()
##########
second_ggplot<-ggplot(home_away, aes(x = league_name, y = home_away_ratio)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Comparison of Home to Away Points Ratio for each League",
       x = "Leage Name",
       y = "Home to Away Points Ratio") +
  theme(plot.title = element_text(hjust = 0.5))

second_ggplot
ggsave("Comparison of Home to Away Points Ratio for each League.png", width = 8, height = 6, dpi = 200)

### Third one
zzz<-c(1729,7809,10257,21518)

league_points <- all_points %>% filter(league_id == zzz[1])
chart_title <- paste("Comparison of Average(PPG) for each Team\nin Home and Away Games \nfor", league_points$league_name)
p1 <- ggplot(league_points, aes(x = avg_home_ppg, y = avg_away_ppg)) + 
    geom_point() +
    xlim(0,3) +
    ylim(0,3) +
    labs(title = chart_title,
         x = "Average Home PPG",
         y = "Average Away PPG") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_smooth(method = lm) +
    geom_text_repel(aes(label = team_long_name), size = 2)

league_points <- all_points %>% filter(league_id == zzz[2])
chart_title <- paste("Comparison of Average(PPG) for each Team\nin Home and Away Games \nfor", league_points$league_name)
p2 <- ggplot(league_points, aes(x = avg_home_ppg, y = avg_away_ppg)) + 
  geom_point() +
  xlim(0,3) +
  ylim(0,3) +
  labs(title = chart_title,
       x = "Average Home PPG",
       y = "Average Away PPG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = lm) +
  geom_text_repel(aes(label = team_long_name), size = 2)


combine_gg <- plot_grid(p1,p2)

save_plot("gg_subplot.png", combine_gg, ncol = 2)