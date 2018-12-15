library(dplyr)
library(dbplyr)
library(RSQLite)
library(plotly)
library(data.table)
library(tidyr)
library(d3heatmap)
library(viridis)


setwd("/Users/zhengze/Desktop/Georgetown Course/ANLY 503/Take Home Final Individual")
system("ls")
con <- dbConnect(drv=SQLite(), dbname="database.sqlite")
# list all tables
dbListTables(con)

player       <- tbl_df(dbGetQuery(con,"SELECT * FROM player"))
# player_stats <- tbl_df(dbGetQuery(con,"SELECT * FROM player_stats"))
Match        <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
Team        <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
Country        <- tbl_df(dbGetQuery(con,"SELECT * FROM Country"))
League        <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))

player  <- select(player,player_api_id, player_name) # use player_api_id as key for join
Team    <- select(Team, team_api_id, team_long_name, team_short_name) # use team_api_id as key for join
Country <-select(Country, id, name) %>% rename(country_id = id)  %>% rename(country_name = name)   # use country_id as key for join
League  <- select(League, country_id, name) %>% rename(league_name = name) # use country_id as key for join
Match   <-select(Match, id, country_id, league_id, season, stage, date, match_api_id, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal, home_player_1, home_player_2, home_player_3, home_player_4, home_player_5, home_player_6, home_player_7, home_player_8, home_player_9, home_player_10, home_player_11, away_player_1, away_player_2, away_player_3, away_player_4, away_player_5, away_player_6, away_player_7, away_player_8, away_player_9, away_player_10, away_player_11, goal, shoton, shotoff, foulcommit, card, cross, corner, possession)

PointsDf <-Match %>% 
  select(1:11)  %>% 
  mutate(homePoint = if_else((home_team_goal > away_team_goal),3,if_else((home_team_goal == away_team_goal),1,0))) %>%
  mutate(awayPoint = if_else((home_team_goal > away_team_goal),0,if_else((home_team_goal == away_team_goal),1,3))) 

tableHomeDt <- PointsDf %>% 
  group_by(season, league_id, home_team_api_id) %>%
  summarise(pointsHome = sum(homePoint)) %>%
  ungroup() %>% data.table

keycols = c("season", "league_id", "home_team_api_id" )
setkeyv(tableHomeDt,keycols) 

tableAwayDt <- PointsDf %>% 
  group_by(season, league_id, away_team_api_id) %>%
  summarise(pointsAway = sum(awayPoint)) %>%
  ungroup()  %>% data.table 
keycols = c("season", "league_id", "away_team_api_id" )
setkeyv(tableAwayDt,keycols) 

tableHomeAwayDt <- tableHomeDt[tableAwayDt, nomatch=0] %>%
  mutate(points = pointsHome + pointsAway) %>%
  group_by(season, league_id)  %>%
  mutate(rank = min_rank(desc(points)))

tableLong <- tableHomeAwayDt %>% 
  left_join(League, by = c("league_id" = "country_id")) %>%
  left_join(Team, by = c("home_team_api_id" = "team_api_id")) %>%
  ungroup() %>%
  select(season, league_name, rank, team_long_name, points)

# melt match data to generate df with player names in one column ----------

matchMelt <-melt(Match,id = c(1:11), measure=c(12:33),na.rm = TRUE, value.name = "player_api_id") %>% 
  mutate(team_api_id=ifelse(grepl("home",variable),home_team_api_id,
                            ifelse(grepl("away",variable),away_team_api_id,NA))) %>%  # create team_api_id column based on variable info
  left_join(Team, by = "team_api_id") %>%
  left_join(player, by = "player_api_id") %>% # add club to each player
  left_join(Country, by = "country_id") %>% # add club to each player
  left_join(League, by = "country_id") %>% # add club to each player
  separate(season, into=c("saisonStart","saisonEnd"),sep = "/", convert = TRUE)  # split saison so it integer




tableLong$points <- as.factor(tableLong$points)
p <- ggplot(filter(tableLong, league_name %in% c("England Premier League" )), mapping = aes(x = season, y = team_long_name)) + 
  geom_tile(mapping = aes(fill = points),color="white", size=0.1 ) + facet_grid(league_name~., scales = "free_y") +scale_fill_viridis(discrete=TRUE) + theme(legend.position = "none")  # free y scale to avoid that all clubs are on Y axis in all leagues
ggplotly(p)
