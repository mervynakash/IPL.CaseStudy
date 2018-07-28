set.seed(123)
setwd("E:/Data Visualization/Datasets/IPL Data 2008-16/")

library(xlsx)
library(readxl)
library(dplyr)

ball_by_ball <- read_excel("ball_by_ball.xlsx",sheet = "ball_by_ball")
batsman_scored <- read_excel("batsman_scored.xlsx", sheet = "batsman_scored")
player <- read_excel("player.xlsx", sheet = "player")


#========================= Over all Top N Batsman: ================================#

player2 <- player %>% select(c(player_id, player_name))
ball2 <- ball_by_ball %>% select(c(match_id, over_id, ball_id, innings_no, striker))

newdf <- merge.data.frame(player2, ball2, by.x = "player_id", by.y = "striker", all = T)

newdf2 <- merge.data.frame(newdf, batsman_scored, by = c("match_id","over_id","ball_id","innings_no"), all = T)


# Top 10 Batsman based on runs scored
topruns <- newdf2 %>% group_by(player_name) %>%
  summarise("Total.Runs" = sum(runs_scored, na.rm = T))

top10runs <- topruns %>% arrange(-Total.Runs) %>% head(10)

# Top 10 Batsman based on batting average
numMatch <- newdf2 %>% group_by(player_name, match_id) %>% 
  summarise("Match" = n())
numMatch$Match <- NULL
head(numMatch)

matchPlayed <- numMatch %>% group_by(player_name) %>% 
  summarise(Count = length(unique(match_id)))
batavg <- round(topruns$Total.Runs / matchPlayed$Count,2)

topruns$batavg <- batavg

top10avg <- topruns %>% arrange(-batavg) %>% head(10) %>% select(c(player_name, batavg))

# Top 10 Batsman based on batting strike rate
bowlFaced <- newdf2 %>% group_by(player_name) %>% summarise(BowlsFaced = n())

batstrike <- round(topruns$Total.Runs*100/bowlFaced$BowlsFaced,2)

topruns$batstrike <- batstrike

top10batstrike <- topruns %>% arrange(-batstrike) %>% head(10) %>% 
  select(c(player_name, batstrike))

# Top 10 batsman based on highest score
Score <- newdf2 %>% group_by(player_name, match_id) %>% 
  summarise("Score" = sum(runs_scored, na.rm = T))
highestScore <- Score %>% group_by(player_name) %>% 
  summarise(HighestScore = max(Score))

topruns$highesScore = highestScore$HighestScore

top10highscore <- Score %>% arrange(-Score) %>% head(10) %>% select(-match_id)

write.csv(topruns, "E:/Tableau/IPL/TopRuns.csv")

#======================= Season wise ============================#
season <- read_excel("season.xlsx",sheet = "season")

# Orange Cap Holder
season2 <- season %>% select(orange_cap,season_year)

playerid <- season2$orange_cap

orangeplayer <- player %>% filter(player_id %in% playerid) %>% select(player_id,player_name) %>% unique()

orangecap <- merge.data.frame(orangeplayer, season2, by.x = "player_id", by.y = "orange_cap", all = T)

orangecap <- orangecap %>% arrange(season_year)

#Purple Cap Holder
season3 <- season %>% select(purple_cap,season_year)

playerid <- season3$purple_cap

purpleplayer <- player %>% filter(player_id %in% playerid) %>% select(player_id,player_name) %>% unique()

purplecap <- merge.data.frame(purpleplayer, season3, by.x = "player_id", by.y = "purple_cap", all = T)

purplecap <- purplecap %>% arrange(season_year)

write.csv(orangecap, "E:/Tableau/IPL/OrangeCap.csv")

write.csv(purplecap, "E:/Tableau/IPL/PurpleCap.csv")

#======================= Total Number ===========================#
wicket <- read_excel("wicket_taken.xlsx", sheet = "wicket_taken")
match <- read_excel("match.xlsx", sheet = "match")

match2 <- match %>% select(match_id, season_id)

wickdf <- merge.data.frame(wicket, match2,by = "match_id", all = T)
wickdf <- merge.data.frame(wickdf, season %>% select(season_id, season_year), by = "season_id", all = T)

# Matches

numMatch <- match2 %>% group_by(season_id) %>% summarise(matches = n())
numMatch <- merge.data.frame(numMatch, season %>% select(season_id, season_year), by = "season_id", all = T)
numMatch <- numMatch %>% select(season_year, matches)

# Wickets
wickets <- wickdf %>% group_by(season_year) %>% summarise(Wickets = n())

# 6s
rundf <- merge.data.frame(newdf2, match2, by = "match_id", all = T)
rundf <- merge.data.frame(rundf, season %>% select(season_id, season_year), by = "season_id", all = T)

no6 <- rundf %>% filter(runs_scored == 6) %>% group_by(season_year) %>% summarise(Count6 = n())

# 4s
no4 <- rundf %>% filter(runs_scored == 4) %>% group_by(season_year) %>% 
  summarise(Count4 = n())

totalNumber <- data.frame(numMatch, wickets, no6, no4)
totalNumber <- totalNumber %>% select(-c(season_year.1,season_year.2,season_year.3))

write.csv(totalNumber, "E:/Tableau/IPL/totalNumber.csv")
#======================== Title won by Teams ===========================#
team <- read_excel("team.xlsx", sheet = "team")

match3 <- match %>% select(match_id, season_id, match_winner, match_date)

teamtitle <- merge.data.frame(match3,team, by.y = "team_id", by.x = "match_winner", all = T)
teamtitle <- teamtitle %>% arrange(match_date)

seaid <- unique(teamtitle$season_id)
final <- c()
for(i in seaid){
  final <- c(final, max(which(teamtitle$season_id == i)))
}

winnerteam <- teamtitle[final,]

winnerteam <- merge.data.frame(winnerteam, season %>% select(season_id, season_year), by = "season_id", all = T)

winners <- winnerteam %>% select(season_year, team_name) %>% group_by(team_name,season_year) %>% 
  summarise(Titles = n()) %>% arrange(Titles)

write.csv(winners, "E:/Tableau/IPL/winners.csv")