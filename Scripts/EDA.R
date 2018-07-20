#read in required libraries
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(googlesheets))
suppressMessages(library(gridExtra))
suppressMessages(library(scales))

#read in the combined_goals, combined_fouls, and combined_corners datasets
combined_goals <- read_csv(here("English_Soccer_project","Data", "combined_goals.csv"))
combined_fouls <- read_csv(here("English_Soccer_project","Data", "combined_fouls.csv"))
combined_corners <- read_csv(here("English_Soccer_project","Data", "combined_corners.csv"))
combined_shots <- read_csv(here("English_Soccer_project","Data","combined_shots.csv"))

#insert a "/" character into the seasons_rep column
combined_goals$seasons_rep <- paste(substr(combined_goals$seasons_rep,1,2),"/",substr(combined_goals$seasons_rep,3,4),sep="")
combined_fouls$seasons_rep <- paste(substr(combined_fouls$seasons_rep,1,2),"/",substr(combined_fouls$seasons_rep,3,4),sep="")
combined_corners$seasons_rep <- paste(substr(combined_corners$seasons_rep,1,2),"/",substr(combined_corners$seasons_rep,3,4),sep="")

#combined_all the datasets into a single dataset just called 'combined'
combined <- cbind(combined_goals,combined_fouls[,4:8],combined_corners[,4:8])

#range of Goals For and range of Goals against
rng_For <- range(combined_goals$FTotal)
rng_Against <- range(combined_goals$ATotal)

#plot number of instances of each team was in the Premier League from 2008 - 2018
combined_goals %>%
group_by(Team) %>%
summarize(Total = n()) %>%
arrange(., desc(Total)) %>%
ggplot(data = ., aes(x = reorder(Team,-Total), y = Total, fill = Total)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  xlab("Team") +
  scale_y_continuous(breaks = seq(0,10,1)) +
  scale_fill_gradient(low = "red", high = "green") +
  ylab("Total seasons in Premier League from 2008 - 2018")

#ggsave("All_PL_Teams0818.png", path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#create time series plot of table position by season for each team
combined_goals %>%
  group_by(Team) %>%
  summarize(tot_seasons = n()) %>%
  left_join(., combined_goals, by = c("Team", "Team")) %>%
  arrange(., desc(tot_seasons)) %>%
  mutate(year = as.numeric(paste("20",substr(seasons_rep,4,5),sep = ""))) %>%
  mutate(facTeam = factor(Team, levels = unique(Team[order(-tot_seasons)], ordered = TRUE))) %>%
  ggplot(., aes(x = year, y = Position, color = Team)) +
  geom_line(lwd = 0.75)+
  geom_point(color = "black") +
  geom_hline(yintercept = 18, lwd = 2, lty = 1, color = "white", alpha = 0.5) +
  ylab("Position in table") +
  ylim(20,1)+
  xlab("Year") +
  guides(color = FALSE) +
  facet_wrap(~facTeam) +
  labs(title = "Position in table for last 10 years",subtitle = "Only includes those teams that were in the Premier League 8 of the last 10 seasons")

#look at distribution of goals for and goals against for each year
For_overall <- ggplot(combined_goals, aes(x = seasons_rep, y = FTotal, fill = seasons_rep)) +
               geom_boxplot() +
               geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
               ylab("Number of goals") +
               xlab("Season") +
               ylim(rng_Against[1] - 0.10, rng_For[2] + 0.10) +
               labs(title = "Annual distribution of total team goals scored") +
               guides(fill = FALSE)

Against_overall <- ggplot(combined_goals, aes(x = seasons_rep, y = ATotal, fill = seasons_rep)) +
                   geom_boxplot() +
                   geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
                   ylab("Number of goals") +
                   xlab("Season") +
                   ylim(rng_Against[1] - 0.10, rng_For[2] + 0.10) +
                   labs(title = "Annual distribution of total team goals allowed") +
                   guides(fill = FALSE)

#plot annual total goals for and total goals against across all teams
g <- grid.arrange(For_overall,Against_overall, nrow = 1, ncol =2)

#when saving a plot generated using grid.arrange you have to assign the 
#plot to an object and pass that object to #ggsave
#ggsave("Total_team_goals_scored_vs_allowed_ALL_TEAMS.png", g, path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#look at goals scored by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less goals through time
For_top6 <- combined_goals %>%
filter(Position <= 6) %>%
ggplot(data = ., aes(x = seasons_rep, y = FTotal, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals") +
  xlab("Season") +
  ylim(rng_Against[1] - 0.10, rng_For[2] + 0.10) +
  labs(title = "Annual distribution of total team goals scored", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)

#look at goals scored by teams ranked 7 - 17 at end of season
For_Mids <- combined_goals %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = FTotal, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals") +
  xlab("Season") + 
  ylim(rng_Against[1] - 0.10, rng_For[2] + 0.10) +
  labs(title = "Annual distribution of total team goals scored", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at goals scored by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less goals through time
For_bottom3 <- combined_goals %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = FTotal, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals") +
  xlab("Season") +
  ylim(rng_Against[1] - 0.10, rng_For[2] + 0.10) +
  labs(title = "Annual distribution of total team goals scored", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE)

#plot annual total goals for broken out by top 6 and bottom 3 teams
g <- grid.arrange(For_top6, For_Mids, For_bottom3, nrow = 1, ncol = 3)

#ggsave("Total_team_goals_scored.png", g, path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#look at goals allowed by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less goals through time
Against_top6 <- combined_goals %>%
  filter(Position <= 6) %>%
  ggplot(data = ., aes(x = seasons_rep, y = ATotal, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals") +
  xlab("Season") +
  ylim(rng_Against[1] - 0.10, rng_Against[2] + 0.10) +
  labs(title = "Annual distribution of total team goals allowed", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)


#look at goals scored by teams ranked 6 - 17 at end of season
Against_Mids <- combined_goals %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = ATotal, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals") +
  xlab("Season") + 
  ylim(rng_Against[1] - 0.10, rng_Against[2] + 0.10) +
  labs(title = "Annual distribution of total team goals allowed", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at goals allowed by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less goals through time
Against_bottom3 <- combined_goals %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = ATotal, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals") +
  xlab("Season") + 
  ylim(rng_Against[1] - 0.10, rng_Against[2] + 0.10) +
  labs(title = "Annual distribution of total team goals allowed", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE)

#plot annual total goals against broken out by top 6 and bottom 3 teams
g <- grid.arrange(Against_top6, Against_Mids, Against_bottom3, nrow = 1, ncol = 3)

#ggsave("Total_team_goals_allowed.png", g, path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#####################################################################
#Create the same series of graphs created above but instead of
#looking simply at the total number of goals for/against, look 
#at the number for/against per game

#first need to change the column headings FPer Game and APer Game to include no
#blank space
colnames(combined_goals)[grep(".*Per",colnames(combined_goals))] <- gsub("Per ", "Per_",colnames(combined_goals)[grep(".*Per",colnames(combined_goals))])

#range of FPer_Game and range of APer_Game
rng_FPer <- range(combined_goals$FPer_Game)
rng_APer <- range(combined_goals$APer_Game)

#look at distribution of goals for and goals against for each year
ForPer_overall <- ggplot(combined_goals, aes(x = seasons_rep, y = FPer_Game, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals per game") +
  xlab("Season") +
  ylim(rng_APer[1] - 0.10, rng_FPer[2] + 0.10) +
  labs(title = "Annual distribution of total team goals scored") +
  guides(fill = FALSE)

AgainstPer_overall <- ggplot(combined_goals, aes(x = seasons_rep, y = APer_Game, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals per game") +
  xlab("Season") +
  ylim(rng_APer[1] - 0.10, rng_FPer[2] + 0.10) +
  labs(title = "Annual distribution of total team goals allowed") +
  guides(fill = FALSE)

#plot annual total goals for and total goals against across all teams
g <- grid.arrange(ForPer_overall,AgainstPer_overall, nrow = 1, ncol =2)

#ggsave("Total_team_goals_scored_vs_allowedPerGame_ALL_TEAMS.png", g, path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#look at goals scored by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less goals through time
ForPer_top6 <- combined_goals %>%
  filter(Position <= 6) %>%
  ggplot(data = ., aes(x = seasons_rep, y = FPer_Game, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals per game") +
  xlab("Season") +
  ylim(rng_APer[1] - 0.10, rng_FPer[2] + 0.10) +
  labs(title = "Annual distribution of total team goals scored", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)

#look at goals scored by teams ranked 7 - 17 at end of season
ForPer_Mids <- combined_goals %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = FPer_Game, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals per game") +
  xlab("Season") + 
  ylim(rng_APer[1] - 0.10, rng_FPer[2] + 0.10) +
  labs(title = "Annual distribution of total team goals scored", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at goals scored by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less goals through time
ForPer_bottom3 <- combined_goals %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = FPer_Game, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals per game") +
  xlab("Season") +
  ylim(rng_APer[1] - 0.10, rng_FPer[2] + 0.10) +
  labs(title = "Annual distribution of total team goals scored", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE)

#plot annual total goals for broken out by top 6 and bottom 3 teams
g <- grid.arrange(ForPer_top6, ForPer_Mids, ForPer_bottom3, nrow = 1, ncol = 3)

#ggsave("Total_team_goals_scoredPerGame.png", g, path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#look at goals allowed by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less goals through time
AgainstPer_top6 <- combined_goals %>%
  filter(Position <= 6) %>%
  ggplot(data = ., aes(x = seasons_rep, y = APer_Game, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals per game") +
  xlab("Season") +
  ylim(rng_APer[1] - 0.10, rng_APer[2] + 0.10) +
  labs(title = "Annual distribution of total team goals allowed", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)


#look at goals scored by teams ranked 6 - 17 at end of season
AgainstPer_Mids <- combined_goals %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = APer_Game, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals per game") +
  xlab("Season") + 
  ylim(rng_APer[1] - 0.10, rng_APer[2] + 0.10) +
  labs(title = "Annual distribution of total team goals allowed", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at goals allowed by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less goals through time
AgainstPer_bottom3 <- combined_goals %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = APer_Game, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals per game") +
  xlab("Season") + 
  ylim(rng_APer[1] - 0.10, rng_APer[2] + 0.10) +
  labs(title = "Annual distribution of total team goals allowed", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE)

#plot annual total goals against broken out by top 6 and bottom 3 teams
g <- grid.arrange(AgainstPer_top6, AgainstPer_Mids, AgainstPer_bottom3, nrow = 1, ncol = 3)

#ggsave("Total_team_goals_allowedPerGame.png", g, path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#look at number of top four finishes by team; needs work haven't figured out code yet
combined_goals %>%
  group_by(Team) %>%
  count(Position) %>%
  filter(Position <= 4) %>%
  ggplot(., aes(x = Team, y = n, fill = as.factor(Position))) +
  geom_col(position = "dodge") +
  ylab("Number of times a team has finished in one of the top 4 positions") +
  labs(fill = "Final team position") +
  scale_fill_manual(labels = c("1st", "2nd", "3rd", "4th"), values = hue_pal()(4))

ggsave("Number of Top 4 finishes by club.png", path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#look at number of bottom 3 finishes by team
combined_goals %>%
  group_by(Team) %>%
  count(Position) %>%
  filter(Position >= 18) %>%
  ggplot(., aes(x = Team, y = n, fill = as.factor(Position))) +
  geom_col(position = "dodge") +
  ylab("Number of times a team has finished in one of the bottom 3 positions") +
  scale_y_continuous(breaks = seq(0,2,1)) +
  labs(fill = "Final team position") +
  scale_fill_manual(labels = c("18th", "19th", "20th"), values = hue_pal()(3)) +
  coord_flip()

ggsave("Number of Bottom 3 finishes by club.png", path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#create goal differential column; in theory this should be higher the higher up the table you go, but that does
#not happen, could be useful for predicting a team regression in the following year
combined_goals$goal_diff <- combined_goals$FTotal - combined_goals$ATotal

#look at range in goal differential
rng_goaldff <- range(combined_goals$goal_diff)

#create figures for goal differential for each of the 3 subgroups created above
GD_top6 <- combined_goals %>%
  filter(Position <= 6) %>%
  ggplot(data = ., aes(x = seasons_rep, y = goal_diff, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Goal differential") +
  xlab("Season") +
  ylim(rng_goaldff[1] - 5, rng_goaldff[2] + 5) +
  labs(title = "Annual distribution of goal differential across all teams", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)


#look at goals scored by teams ranked 6 - 17 at end of season
GD_Mids <- combined_goals %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = goal_diff, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Goal differential") +
  xlab("Season") +
  ylim(rng_goaldff[1] - 5, rng_goaldff[2] + 5) +
  labs(title = "Annual distribution of goal differential across all teams", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at goals allowed by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less goals through time
GD_bottom3 <- combined_goals %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = goal_diff, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Goal differential") +
  xlab("Season") +
  ylim(rng_goaldff[1] - 5, rng_goaldff[2] + 5) +
  labs(title = "Annual distribution of goal differential across all teams", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE) 

#plot annual total goals against broken out by top 6 and bottom 3 teams
g <- grid.arrange(GD_top6, GD_Mids, GD_bottom3, nrow = 1, ncol = 3)

#ggsave("GoalDifferentialAcrossSeasons_BoxPlots.png", g, path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

combined_goals %>%
  group_by(Team) %>%
  summarize(tot_seasons = n()) %>%
  filter(tot_seasons > 7) %>%
  left_join(., combined_goals, by = c("Team", "Team")) %>%
  mutate(year = as.numeric(paste("20",substr(seasons_rep,4,5),sep = ""))) %>%
  ggplot(., aes(x = year, y = goal_diff, color = Team)) +
  geom_line(lwd = 0.75)+
  geom_point(color = "black") +
  geom_hline(yintercept = 0, lwd = 0.75, lty = 2) +
  ylab("Goal differential") +
  xlab("Year") +
  guides(color = FALSE) +
  facet_wrap(~Team) +
  labs(title = "Goal differential by season",subtitle = "Only includes those teams that were in the Premier League 8 of the last 10 seasons")

#ggsave("GoalDifferentialSubsetOfTeams_LineGraphs.png", path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#look at relationship between goals for and goals against overall
  ggplot(combined_goals, aes(x = ATotal, y = FTotal)) +
  geom_point(aes(color = seasons_rep)) +
  geom_smooth(method = "loess", se = FALSE, aes(x = ATotal, y = FTotal), color = "black", lty = 2) +
  labs(color = "Season", title = "Relationship between total goals scored vs. goals against" , subtitle = "Relationship modeled across all seasons") +
  xlab("Goals Against") +
  ylab("Goals Scored") 
  
ggsave("RelationshipBtwnGlsForvsAgainstOverall.png", path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#look at relationship between goals for and goals against broken out by 
combined_goals %>%
  ggplot(., aes(x = ATotal, y = FTotal)) +
  geom_point(color = seasons_rep) +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2) +
  facet_wrap(~seasons_rep) +
  xlab("Goals Against") +
  ylab("Goals Scored") +
  labs(title = "Relationship between total goals scored vs. goals against", subtitle = "Results broken out by season") 

ggsave("RelationshipBtwnGlsForvsAgainstByTeam.png", path = here("English_Soccer_project","EDA_Figures","Goals_Figures"), device = "png", dpi = 400)

#######################################################################
#######################################################################
#move away from looking at goals and begin looking at fouls
#######################################################################
#######################################################################
rng_foulsFor <- range(combined_fouls$TotFouls_Cmt)
rng_foulsAgst <- range(combined_fouls$TotFouls_Agst)
rng_foulsFor
rng_foulsAgst

Fouls_For <- combined_fouls %>%
      ggplot(., aes(seasons_rep, y = TotFouls_Cmt, fill = seasons_rep)) +
      geom_boxplot() +
      geom_smooth(method = "loess", aes(group = 1), lty = 2, color = "black", se = FALSE) +
      xlab("Season") +
      ylab("Total fouls committed") +
      ylim(rng_foulsAgst[1] - 5,rng_foulsFor[2] + 5) +
      labs(title = "Distribution of total fouls committed across all teams") +
      guides(fill = FALSE)

Fouls_Agst <- combined_fouls %>%
      ggplot(., aes(seasons_rep, y = TotFouls_Agst, fill = seasons_rep)) +
      geom_boxplot() +
      geom_smooth(method = "loess", aes(group = 1), lty = 2, color = "black", se = FALSE) +
      xlab("Season") +
      ylab("Total fouls against") +
      ylim(rng_foulsAgst[1] - 5,rng_foulsFor[2] + 5) +
      labs(title = "Distribution of total fouls against across all teams") +
      guides(fill = FALSE)

g <- grid.arrange(Fouls_For, Fouls_Agst, nrow = 1)

ggsave("TotalFOulsForAgainstOverall.png", g, path = here("English_Soccer_project","EDA_Figures","Fouls_Figures"), device = "png", dpi = 400)

#although somewhat difficult to see there are definitely differences in the numbers of
#fouls for and against, but the overall pattern looks roughly the same

#look at fouls committed by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less fouls through time
FoulsFor_top6 <- combined_fouls %>%
  filter(Position <= 6) %>%
  ggplot(data = ., aes(x = seasons_rep, y = TotFouls_Cmt, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of fouls") +
  xlab("Season") +
  ylim(rng_foulsAgst[1] - 5,rng_foulsFor[2] + 5) +
  labs(title = "Annual distribution of total team fouls committed", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)

#look at fouls committed by teams ranked 7 - 17 at end of season
FoulsFor_Mids <- combined_fouls %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = TotFouls_Cmt, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of fouls") +
  xlab("Season") + 
  ylim(rng_foulsAgst[1] - 5,rng_foulsFor[2] + 5) +
  labs(title = "Annual distribution of total team fouls committed", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at fouls committed by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less fouls through time
FoulsFor_bottom3 <- combined_fouls %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = TotFouls_Cmt, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of fouls") +
  xlab("Season") +
  ylim(rng_foulsAgst[1] - 5,rng_foulsFor[2] + 5) +
  labs(title = "Annual distribution of total team fouls committed", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE)

#plot annual total fouls for broken out by top 6 and bottom 3 teams
g <- grid.arrange(FoulsFor_top6, FoulsFor_Mids, FoulsFor_bottom3, nrow = 1, ncol = 3)

ggsave("Total_team_fouls_committedByGroup.png", g, path = here("English_Soccer_project","EDA_Figures","Fouls_Figures"), device = "png", dpi = 400)

#look at fouls against by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less fouls through time
FoulsAgst_top6 <- combined_fouls %>%
  filter(Position <= 6) %>%
  ggplot(data = ., aes(x = seasons_rep, y = TotFouls_Agst, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of fouls") +
  xlab("Season") +
  ylim(rng_foulsAgst[1] - 5,rng_foulsFor[2] + 5) +
  labs(title = "Annual distribution of total team fouls against", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)

#look at fouls against by teams ranked 7 - 17 at end of season
FoulsAgst_Mids <- combined_fouls %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = TotFouls_Agst, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of fouls") +
  xlab("Season") + 
  ylim(rng_foulsAgst[1] - 5,rng_foulsFor[2] + 5) +
  labs(title = "Annual distribution of total team fouls against", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at fouls against by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less fouls through time
FoulsAgst_bottom3 <- combined_fouls %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = TotFouls_Agst, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of fouls") +
  xlab("Season") +
  ylim(rng_foulsAgst[1] - 5,rng_foulsFor[2] + 5) +
  labs(title = "Annual distribution of total team fouls against", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE)

#plot annual total fouls for broken out by top 6 and bottom 3 teams
g <- grid.arrange(FoulsAgst_top6, FoulsAgst_Mids, FoulsAgst_bottom3, nrow = 1, ncol = 3)

ggsave("Total_team_fouls_AgainstByGroup.png", g, path = here("English_Soccer_project","EDA_Figures","Fouls_Figures"), device = "png", dpi = 400)


#look at relationship between goals for and goals against overall
ggplot(combined_fouls, aes(x = TotFouls_Agst, y = TotFouls_Cmt)) +
  geom_point(aes(color = seasons_rep)) +
  geom_smooth(method = "loess", se = FALSE, aes(x = TotFouls_Agst, y = TotFouls_Cmt), color = "black", lty = 2) +
  labs(color = "Season", title = "Relationship between total fouls committed vs. fouls against" , subtitle = "Relationship modeled across all seasons") +
  xlab("Fouls Against") +
  ylab("Fouls Committed") 

ggsave("RelationshipBtwnGlsForvsAgainstOverall.png", path = here("English_Soccer_project","EDA_Figures","Fouls_Figures"), device = "png", dpi = 400)

#look at relationship between goals for and goals against broken out by 
combined_fouls %>%
  ggplot(., aes(x = TotFouls_Agst, y = TotFouls_Cmt)) +
  geom_point(color = seasons_rep) +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2) +
  facet_wrap(~seasons_rep) +
  xlab("Fouls Against") +
  ylab("Fouls Committed") +
  labs(title = "Relationship between total fouls committed vs. fouls against", subtitle = "Results broken out by season") 

ggsave("RelationshipBtwnGlsForvsAgainstByTeam.png", path = here("English_Soccer_project","EDA_Figures","Fouls_Figures"), device = "png", dpi = 400)

#calculate foul differential
combined_fouls$foul_diff <- combined_fouls$TotFouls_Cmt - combined_fouls$TotFouls_Agst

#look at range in foul differential
rng_fouldff <- range(combined_fouls$foul_diff)

#create figures for foul differential for each of the 3 subgroups created above
FD_top6 <- combined_fouls %>%
  filter(Position <= 6) %>%
  ggplot(data = ., aes(x = seasons_rep, y = foul_diff, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Foul differential") +
  xlab("Season") +
  ylim(rng_fouldff[1] - 5, rng_fouldff[2] + 5) +
  labs(title = "Annual distribution of foul differential across all teams", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)


#look at fouls scored by teams ranked 6 - 17 at end of season
FD_Mids <- combined_fouls %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = foul_diff, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Foul differential") +
  xlab("Season") +
  ylim(rng_fouldff[1] - 5, rng_fouldff[2] + 5) +
  labs(title = "Annual distribution of foul differential across all teams", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at fouls allowed by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less fouls through time
FD_bottom3 <- combined_fouls %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = foul_diff, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Foul differential") +
  xlab("Season") +
  ylim(rng_fouldff[1] - 5, rng_fouldff[2] + 5) +
  labs(title = "Annual distribution of foul differential across all teams", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE) 

#plot annual total fouls against broken out by top 6 and bottom 3 teams
g <- grid.arrange(FD_top6, FD_Mids, FD_bottom3, nrow = 1, ncol = 3)

ggsave("FoulDifferentialAcrossSeasons_BoxPlots.png", g, path = here("English_Soccer_project","EDA_Figures","Fouls_Figures"), device = "png", dpi = 400)


combined_fouls %>%
  group_by(Team) %>%
  summarize(tot_seasons = n()) %>%
  filter(tot_seasons > 7) %>%
  left_join(., combined_fouls, by = c("Team", "Team")) %>%
  mutate(year = as.numeric(paste("20",substr(seasons_rep,4,5),sep = ""))) %>%
  ggplot(., aes(x = year, y = foul_diff, color = Team)) +
  geom_line(lwd = 0.75)+
  geom_point(color = "black") +
  geom_hline(yintercept = 0, lwd = 0.75, lty = 2) +
  ylab("Foul differential") +
  xlab("Year") +
  guides(color = FALSE) +
  facet_wrap(~Team) +
  labs(title = "Foul differential by season",subtitle = "Only includes those teams that were in the Premier League 8 of the last 10 seasons")

ggsave("FoulDifferentialSubsetOfTeams_LineGraphs.png", path = here("English_Soccer_project","EDA_Figures","Fouls_Figures"), device = "png", dpi = 400)

#look at relationship between fouls for and fouls against overall
ggplot(combined_fouls, aes(x = TotFouls_Agst, y = TotFouls_Cmt)) +
  geom_point(aes(color = seasons_rep)) +
  geom_smooth(method = "loess", se = FALSE, aes(x = TotFouls_Agst, y = TotFouls_Cmt), color = "black", lty = 2) +
  labs(color = "Season", title = "Relationship between total fouls committed vs. fouls against" , subtitle = "Relationship modeled across all seasons") +
  xlab("Fouls Against") +
  ylab("Fouls Committed") 

ggsave("RelationshipBtwnFlsForvsAgainstOverall.png", path = here("English_Soccer_project","EDA_Figures","Fouls_Figures"), device = "png", dpi = 400)

#look at relationship between fouls for and fouls against broken out by  seasons
combined_fouls %>%
  ggplot(., aes(x = TotFouls_Agst, y = TotFouls_Cmt)) +
  geom_point(color = seasons_rep) +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2) +
  facet_wrap(~seasons_rep) +
  xlab("Fouls Against") +
  ylab("Fouls Committed") +
  labs(title = "Relationship between total fouls committed vs. fouls against", subtitle = "Results broken out by season") 

ggsave("RelationshipBtwnFlsForvsAgainstByTeam.png", path = here("English_Soccer_project","EDA_Figures","Fouls_Figures"), device = "png", dpi = 400)


#look at relationship between fouls committed/against vs. position in table
FoulsCmt <- ggplot(combined_fouls, aes(x = Position, y = TotFouls_Cmt, color = as.factor(combined_fouls$Position))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2, aes(group = 1)) +
  xlab("Position in table") +
  ylab("Total fouls committed") +
  labs(title = "Relationship between position in table and total fouls committed", subtitle = "Relationship modeled across all seasons") +
  guides(color = FALSE)

FoulsAgst <- ggplot(combined_fouls, aes(x = Position, y = TotFouls_Agst, color = as.factor(combined_fouls$Position))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2, aes(group = 1)) +
  xlab("Position in table") +
  ylab("Total fouls against") +
  labs(color = "Table Position", title = "Relationship between position in table and total fouls against", subtitle = "Relationship modeled across all seasons")

g <- grid.arrange(FoulsCmt, FoulsAgst, nrow = 1)

ggsave("TablePositionVsFoulsAgainstCmtCombinedOverall.png", g, path = here("English_Soccer_project","EDA_Figures","Fouls_Figures") , device = "png", dpi = 400)

#LOOK AT FOULS DIFFERENTIAL AS A FUNCTION OF TABLE POSITION
#overall
ggplot(combined_fouls, aes(x = Position, y = foul_diff, color = as.factor(Position))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "white", lwd = 2, alpha = 0.5) +
  geom_vline(xintercept = 10, color = "white", lwd = 2, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2, aes(group = 1)) +
  xlab("Position in table") +
  ylab("Foul differential") +
  labs(color = "Season", title = "Relationship between position in table and foul differential", subtitle = "Relationship modeled across all seasons") +
  guides(color = FALSE)

ggsave("TablePositionVsFoulDifferentialOverall.png", path = here("English_Soccer_project","EDA_Figures","Fouls_Figures") , device = "png", dpi = 400)

#broken out by season; I added horizontal lines at y = 0 and x = 10. These positions 
#were somewhat difficult to see so I did plot them in a thicker white line with some
#transparency
combined_fouls %>%
  ggplot(., aes(x = Position, y = foul_diff)) +
  geom_point(color = seasons_rep) +
  geom_hline(yintercept = 0, color = "white", lwd = 2, alpha = 0.5) +
  geom_vline(xintercept = 10, color = "white", lwd = 2, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2) +
  facet_wrap(~seasons_rep) +
  xlab("Position") +
  ylab("Foul differential") +
  labs(title = "Relationship between table position vs. foul differential", subtitle = "Results broken out by season") 

ggsave("TablePositionVsFoulDifferentialBySeason.png", path = here("English_Soccer_project","EDA_Figures","Fouls_Figures") , device = "png", dpi = 400)

#######################################################################
#######################################################################
#move away from looking at fouls and begin looking at corners
#######################################################################
#######################################################################
rng_cornersFor <- range(combined_corners$CornersFor)
rng_cornersAgst <- range(combined_corners$CornersAgst)
rng_cornersFor
rng_cornersAgst

Corners_For <- combined_corners %>%
  ggplot(., aes(seasons_rep, y = CornersFor, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), lty = 2, color = "black", se = FALSE) +
  xlab("Season") +
  ylab("Total corners ") +
  ylim(rng_cornersAgst[1] - 5,rng_cornersFor[2] + 5) +
  labs(title = "Distribution of total corners for across all teams") +
  guides(fill = FALSE)

Corners_Agst <- combined_corners %>%
  ggplot(., aes(seasons_rep, y = CornersAgst, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), lty = 2, color = "black", se = FALSE) +
  xlab("Season") +
  ylab("Total corners ") +
  ylim(rng_cornersAgst[1] - 5,rng_cornersFor[2] + 5) +
  labs(title = "Distribution of total corners against across all teams") +
  guides(fill = FALSE)

g <- grid.arrange(Corners_For, Corners_Agst, nrow = 1)

ggsave("TotalCornersForAgainstOverall.png", g, path = here("English_Soccer_project","EDA_Figures","Corners_Figures"), device = "png", dpi = 400)

#although somewhat difficult to see there are definitely differences in the numbers of
#corners for and , but the overall pattern looks roughly the same

#look at corners  by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less corners through time
CornersFor_top6 <- combined_corners %>%
  filter(Position <= 6) %>%
  ggplot(data = ., aes(x = seasons_rep, y = CornersFor, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of corners") +
  xlab("Season") +
  ylim(rng_cornersAgst[1] - 5,rng_cornersFor[2] + 5) +
  labs(title = "Annual distribution of total team corners for ", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)

#look at corners  by teams ranked 7 - 17 at end of season
CornersFor_Mids <- combined_corners %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = CornersFor, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of corners") +
  xlab("Season") + 
  ylim(rng_cornersAgst[1] - 5,rng_cornersFor[2] + 5) +
  labs(title = "Annual distribution of total team corners for ", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at corners  by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less corners through time
CornersFor_bottom3 <- combined_corners %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = CornersFor, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of corners") +
  xlab("Season") +
  ylim(rng_cornersAgst[1] - 5,rng_cornersFor[2] + 5) +
  labs(title = "Annual distribution of total team corners for ", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE)

#plot annual total corners for broken out by top 6 and bottom 3 teams
g <- grid.arrange(CornersFor_top6, CornersFor_Mids, CornersFor_bottom3, nrow = 1, ncol = 3)

ggsave("Total_team_cornersFor_ByGroup.png", g, path = here("English_Soccer_project","EDA_Figures","Corners_Figures"), device = "png", dpi = 400)

#look at corners  by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less corners through time
CornersAgst_top6 <- combined_corners %>%
  filter(Position <= 6) %>%
  ggplot(data = ., aes(x = seasons_rep, y = CornersAgst, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of corners") +
  xlab("Season") +
  ylim(rng_cornersAgst[1] - 5,rng_cornersFor[2] + 5) +
  labs(title = "Annual distribution of total team corners against ", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)

#look at corners  by teams ranked 7 - 17 at end of season
CornersAgst_Mids <- combined_corners %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = CornersAgst, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of corners") +
  xlab("Season") + 
  ylim(rng_cornersAgst[1] - 5,rng_cornersFor[2] + 5) +
  labs(title = "Annual distribution of total team corners against", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at corners  by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less corners through time
CornersAgst_bottom3 <- combined_corners %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = CornersAgst, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of corners") +
  xlab("Season") +
  ylim(rng_cornersAgst[1] - 5,rng_cornersFor[2] + 5) +
  labs(title = "Annual distribution of total team corners against", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE)

#plot annual total corners for broken out by top 6 and bottom 3 teams
g <- grid.arrange(CornersAgst_top6, CornersAgst_Mids, CornersAgst_bottom3, nrow = 1, ncol = 3)

ggsave("Total_team_corners_AgainstByGroup.png", g, path = here("English_Soccer_project","EDA_Figures","Corners_Figures"), device = "png", dpi = 400)


#look at relationship between corners for and corners  overall
ggplot(combined_corners, aes(x = CornersAgst, y = CornersFor)) +
  geom_point(aes(color = seasons_rep)) +
  geom_smooth(method = "loess", se = FALSE, aes(x = CornersAgst, y = CornersFor), color = "black", lty = 2) +
  labs(color = "Season", title = "Relationship between total corners for vs. corners against " , subtitle = "Relationship modeled across all seasons") +
  xlab("Corners Against") +
  ylab("Corners For") 

ggsave("RelationshipBtwnCrnersForvsAgainstOverall.png", path = here("English_Soccer_project","EDA_Figures","Corners_Figures"), device = "png", dpi = 400)

#look at relationship between corners for and corners against broken out by 
combined_corners %>%
  ggplot(., aes(x = CornersAgst, y = CornersFor)) +
  geom_point(color = seasons_rep) +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2) +
  facet_wrap(~seasons_rep) +
  xlab("Corners Against") +
  ylab("Corners For") +
  labs(title = "Relationship between total corners for vs. corners against ", subtitle = "Results broken out by season") 

ggsave("RelationshipBtwnCrnersForvsAgainstByTeam.png", path = here("English_Soccer_project","EDA_Figures","Corners_Figures"), device = "png", dpi = 400)

#calculate corner differential
combined_corners$corner_diff <- combined_corners$CornersFor - combined_corners$CornersAgst

#look at range in corner differential
rng_cornerdff <- range(combined_corners$corner_diff)
rng_cornerdff 

#create figures for corner differential for each of the 3 subgroups created above
CD_top6 <- combined_corners %>%
  filter(Position <= 6) %>%
  ggplot(data = ., aes(x = seasons_rep, y = corner_diff, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Corner differential") +
  xlab("Season") +
  ylim(rng_cornerdff[1] - 5, rng_cornerdff[2] + 5) +
  labs(title = "Annual distribution of corner differential across all teams", subtitle = "Includes only the top 6 teams per year") +
  guides(fill = FALSE)


#look at corners scored by teams ranked 6 - 17 at end of season
CD_Mids <- combined_corners %>%
  filter(Position >= 7 & Position <= 17) %>%
  ggplot(data = ., aes(x = seasons_rep, y = corner_diff, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Corner differential") +
  xlab("Season") +
  ylim(rng_cornerdff[1] - 5, rng_cornerdff[2] + 5) +
  labs(title = "Annual distribution of corner differential across all teams", subtitle = "Includes only teams ranked 7 - 17 annually") +
  guides(fill = FALSE)

#look at corners allowed by bottom 3 teams (i.e. those who are relegated at end of season) for changes
#through time; question being are the bottom 3 teams scoring more/less corners through time
CD_bottom3 <- combined_corners %>%
  filter(Position >= 18) %>%
  ggplot(data = ., aes(x = seasons_rep, y = corner_diff, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Corner differential") +
  xlab("Season") +
  ylim(rng_cornerdff[1] - 5, rng_cornerdff[2] + 5) +
  labs(title = "Annual distribution of corner differential across all teams", subtitle = "Includes only the bottom 3 teams per year") +
  guides(fill = FALSE) 

#plot annual total corners  broken out by top 6 and bottom 3 teams
g <- grid.arrange(CD_top6, CD_Mids, CD_bottom3, nrow = 1, ncol = 3)

ggsave("CornerDifferentialAcrossSeasons_BoxPlots.png", g, path = here("English_Soccer_project","EDA_Figures","Corners_Figures"), device = "png", dpi = 400)


combined_corners %>%
  group_by(Team) %>%
  summarize(tot_seasons = n()) %>%
  filter(tot_seasons > 7) %>%
  left_join(., combined_corners, by = c("Team", "Team")) %>%
  mutate(year = as.numeric(paste("20",substr(seasons_rep,4,5),sep = ""))) %>%
  ggplot(., aes(x = year, y = corner_diff, color = Team)) +
  geom_line(lwd = 0.75)+
  geom_point(color = "black") +
  geom_hline(yintercept = 0, lwd = 0.75, lty = 2) +
  ylab("Corner differential") +
  xlab("Year") +
  guides(color = FALSE) +
  facet_wrap(~Team) +
  labs(title = "Corner differential by season",subtitle = "Only includes those teams that were in the Premier League 8 of the last 10 seasons")

ggsave("CornerDifferentialSubsetOfTeams_LineGraphs.png", path = here("English_Soccer_project","EDA_Figures","Corners_Figures"), device = "png", dpi = 400)

#look at relationship between corners for and corners against overall
ggplot(combined_corners, aes(x = CornersAgst, y = CornersFor)) +
  geom_point(aes(color = seasons_rep)) +
  geom_smooth(method = "loess", se = FALSE, aes(x = CornersAgst, y = CornersFor), color = "black", lty = 2) +
  labs(color = "Season", title = "Relationship between total corners for vs. corners against" , subtitle = "Relationship modeled across all seasons") +
  xlab("Corners Against") +
  ylab("Corners For") 

ggsave("RelationshipBtwnFlsForvsAgainstOverall.png", path = here("English_Soccer_project","EDA_Figures","Corners_Figures"), device = "png", dpi = 400)

#look at relationship between corners for and corners  broken out by  seasons
combined_corners %>%
  ggplot(., aes(x = CornersAgst, y = CornersFor)) +
  geom_point(color = seasons_rep) +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2) +
  facet_wrap(~seasons_rep) +
  xlab("Corners Against") +
  ylab("Corners For") +
  labs(title = "Relationship between total corners for vs. corners against ", subtitle = "Results broken out by season") 

ggsave("RelationshipBtwnFlsForvsAgainstByTeam.png", path = here("English_Soccer_project","EDA_Figures","Corners_Figures"), device = "png", dpi = 400)


#look at relationship between corners / vs. position in table
CornersFor <- ggplot(combined_corners, aes(x = Position, y = CornersFor, color = as.factor(combined_corners$Position))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2, aes(group = 1)) +
  xlab("Position in table") +
  ylab("Total corners for ") +
  labs(title = "Relationship between position in table and total corners for", subtitle = "Relationship modeled across all seasons") +
  guides(color = FALSE)

CornersAgst <- ggplot(combined_corners, aes(x = Position, y = CornersAgst, color = as.factor(combined_corners$Position))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2, aes(group = 1)) +
  xlab("Position in table") +
  ylab("Total corners against") +
  labs(color = "Table Position", title = "Relationship between position in table and total corners against", subtitle = "Relationship modeled across all seasons")

g <- grid.arrange(CornersFor, CornersAgst, nrow = 1)

ggsave("TablePositionVsCornersAgainstForCombinedOverall.png", g, path = here("English_Soccer_project","EDA_Figures","Corners_Figures") , device = "png", dpi = 400)

#LOOK AT CORNERS DIFFERENTIAL AS A FUNCTION OF TABLE POSITION
#overall
ggplot(combined_corners, aes(x = Position, y = corner_diff, color = as.factor(Position))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "white", lwd = 2, alpha = 0.5) +
  geom_vline(xintercept = 10, color = "white", lwd = 2, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2, aes(group = 1)) +
  xlab("Position in table") +
  ylab("Corner differential") +
  labs(color = "Season", title = "Relationship between position in table and corner differential", subtitle = "Relationship modeled across all seasons") +
  guides(color = FALSE)

ggsave("TablePositionVsCornerDifferentialOverall.png", path = here("English_Soccer_project","EDA_Figures","Corners_Figures") , device = "png", dpi = 400)

#broken out by season; I added horizontal lines at y = 0 and x = 10. These positions 
#were somewhat difficult to see so I did plot them in a thicker white line with some
#transparency
combined_corners %>%
  ggplot(., aes(x = Position, y = corner_diff)) +
  geom_point(color = seasons_rep) +
  geom_hline(yintercept = 0, color = "white", lwd = 2, alpha = 0.5) +
  geom_vline(xintercept = 10, color = "white", lwd = 2, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black", lty = 2) +
  facet_wrap(~seasons_rep) +
  xlab("Position") +
  ylab("Corner differential") +
  labs(title = "Relationship between table position vs. corner differential", subtitle = "Results broken out by season") 

ggsave("TablePositionVsCornerDifferentialBySeason.png", path = here("English_Soccer_project","EDA_Figures","Corners_Figures") , device = "png", dpi = 400)

