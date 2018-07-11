#read in required libraries
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(googlesheets))
suppressMessages(library(gridExtra))
suppressMessages(library(scales))

#read in files using source file
source(here("English_Soccer_project","Scripts","ReadFiles_FromGoogleSheets.R"))

#all individual files read in from the source file were combined into a single dataframe 'combined'

#insert a "/" character into the seasons_rep column
combined$seasons_rep <- paste(substr(combined$seasons_rep,1,2),"/",substr(combined$seasons_rep,3,4),sep="")

#range of FPer_Game and range of APer_Game
rng_For <- range(combined$FTotal)
rng_Against <- range(combined$ATotal)

#plot number of instances of each team was in the Premier League from 2008 - 2018
combined %>%
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

#ggsave("All_PL_Teams0818.png", path = here("English_Soccer_project","EDA_Figures"), device = "png", dpi = 400)

#look at distribution of goals for and goals against for each year
For_overall <- ggplot(combined, aes(x = seasons_rep, y = FTotal, fill = seasons_rep)) +
               geom_boxplot() +
               geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
               ylab("Number of goals") +
               xlab("Season") +
               ylim(rng_Against[1] - 0.10, rng_For[2] + 0.10) +
               labs(title = "Annual distribution of total team goals scored") +
               guides(fill = FALSE)

Against_overall <- ggplot(combined, aes(x = seasons_rep, y = ATotal, fill = seasons_rep)) +
                   geom_boxplot() +
                   geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
                   ylab("Number of goals") +
                   xlab("Season") +
                   ylim(rng_Against[1] - 0.10, rng_For[2] + 0.10) +
                   labs(title = "Annual distribution of total team goals allowed") +
                   guides(fill = FALSE)

#plot annual total goals for and total goals against across all teams
grid.arrange(For_overall,Against_overall, nrow = 1, ncol =2)

#ggsave("Total_team_goals_scored_vs_allowed_ALL_TEAMS.png", path = here("English_Soccer_project","EDA_Figures"), device = "png", dpi = 400)

#look at goals scored by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less goals through time
For_top6 <- combined %>%
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
For_Mids <- combined %>%
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
For_bottom3 <- combined %>%
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
grid.arrange(For_top6, For_Mids, For_bottom3, nrow = 1, ncol = 3)

#ggsave("Total_team_goals_scored.png", path = here("English_Soccer_project","EDA_Figures"), device = "png", dpi = 400)

#look at goals allowed by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less goals through time
Against_top6 <- combined %>%
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
Against_Mids <- combined %>%
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
Against_bottom3 <- combined %>%
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
grid.arrange(Against_top6, Against_Mids, Against_bottom3, nrow = 1, ncol = 3)

#ggsave("Total_team_goals_allowed.png", path = here("English_Soccer_project","EDA_Figures"), device = "png", dpi = 400)

#####################################################################
#Create the same series of graphs created above but instead of
#looking simply at the total number of goals for/against, look 
#at the number for/against per game

#first need to change the column headings FPer Game and APer Game to include no
#blank space
colnames(combined)[grep(".*Per",colnames(combined))] <- gsub("Per ", "Per_",colnames(combined)[grep(".*Per",colnames(combined))])

#range of FPer_Game and range of APer_Game
rng_FPer <- range(combined$FPer_Game)
rng_APer <- range(combined$APer_Game)

#look at distribution of goals for and goals against for each year
ForPer_overall <- ggplot(combined, aes(x = seasons_rep, y = FPer_Game, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals per game") +
  xlab("Season") +
  ylim(rng_APer[1] - 0.10, rng_FPer[2] + 0.10) +
  labs(title = "Annual distribution of total team goals scored") +
  guides(fill = FALSE)

AgainstPer_overall <- ggplot(combined, aes(x = seasons_rep, y = APer_Game, fill = seasons_rep)) +
  geom_boxplot() +
  geom_smooth(method = "loess", aes(group = 1), se = FALSE, color = "black", lty = 2) +
  ylab("Number of goals per game") +
  xlab("Season") +
  ylim(rng_APer[1] - 0.10, rng_FPer[2] + 0.10) +
  labs(title = "Annual distribution of total team goals allowed") +
  guides(fill = FALSE)

#plot annual total goals for and total goals against across all teams
grid.arrange(ForPer_overall,AgainstPer_overall, nrow = 1, ncol =2)

#ggsave("Total_team_goals_scored_vs_allowedPerGame_ALL_TEAMS.png", path = here("English_Soccer_project","EDA_Figures"), device = "png", dpi = 400)

#look at goals scored by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less goals through time
ForPer_top6 <- combined %>%
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
ForPer_Mids <- combined %>%
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
ForPer_bottom3 <- combined %>%
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
grid.arrange(ForPer_top6, ForPer_Mids, ForPer_bottom3, nrow = 1, ncol = 3)

#ggsave("Total_team_goals_scoredPerGame.png", path = here("English_Soccer_project","EDA_Figures"), device = "png", dpi = 400)

#look at goals allowed by top 6 teams (i.e. those who qualify for either Champions League or Europa League) for changes
#through time; question being are the top 6 teams scoring more/less goals through time
AgainstPer_top6 <- combined %>%
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
AgainstPer_Mids <- combined %>%
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
AgainstPer_bottom3 <- combined %>%
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
grid.arrange(AgainstPer_top6, AgainstPer_Mids, AgainstPer_bottom3, nrow = 1, ncol = 3)

#ggsave("Total_team_goals_allowedPerGame.png", path = here("English_Soccer_project","EDA_Figures"), device = "png", dpi = 400)

#look at number of top four finishes by team; needs work haven't figured out code yet
combined %>%
  group_by(Team) %>%
  count(Position) %>%
  filter(Position <= 4) %>%
  ggplot(., aes(x = Team, y = n, fill = as.factor(Position))) +
  geom_col(position = "dodge") +
  ylab("Number of times a team has finished in one of the top 4 positions") +
  labs(fill = "Final team position") +
  scale_fill_manual(labels = c("1st", "2nd", "3rd", "4th"), values = hue_pal()(4))

#look at number of bottom 3 finishes by team
combined %>%
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



