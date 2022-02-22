library(tidyverse)

# this code works 
new.features.df <- as_tibble(readRDS("data/nfl/NFL_DataBowl_TrainingData.rds")) %>%
	select(Season, Week, PlayId, Yards, NflIdRusher, NflId, DisplayName, PossessionTeam) %>%
	rename(Play.ID = PlayId)				%>%
	rename(NFL.ID.Rusher = NflIdRusher)		%>%
	rename(NFL.ID = NflId)					%>%
	rename(Player.Name = DisplayName)		%>%
	rename(Offense = PossessionTeam)		%>%
	filter(Season==2018)					%>%
	filter(NFL.ID.Rusher == NFL.ID)


print(new.features.df)


# this code generates the error "NFL.ID.Rusher" because the column name has not been updated yet
new.features.df <- as_tibble(readRDS("data/nfl/NFL_DataBowl_TrainingData.rds")) %>%
	select(Season, Week, PlayId, Yards, NflIdRusher, NflId, DisplayName, PossessionTeam) %>%
	filter(Season==2018)					%>%
	filter(NFL.ID.Rusher == NFL.ID)			%>%
	rename(Play.ID = PlayId)				%>%
	rename(NFL.ID.Rusher = NflIdRusher)		%>%
	rename(NFL.ID = NflId)					%>%
	rename(Player.Name = DisplayName)		%>%
	rename(Offense = PossessionTeam)

