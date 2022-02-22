
library(tidyverse)


# get rushers from 2018
rushers.tib <- as_tibble(readRDS("data/nfl/NFL_DataBowl_TrainingData.rds")) %>%
	select(Season, Week, Yards, NflIdRusher, NflId, PossessionTeam) %>%
	rename(NFL.ID.Rusher = NflIdRusher)	%>%
	rename(NFL.ID = NflId)				%>%
	rename(Team = PossessionTeam)		%>%
	filter(NFL.ID.Rusher == NFL.ID)		%>%
	filter(Season==2018) %>%
	mutate(Week.Range = ifelse(Week < 13, "Weeks.1-12", "Weeks.13-17"))

print(rushers.tib)


summary.tib <- rushers.tib %>%
	group_by(Team, Week.Range) %>%
	summarize(Average.Yards = mean(Yards)) %>%
	spread(key = Week.Range, value = Average.Yards)


pdf("Example3_Output.pdf")

p.1 <- ggplot(summary.tib, aes(`Weeks.1-12`,`Weeks.13-17`)) + 
 	geom_point() + 
 	geom_abline() +
 	ggtitle("NFL 2018 Rushing Yards by Team\nAverage ofWeeks 1-12 vs Weeks 13-17")
print(p.1)

dev.off()