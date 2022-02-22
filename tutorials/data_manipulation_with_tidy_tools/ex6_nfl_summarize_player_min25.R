
library(tidyverse)


# get rushers from 2018
rushers.tib <- as_tibble(readRDS("data/nfl/NFL_DataBowl_TrainingData.rds")) %>%
	select(Season, Week, Yards, NflIdRusher, NflId, PossessionTeam) %>%
	rename(NFL.ID.Rusher = NflIdRusher)	%>%
	rename(NFL.ID = NflId)				%>%
	rename(Team = PossessionTeam)		%>%
	filter(NFL.ID.Rusher == NFL.ID)		%>%
	filter(Season==2018) %>%
	mutate(Week.Range = ifelse(Week < 13, "Weeks.1-12", "Weeks.13-17")) %>%
	mutate(Yards.FC = ifelse(Yards > 10, 10, ifelse(Yards < -3, -3, Yards)))

print(rushers.tib)


summary.tib <- rushers.tib %>%
	group_by(NFL.ID.Rusher, Week.Range) %>%
	summarize(Average.Yards = mean(Yards.FC)) %>%
	spread(key = Week.Range, value = Average.Yards)


# apply filter for players with at least 30 rushes in each range of weeks
counts.tab <- table(rushers.tib$NFL.ID.Rusher, rushers.tib$Week.Range)
min.count.vec <- apply(counts.tab, 1, min)
usable.ids <- names(min.count.vec[min.count.vec >= 25])

pdf("Example6_Output.pdf")

p.1 <- ggplot(filter(summary.tib, NFL.ID.Rusher %in% usable.ids), aes(`Weeks.1-12`,`Weeks.13-17`)) + 
 	geom_point() + 
 	geom_abline() +
  	xlim(-3,10) +
 	ylim(-3,10) +	
 	ggtitle("NFL 2018 Rushing Yards by Player (n >= 25) \nAverage of Weeks 1-12 vs Weeks 13-17\n with floor = -3, ceil = 10")
print(p.1)


dev.off()