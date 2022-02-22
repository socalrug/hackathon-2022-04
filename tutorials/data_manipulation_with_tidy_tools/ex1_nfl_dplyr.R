
library(dplyr)

# without pipes
raw.nfl.train.df <- readRDS("data/nfl/NFL_DataBowl_TrainingData.rds")
sel.cols.df <- select(raw.nfl.train.df, Season, Week, Yards, NflIdRusher, NflId)
rushers.df 	<- filter(sel.cols.df, NflIdRusher == NflId)
final.df 	<- filter(rushers.df, Season == 2018)
final.df$Train.Test <- ifelse(final.df$Week < 13, "Train", "Test")

# with pipes
pipes.final.df <- readRDS("data/nfl/NFL_DataBowl_TrainingData.rds") %>%
	select(Season, Week, Yards, NflIdRusher, NflId) %>%
	filter(NflIdRusher == NflId) %>%
	filter(Season==2018) %>%
	mutate(Train.Test = ifelse(Week < 13, "Train", "Test"))


# check if the objects have the same content
print("Number of differences:")
print(sum(final.df != pipes.final.df))