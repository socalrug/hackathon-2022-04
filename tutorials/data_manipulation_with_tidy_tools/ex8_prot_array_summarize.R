
library(tidyverse)

# Make data more tidy
#   use 'gather' command to bring organize protein array signals into single
#   column called 'Norm.Intensity'
tidy.prot.array.tib <- as_tibble(read.csv("data/protein_array/norm_data_matrix.csv", stringsAsFactors = FALSE)) %>% 
  gather(starts_with("JH"), key = "Sample.ID", value = "Norm.Intensity") %>%
  separate(Sample.ID, into = c("Subject.ID", "Study.Day"), sep = ".Day.") %>%
  mutate(Study.Day = paste("Day", Study.Day, sep = ".")) %>%
  mutate(Study.Day = factor(Study.Day, levels=c("Day.0","Day.7","Day.28","Day.35","Day.56","Day.63","Day.7.Post"))) # %>%


delta.tib <- tidy.prot.array.tib %>% 
  spread(key = Study.Day, value = Norm.Intensity) %>%
  mutate(Post.Delta = Day.7.Post - Day.0) %>%
  group_by(ID_REF) %>%
  summarize(Mean.Post.Delta = mean(Post.Delta)) %>%
  arrange(desc(Mean.Post.Delta))

	
top.10.ids <- delta.tib$ID_REF[1:10]

# now make a tidy tibble with just the ten proteins with largest mean increases
top.10.tib <- tidy.prot.array.tib %>%
	filter(ID_REF %in% top.10.ids)



pdf("Example8_Output.pdf")

p.boxplot <- ggplot(top.10.tib, aes(Study.Day, Norm.Intensity)) +
	geom_boxplot() +
	facet_wrap(~ID_REF) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	ggtitle("Top 10 Delta\nStudy Day versus Normalized Intensity")  
print(p.boxplot)

# Add numeric version of Day for longitudinal plot
top.10.tib$Day <- as.numeric(substr(top.10.tib$Study.Day, 5,10))

p.lineplot <- ggplot(top.10.tib, aes(Day, Norm.Intensity, color=ID_REF)) +
	geom_line() +
	facet_wrap(~Subject.ID) + 
	ggtitle("Top 10 Delta\nStudy Day versus Normalized Intensity\nFacet by Subject")  
print(p.lineplot)

p.lineplot <- ggplot(top.10.tib, aes(Day, Norm.Intensity, color=Subject.ID)) +
	geom_line() +
	facet_wrap(~ID_REF) + 
	ggtitle("Top 10 Delta\nStudy Day versus Normalized Intensity\nFacet by Protein")  
print(p.lineplot)

dev.off()