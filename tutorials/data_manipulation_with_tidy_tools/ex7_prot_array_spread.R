
library(tidyverse)

# Make data more tidy
#   use 'gather' command to bring organize protein array signals into single
#   column called 'Norm.Intensity'
tidy.prot.array.tib <- as_tibble(read.csv("data/protein_array/norm_data_matrix.csv", stringsAsFactors = FALSE)) %>% 
  gather(starts_with("JH"), key = "Sample.ID", value = "Norm.Intensity") %>%
  separate(Sample.ID, into = c("Subject.ID", "Study.Day"), sep = ".Day.") %>%
  mutate(Study.Day = paste("Day", Study.Day, sep = ".")) %>%
  mutate(Study.Day = factor(Study.Day, levels=c("Day.0","Day.7","Day.28","Day.35","Day.56","Day.63","Day.7.Post"))) # %>%

# Add the time points as separate columns to make scatterplots by subject
scatter.tib <- tidy.prot.array.tib %>% 
  spread(key = Study.Day, value = Norm.Intensity)

pdf("Example7_Output.pdf")

p.scatter <- ggplot(scatter.tib, aes(Day.0, Day.7)) + 
  geom_point(alpha=0.25) +
  facet_wrap(~Subject.ID) +
  geom_abline() +
  ggtitle("Subject Scatterplots: \nDay 0 vs Day 7 After First Vax")
print(p.scatter)

p.scatter <- ggplot(scatter.tib, aes(Day.0, Day.7.Post)) + 
  geom_point(alpha=0.25) +
  facet_wrap(~Subject.ID) +
  geom_abline() +  
  ggtitle("Subject Scatterplots: \nDay 0 vs Day 7 Post-Challenge")
print(p.scatter)

dev.off()