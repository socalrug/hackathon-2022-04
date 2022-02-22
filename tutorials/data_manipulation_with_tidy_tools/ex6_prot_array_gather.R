


library(tidyverse)


# Make data tidy - one row per observation
#   use 'gather' command to bring organize protein array signals into single
#   column called 'Norm.Intensity'
tidy.prot.array.tib <- as_tibble(read.csv("data/protein_array/norm_data_matrix.csv")) %>% 
  gather(starts_with("JH"), key = "Sample.ID", value = "Norm.Intensity") %>%
  separate(Sample.ID, into = c("Subject.ID", "Study.Day"), sep = ".Day.") %>%
  mutate(Study.Day = factor(Study.Day, levels=c("0","7","28","35","56","63","7.Post")))


# tidy.prot.array.tib

pdf("Example2_Output.pdf")

p.boxplot <- ggplot(tidy.prot.array.tib, aes(Study.Day, Norm.Intensity)) +
	geom_boxplot() +
	ggtitle("All Array Probs\nStudy Day versus Normalized Intensity")  
print(p.boxplot)

# here I am just selecting the single most reactive protein
react.df <- tidy.prot.array.tib %>%
				filter(ID_REF == "ETEC_2032")

p.react.boxplot <- ggplot(react.df, aes(Study.Day, Norm.Intensity)) +
	geom_boxplot() +
	ggtitle("Single Reactive Antigen\nStudy Day versus Normalized Intensity") 			
print(p.react.boxplot)

dev.off()