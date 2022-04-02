library(tidyverse)

years <- 2004:2019

read_data <- function(yr) {
  fn <- paste0("us_pop_", yr, ".csv")
  read_csv(fn, skip = 4, col_names = FALSE) %>%
    rename(state = X1,
           total_population = X2) %>%
    mutate(year = yr)
}

pop_mrg <- map(years, read_data) %>% bind_rows()
write_csv(pop_mrg, "us_pop_2004-2019.csv")