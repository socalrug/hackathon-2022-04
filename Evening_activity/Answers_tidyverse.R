# Tidyverse ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)



DF <- read_csv("Global_Superstore2.csv")
DF <- janitor::clean_names(DF)  #use the janitor package to convert variable names to lower case and snake case 
glimpse(DF)


# Which customer segment is most profitable in each year? 
# (Hint: convert dates from character to actual dates)

DF <- DF %>% 
  mutate(order_date = lubridate::dmy(order_date)) %>% # Convert order_date from character to date
  mutate(order_year = lubridate::year(order_date))  # create a new colum with Year from order_date

DF1 <- DF %>% 
  group_by(order_year) %>% 
  slice_max(profit) %>% 
  select(segment, order_year, profit)

DF1

# Answer:
#   segment   order_year profit
# 1 Consumer        2011  4630.
# 2 Consumer        2012  3177.
# 3 Corporate       2013  8400.
# 4 Consumer        2014  6720.


# Which country has the second highest total sales in 2011?
DF2 <- DF %>% 
  filter(order_year == 2011) %>%
  group_by(country) %>% 
  summarise(total_sales = sum(sales)) %>% 
  arrange(-total_sales)

DF2[2,]

# Answer:
# country total_sales
#   1 China       155694.


# Which are the top 5 total-profit products in 2014?
DF3 <- DF %>%   
  filter(order_year == 2014) %>%
  group_by(product_name) %>% 
  summarise(total_profit = sum(profit)) %>% 
  arrange(-total_profit)

head(DF3, 5)

# Answer: 
#     product_name                          total_profit
#   1 Canon imageCLASS 2200 Advanced Copier       15680.
# 2 Cisco Smart Phone, Full Size                 7262.
# 3 Motorola Smart Phone, Full Size              6308.
# 4 Hoover Stove, Red                            5123.
# 5 Sauder Classic Bookcase, Traditional         4938.

# Which 3 countries have the fastest mean shipping time in days? 
# (hint: shipping time = ship_date - order_date)

DF4 <- DF %>% 
  mutate(ship_date = lubridate::dmy(ship_date)) %>% 
  mutate(shipping_time = ship_date - order_date) %>% 
  group_by(country) %>% 
  summarise(avg_ship_time= mean(shipping_time)) %>% 
  arrange(avg_ship_time)

head(DF4,3)

# Answer:
#   country       avg_ship_time
# 1 Bahrain       2.000000 days
# 2 Chad          2.000000 days
# 3 Guinea-Bissau 2.444444 days