# data.table  -------------------------------------------------------------
library(data.table)
library(lubridate)
library(janitor)

DT <- data.table::fread("Evening_activity/Global_Superstore2.csv") # Read in the data

DT <- janitor::clean_names(DT)  #use the janitor package to convert variable names to lower case and snake case 

str(DT)

# Which customer segment is most profitable in each year? 
# (Hint: convert dates from character to actual dates)

DT[, order_date := lubridate::dmy(order_date)]  # Convert order_date from character to date
DT[, order_year := lubridate::year(order_date)]  # create a new colum with Year from order_date

DT[, tot_profit := sum(profit), by = .(order_year, segment)] 


DT1 <- DT[, .SD[which.max(tot_profit)], by = .(order_year,segment)][
  ,.(order_year, segment, tot_profit)][
    order(order_year,-tot_profit),]
DT1

# Answer:
#   order_year   segment   
# 1:       2011  Consumer 
# 2:       2012  Consumer 
# 3:       2013  Consumer 
# 4:       2014  Consumer 


# Which country has the second highest total sales in 2011?
DT2 <- DT[order_year == 2011,  sum(sales), by = country][
  order(-V1)]
DT2[2,]

# Answer:
#         country       V1
# 1: United States 484247.5
# 2:         China 155693.6


# Which are the top 5 total-profit products in 2014?
DT3 <- DT[order_year == 2014,  sum(profit), by = product_name][
  order(-V1)]

head(DT3, 5)

# Answer
#                             product_name        V1
# 1: Canon imageCLASS 2200 Advanced Copier 15679.955
# 2:          Cisco Smart Phone, Full Size  7262.348
# 3:       Motorola Smart Phone, Full Size  6307.557
# 4:                     Hoover Stove, Red  5123.234
# 5:  Sauder Classic Bookcase, Traditional  4937.969



# Which 3 countries have the fastest mean shipping time in days? 
# (hint: shipping time = ship_date - order_date) 
DT[, ship_date := lubridate::dmy(ship_date)]  # Convert order_date from character to date
DT[, shipping_time := ship_date - order_date]
DT4 <- DT[,  mean(shipping_time), by = country][
  order(V1)]
head(DT4, 3)

# Answer:
#           country            V1
# 1:          Chad 2.000000 days
# 2:       Bahrain 2.000000 days
# 3: Guinea-Bissau 2.444444 days


# Plot
# Provide a boxplot showing Sales by Sub-category
library(tidyverse)
DT %>% 
  ggplot2::ggplot(aes(x= sub_category, y = sales)) +
  geom_boxplot()