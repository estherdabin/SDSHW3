library(readr)
library(dplyr)
library(ggplot2)
gasprices <- read_csv("C:/Users/esthe/OneDrive/Documents/SDS313/gasprices.csv")
#Question1

# A) Gas stations charge more if they lack direct competition in sight.

#This compares the prices of gas stations with and without visible competitors
ggplot(gasprices, aes(x = Competitors, y = Price)) +
  geom_boxplot() +
  labs(title = "Price comparison with or without competitors",
       x = "Competitors", y = "Price")

#B) The richer the area, the higher the gas prices.
#This plots the relationship between median household income and gas price.

ggplot(gasprices, aes(x = Income, y = Price)) +
  geom_point() + geom_smooth(method = "lm", color = "blue") +
  labs(title = "Gas Price vs. Income",
       x = "Median household income", y = "Price")

# C) Gas stations at stoplights charge more.
# This compares the prices at gas stations with and without stoplights.
ggplot(gasprices, aes(x = Stoplight, y = Price)) +
  geom_boxplot() +
  labs(title = "Price comparison of having stoplights and no stoplights",
       x = "Stoplight", y = "Price")


# D) Gas stations with direct highway access charge more.
# This compares the prices of gas stations with and without highway access.
ggplot(gasprices, aes(x = Highway, y = Price)) +
  geom_boxplot() + labs(title = "Price comparison of having highway access and no highway access",
       x = "Highway Access", y = "Price")

# E) Shell charges more than all other non-Shell brands.
# This compares the prices for Shell and non-Shell brands.
ggplot(gasprices, aes(x = Brand, y = Price)) +
  geom_boxplot() + labs(title = "Price comparison of Shell and other brands",
       x = "Brand", y = "Price")
