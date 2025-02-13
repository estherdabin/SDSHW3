library(readr)
library(dplyr)
library(ggplot2)
library(boot)
gasprices <- read_csv("C:/Users/esthe/OneDrive/Documents/SDS313/gasprices.csv")
#Question1

# A) Gas stations charge more if they lack direct competition in sight.

#This compares the prices of gas stations with and without visible competitors

summary_A <- gasprices %>%
  group_by(Competitors) %>%
  summarise(mean_price = mean(Price), sd_price = sd(Price), n = n())

# Compute the mean difference
bootstrap_diff <- function(data, indices) {
  sample_data <- data[indices, ]
  mean(sample_data$Price[sample_data$Competitors == "N"]) -
    mean(sample_data$Price[sample_data$Competitors == "Y"])
}

set.seed(123)
bootstrap_A <- boot(data = gasprices, statistic = bootstrap_diff, R = 1000)

# To compute 95% confidence interval
ci_A <- boot.ci(bootstrap_A, type = "perc", conf = 0.95)

# Graph
ggplot(gasprices, aes(x = Competitors, y = Price, fill = Competitors)) +
  geom_boxplot() +
  labs(title = "Price comparison with and without competitors",
       x = "Competitors", y = "Price") +
  annotate("text", x = 1.5, y = max(gasprices$Price), 
           label = paste("95% CI: [", round(ci_A$percent[4], 2), ",", round(ci_A$percent[5], 2), "]"), 
           color = "blue", size = 5)


#B) The richer the area, the higher the gas prices.
#This plots the relationship between median household income and gas price.

ggplot(gasprices, aes(x = Income, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Gas prices coorelation to Income",
       x = "Median household income", y = "Price")

bootstrap_mean_B <- function(data, indices) {
  sample_data <- data[indices, ]
  mean(sample_data$Price)
}

set.seed(123)
bootstrap_results_B <- boot(data = gasprices, statistic = bootstrap_mean_B, R = 1000)

# To compute 95% confidence interval
ci_B <- boot.ci(bootstrap_results_B, type = "perc", conf = 0.95)
ci_B

# To compare prices with and without stoplights
ggplot(gasprices, aes(x = Stoplight, y = Price, fill = Stoplight)) +
  geom_boxplot() +
  labs(title = "Price comparison between stoplight and no stoplight",
       x = "Stoplight", y = "Price") +
  scale_fill_manual(values = c("blue", "lightblue"))

bootstrap_mean_C <- function(data, indices) {
  sample_data <- data[indices, ]
  mean(sample_data$Price)
}

set.seed(123)
bootstrap_results_C <- boot(data = gasprices, statistic = bootstrap_mean_C, R = 1000)

# To get 95% confidence interval
ci_C <- boot.ci(bootstrap_results_C, type = "perc", conf = 0.95)
ci_C
# D) Gas stations with direct highway access charge more.
# This compares the prices of gas stations with and without highway access.
ggplot(gasprices, aes(x = Highway, y = Price, fill = Highway)) +
  geom_boxplot() +
  labs(title = "Price comparison of having highway access and no highway access",
       x = "Highway Access", y = "Price") +
  scale_fill_manual(values = c("blue", "lightblue"))

bootstrap_mean_D <- function(data, indices) {
  sample_data <- data[indices, ]
  mean(sample_data$Price)
}

set.seed(123)
bootstrap_results_D <- boot(data = gasprices, statistic = bootstrap_mean_D, R = 1000)

# To calculate 95% confidence interval
ci_D <- boot.ci(bootstrap_results_D, type = "perc", conf = 0.95)
ci_D

# E) Shell charges more than all other non-Shell brands.
# This compares the prices for Shell and non-Shell brands.
ggplot(gasprices, aes(x = Brand, y = Price, fill = Brand)) +
  geom_boxplot() +
  labs(title = "Price comparison of Shell and Non-Shell",
       x = "Brand", y = "Price") +
  scale_fill_manual(values = c("blue", "lightblue", "lightgreen", "lightpink"))

bootstrap_mean_E <- function(data, indices) {
  sample_data <- data[indices, ]
  mean(sample_data$Price)
}

set.seed(123)
bootstrap_results_E <- boot(data = gasprices, statistic = bootstrap_mean_E, R = 1000)

# To get 95% confidence interval
ci_E <- boot.ci(bootstrap_results_E, type = "perc", conf = 0.95)
ci_E
