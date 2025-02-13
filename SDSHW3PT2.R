library(readr)
library(dplyr)
library(ggplot2)
library(boot)
sclass <- read_csv("C:/Users/esthe/OneDrive/Documents/SDS313/sclass.csv")
#Question2

# This filters the data for 2011 year and get rid of 63 AMG
filtered_data_A <- sclass %>%
  filter(year == 2011, trim == "63 AMG")

# To calculate the mean mileage
mean_mileage <- function(data, indices) {
  sample_data <- data[indices, ]  # Resample the data
  mean(sample_data$mileage)       # Return the mean mileage
}
bootstrap_A <- boot(filtered_data_A, mean_mileage, R = 1000)

# This Calculates the 95% confidence interval
CI_A <- boot.ci(bootstrap_A, type = "perc", conf = 0.95)

CI_A

# This filters the data for 2014 year get rid of 550
filtered_data_B <- sclass %>%
  filter(year == 2014, trim == "550")

# The new variable I made called isBlack and if the car is black it says true
filtered_data_B <- filtered_data_B %>%
  mutate(isBlack = ifelse(color == "Black", TRUE, FALSE))

# This calculates the proportion of black cars
proportion_black <- function(data, indices) {
  sample_data <- data[indices, ]
  mean(sample_data$isBlack)
}

bootstrap_B <- boot(filtered_data_B, proportion_black, R = 1000)

# This calculate the 95% confidence interval
CI_B <- boot.ci(bootstrap_B, type = "perc", conf = 0.95)

CI_B
