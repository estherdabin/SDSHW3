library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

nbc_pilotsurvey <- read_csv("C:/Users/esthe/OneDrive/Documents/SDS313/nbc_pilotsurvey.csv")
#Question3

# This filters data for the two shows
part_A_data <- nbc_pilotsurvey %>%
  filter(Show %in% c("Living with Ed", "My Name is Earl"))

# This calculates means and standard deviations for happiness
happy_stats <- part_A_data %>%
  group_by(Show) %>%
  summarise(mean_happy = mean(Q1_Happy, na.rm = TRUE),
            sd_happy = sd(Q1_Happy, na.rm = TRUE),
            n = n())

# The Confidence interval for the difference in means
diff_mean_A <- happy_stats$mean_happy[happy_stats$Show == "Living with Ed"] - happy_stats$mean_happy[happy_stats$Show == "My Name is Earl"]
se_A <- sqrt((happy_stats$sd_happy[happy_stats$Show == "Living with Ed"]^2 / happy_stats$n[happy_stats$Show == "Living with Ed"]) +
               (happy_stats$sd_happy[happy_stats$Show == "My Name is Earl"]^2 / happy_stats$n[happy_stats$Show == "My Name is Earl"]))

CI_lower_A <- diff_mean_A - 1.96 * se_A
CI_upper_A <- diff_mean_A + 1.96 * se_A

list(diff_mean_A = diff_mean_A, CI_lower_A = CI_lower_A, CI_upper_A = CI_upper_A)

# This filters data for two reality shows
part_B_data <- nbc_pilotsurvey %>%
  filter(Show %in% c("The Biggest Loser", "The Apprentice: Los Angeles"))

# This calculate means and standard deviations for annoyed
annoyance_stats <- part_B_data %>%
  group_by(Show) %>%
  summarise(mean_annoyed = mean(Q1_Annoyed, na.rm = TRUE),
            sd_annoyed = sd(Q1_Annoyed, na.rm = TRUE),
            n = n())

# The confidence interval for the difference in means
diff_mean_B <- annoyance_stats$mean_annoyed[annoyance_stats$Show == "The Biggest Loser"] - annoyance_stats$mean_annoyed[annoyance_stats$Show == "The Apprentice: Los Angeles"]
se_B <- sqrt((annoyance_stats$sd_annoyed[annoyance_stats$Show == "The Biggest Loser"]^2 / annoyance_stats$n[annoyance_stats$Show == "The Biggest Loser"]) +
               (annoyance_stats$sd_annoyed[annoyance_stats$Show == "The Apprentice: Los Angeles"]^2 / annoyance_stats$n[annoyance_stats$Show == "The Apprentice: Los Angeles"]))

CI_lower_B <- diff_mean_B - 1.96 * se_B
CI_upper_B <- diff_mean_B + 1.96 * se_B

list(diff_mean_B = diff_mean_B, CI_lower_B = CI_lower_B, CI_upper_B = CI_upper_B)
# To Filter data for the show Dancing with the Stars
part_C_data <- nbc_pilotsurvey %>%
  filter(Show == "Dancing with the Stars")

# To calculate the proportion of viewers who found it confusing
confusing_prop <- mean(part_C_data$Q2_Confusing >= 4, na.rm = TRUE)

# The confidence interval for the proportion
n_C <- nrow(part_C_data)
se_C <- sqrt(confusing_prop * (1 - confusing_prop) / n_C)
CI_lower_C <- confusing_prop - 1.96 * se_C
CI_upper_C <- confusing_prop + 1.96 * se_C

list(confusing_prop = confusing_prop, CI_lower_C = CI_lower_C, CI_upper_C = CI_upper_C)
