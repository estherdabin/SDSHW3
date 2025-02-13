library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

ebay <- read_csv("C:/Users/esthe/OneDrive/Documents/SDS313/ebay.csv")
#Question4

# The creates the revenue ratio variable
ebay <- ebay %>%
  mutate(rev_ratio = rev_after / rev_before)
# To calculate the difference in mean revenue ratio between treatment and control groups
control_group <- ebay %>% filter(adwords_pause == 0)
treatment_group <- ebay %>% filter(adwords_pause == 1)
# The difference in revenue ratio means
obs_diff <- mean(treatment_group$rev_ratio) - mean(control_group$rev_ratio)

# Estimate the p-value and confidence interval
set.seed(123)
n_simulations <- 10000
simulated_diffs <- numeric(n_simulations)
for(i in 1:n_simulations) {
  shuffled_ebay <- ebay %>%
    mutate(adwords_pause = sample(adwords_pause))
  
  control_group_sim <- shuffled_ebay %>% filter(adwords_pause == 0)
  treatment_group_sim <- shuffled_ebay %>% filter(adwords_pause == 1)
  
  simulated_diffs[i] <- mean(treatment_group_sim$rev_ratio) - mean(control_group_sim$rev_ratio)
}

# To calculate the 95% confidence interval for the difference
conf_interval <- quantile(simulated_diffs, probs = c(0.025, 0.975))

list(observed_difference = obs_diff, confidence_interval = conf_interval)

cat("Difference in revenue ratio:", obs_diff, "\n")
cat("95% Confidence interval for the difference:", conf_interval, "\n")
