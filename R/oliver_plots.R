library(ggplot2)
library(dplyr)
library(tidyr)
load("data/survey.RData")

tall_table <- survey %>%
  gather(key = "demographic", value = "group", home_status, education, age, yrs_in_town) %>%
  group_by(group) %>%
  summarize(freq_vote = mean(local_vote_reg, na.rm = TRUE)) %>%
  mutate(group = factor(group, levels = c("Homeowner", "Renter",
                                          "Age < 25", "Age 25-39", "Age 40-54", "Age 55-64", "Age 65+",
                                          "< H.S. degree", "H.S. degree", "Some college", "College degree",
                                          "<2 years in town", "2-4 years in town", "5-9 years in town", "10-19 years in town", "20+ years in town"))) %>%
  filter(!(is.na(group)))

ggplot(tall_table, aes(x = group, y = freq_vote)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  labs(x = "", y = "Percent voting regularly in local elections")
