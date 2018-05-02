library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(scales)
load("data/survey.RData")

tall_table_2.1 <- survey %>%
  gather(key = "demographic", value = "group", home_status, education, age, yrs_in_town) %>%
  group_by(group) %>%
  summarize(freq_vote = mean(local_vote_reg == "Regular voter", na.rm = TRUE)) %>%
  mutate(group = factor(group, levels = c("Homeowner", "Renter",
                                          "Age < 25", "Age 25-39", "Age 40-54", "Age 55-64", "Age 65+",
                                          "< H.S. degree", "H.S. degree", "Some college", "College degree",
                                          "<2 years in town", "2-4 years in town", "5-9 years in town", "10-19 years in town", "20+ years in town"))) %>%
  filter(!(is.na(group)))

ggplot(tall_table_2.1, aes(x = group, y = freq_vote)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  labs(x = "", y = "Percent voting regularly in local elections")


tall_table_2.2 <- survey %>%
  gather(key = "demographic", value = "group", education, age, yrs_in_town) %>%
  group_by(home_status, group) %>%
  summarize(freq_vote = mean(local_vote_reg == "Regular voter", na.rm = TRUE)) %>%
  mutate(group = factor(group, levels = c("Age < 25", "Age 25-39", "Age 40-54", "Age 55-64", "Age 65+",
                                          "< H.S. degree", "H.S. degree", "Some college", "College degree",
                                          "<2 years in town", "2-4 years in town", "5-9 years in town", "10-19 years in town", "20+ years in town"))) %>%
  filter(!(is.na(group)), !(is.na(home_status)))

ggplot(tall_table_2.2, aes(x = group, y = freq_vote, fill = home_status)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5) +
  coord_flip() +
  labs(x = "", y = "Percent voting frequently in local elections", fill = "")

tall_table_2.3 <- survey %>%
  group_by(local_vote_reg, home_status, education) %>%
  summarize(interest = mean(local_pol == "very", na.rm = TRUE)) %>%
  unite(col = "unique", sep = ", ", home_status, local_vote_reg, remove = FALSE) %>%
  mutate(unique = factor(unique, levels = c("Renter, Non-regular voter",
                                            "Homeowner, Non-regular voter",
                                            "Renter, Regular voter",
                                            "Homeowner, Regular voter")))
tall_table_2.3 <- tall_table_2.3[complete.cases(tall_table_2.3), ]

ggplot(tall_table_2.3, aes(x = unique, y = interest, fill = education)) +
  geom_col(position = "dodge2") +
  scale_x_discrete(name="", limits = rev(levels(tall_table_2.3$unique))) +
  scale_fill_discrete(name="", limits = rev(levels(tall_table_2.3$education))) +
  coord_flip() +
  labs(x = "", y = 'Percent "very interested" in local elections')

tall_table_2.5 <- survey %>%
  mutate(knowledge = correct_vote_age & correct_house_maj) %>%
  gather(key = "demographic", value = "group", home_status, education) %>%
  group_by(local_vote_reg, group) %>%
  summarize(knowledge = mean(knowledge)) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("Renter", "Homeowner",
                                          "College degree", "Some college", "H.S. degree", "< H.S. degree")),
         local_vote_reg = factor(local_vote_reg, levels = rev(levels(.$local_vote_reg))))
tall_table_2.5 <- tall_table_2.5[complete.cases(tall_table_2.5), ]

ggplot(tall_table_2.5, aes(x = group, y = knowledge, fill = local_vote_reg)) +
  geom_col(position = "dodge", width = 0.5) +
  coord_flip() +
  labs(x = "", y = "Percent answering correctly", fill = "")

table_summary <- function(x) {
  mean <- round(mean(x, na.rm = TRUE), 1)
  sd <- round(sd(x, na.rm = TRUE), 1)
  paste0(mean, " (", sd, ")")
}

tall_table_2.2b <- survey %>%
  gather(key = "demographic", value = "group", home_status, education) %>%
  group_by(group) %>%
  summarize(gov_spending = table_summary(gov_spending),
            gov_jobs = table_summary(gov_jobs),
            school_prayer = table_summary(school_prayer),
            abortion = table_summary(abortion),
            gay_book_ban = percent(round(mean(gay_book_ban == "YES", na.rm = TRUE), 2))) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("Renter", "Homeowner",
                                          "< H.S. degree", "H.S. degree", "Some college", "College degree"),
                        ordered = TRUE)) %>%
  arrange(group)
j_tab_2.2 <- tall_table_2.2b[complete.cases(tall_table_2.2b), ]
colnames(j_tab_2.2) <- c("",
                         "Govt. Spending",
                         "No Govt. Job",
                         "No Religion in Public K-12",
                         "No Abortion",
                         "Gay Book Ban")

save(j_tab_2.2, file = "plots/j_tab_2.2.RData")
