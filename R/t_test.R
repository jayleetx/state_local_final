library(dplyr)
library(infer)
library(forcats)
load("data/survey.RData")
not_na <- function(vec) !(is.na(vec))
end_levels <- function(factor) c(levels(factor)[1], levels(factor)[length(levels(factor))])

# book ~ home
t_survey <- filter(survey, not_na(home_status), not_na(gay_book_ban))
test_stat <- t_survey %>%
  group_by(home_status) %>%
  summarize(freq = mean(gay_book_ban == "YES")) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(gay_book_ban ~ home_status, success = "YES") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("Renter", "Homeowner"))
p_home_book <- (2*sum(t_samp$stat <= test_stat)+1)/1001

# book ~ vote
t_survey <- filter(survey, not_na(local_vote_reg), not_na(gay_book_ban))
test_stat <- t_survey %>%
  group_by(local_vote_reg) %>%
  summarize(freq = mean(gay_book_ban == "YES")) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(gay_book_ban ~ local_vote_reg, success = "YES") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("Regular voter", "Non-regular voter"))
p_vote_book <- (2*sum(t_samp$stat <= test_stat)+1)/1001

# book ~ education
t_survey <- filter(survey, not_na(gay_book_ban), education %in% end_levels(survey$education))
test_stat <- t_survey %>%
  group_by(education) %>%
  summarize(freq = mean(gay_book_ban == "YES")) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(gay_book_ban ~ education, success = "YES") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("College degree", "< H.S. degree"))
p_edu_book <- (2*sum(t_samp$stat <= test_stat)+1)/1001

# abortion ~ home
t_survey <- filter(survey, not_na(abortion), not_na(home_status))
test_stat <- t_survey %>%
  group_by(home_status) %>%
  summarize(freq = mean(abortion)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(abortion ~ home_status) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Renter", "Homeowner"))
p_home_abort <- (2*sum(t_samp$stat >= test_stat)+1)/1001

# abortion ~ vote
t_survey <- filter(survey, not_na(abortion), not_na(local_vote_reg))
test_stat <- t_survey %>%
  group_by(local_vote_reg) %>%
  summarize(freq = mean(abortion)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(abortion ~ local_vote_reg) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Regular voter", "Non-regular voter"))
p_vote_abort <- (2*sum(t_samp$stat <= test_stat)+1)/1001

# abortion ~ edu
t_survey <- filter(survey, not_na(abortion), education %in% end_levels(survey$education))
test_stat <- t_survey %>%
  group_by(education) %>%
  summarize(freq = mean(abortion)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(abortion ~ education) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("College degree", "< H.S. degree"))
p_edu_abort <- (2*sum(t_samp$stat <= test_stat)+1)/1001

# school_prayer ~ home
t_survey <- filter(survey, not_na(school_prayer), not_na(home_status))
test_stat <- t_survey %>%
  group_by(home_status) %>%
  summarize(freq = mean(school_prayer)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(school_prayer ~ home_status) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Renter", "Homeowner"))
p_home_prayer <- (2*sum(t_samp$stat >= test_stat)+1)/1001

# school_prayer ~ vote
t_survey <- filter(survey, not_na(school_prayer), not_na(local_vote_reg))
test_stat <- t_survey %>%
  group_by(local_vote_reg) %>%
  summarize(freq = mean(school_prayer)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(school_prayer ~ local_vote_reg) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Regular voter", "Non-regular voter"))
p_vote_prayer <- (2*sum(t_samp$stat <= test_stat)+1)/1001

# school_prayer ~ edu
t_survey <- filter(survey, not_na(school_prayer), education %in% end_levels(survey$education))
test_stat <- t_survey %>%
  group_by(education) %>%
  summarize(freq = mean(school_prayer)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(school_prayer ~ education) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("College degree", "< H.S. degree"))
p_edu_prayer <- (2*sum(t_samp$stat >= test_stat)+1)/1001

# gov_jobs ~ home
t_survey <- filter(survey, not_na(gov_jobs), not_na(home_status))
test_stat <- t_survey %>%
  group_by(home_status) %>%
  summarize(freq = mean(gov_jobs)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(gov_jobs ~ home_status) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Renter", "Homeowner"))
p_home_jobs <- (2*sum(t_samp$stat <= test_stat)+1)/1001

# gov_jobs ~ vote
t_survey <- filter(survey, not_na(gov_jobs), not_na(local_vote_reg))
test_stat <- t_survey %>%
  group_by(local_vote_reg) %>%
  summarize(freq = mean(gov_jobs)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(gov_jobs ~ local_vote_reg) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Regular voter", "Non-regular voter"))
p_vote_jobs <- (2*sum(t_samp$stat >= test_stat)+1)/1001

# gov_jobs ~ edu
t_survey <- filter(survey, not_na(gov_jobs), education %in% end_levels(survey$education))
test_stat <- t_survey %>%
  group_by(education) %>%
  summarize(freq = mean(gov_jobs)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(gov_jobs ~ education) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("College degree", "< H.S. degree"))
p_edu_jobs <- (2*sum(t_samp$stat >= test_stat)+1)/1001

# gov_spending ~ home
t_survey <- filter(survey, not_na(gov_spending), not_na(home_status))
test_stat <- t_survey %>%
  group_by(home_status) %>%
  summarize(freq = mean(gov_spending)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(gov_spending ~ home_status) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Renter", "Homeowner"))
p_home_spending <- (2*sum(t_samp$stat >= test_stat)+1)/1001

# gov_spending ~ vote
t_survey <- filter(survey, not_na(gov_spending), not_na(local_vote_reg))
test_stat <- t_survey %>%
  group_by(local_vote_reg) %>%
  summarize(freq = mean(gov_spending)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(gov_spending ~ local_vote_reg) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Regular voter", "Non-regular voter"))
p_vote_spending <- (2*sum(t_samp$stat <= test_stat)+1)/1001

# gov_spending ~ edu
t_survey <- filter(survey, not_na(gov_spending), education %in% end_levels(survey$education))
test_stat <- t_survey %>%
  group_by(education) %>%
  summarize(freq = mean(gov_spending)) %>% 
  select(freq) %>% pull() %>% diff()
t_samp <- t_survey %>%
  specify(gov_spending ~ education) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("College degree", "< H.S. degree"))
p_edu_spending <- (2*sum(t_samp$stat <= test_stat)+1)/1001

p_vals <- data.frame(spending = c(p_home_spending, p_edu_spending, p_vote_spending),
                     jobs = c(p_home_jobs, p_edu_jobs, p_vote_jobs),
                     prayer = c(p_home_prayer, p_edu_prayer, p_vote_prayer),
                     abort = c(p_home_abort, p_edu_abort, p_vote_abort),
                     book = c(p_home_book, p_edu_book, p_vote_book))
save(p_vals, file = "data/p_vals.RData")
