library(glmnet)
load("data/survey.RData")

logit <- glm(local_vote_reg ~ home_status + education + age, data = survey, family = "binomial")
summary(logit)

home_logit <- glm(local_vote_reg ~ home_status, data = survey, family = "binomial")
edu_logit <- glm(local_vote_reg ~ education, data = survey, family = "binomial")
age_logit <- glm(local_vote_reg ~ age, data = survey, family = "binomial")

aics <- c("home_status" = home_logit$aic, "education" = edu_logit$aic, "age" = age_logit$aic)

names(aics)[order(aics)]


voting_issues <- survey %>%
  group_by(local_vote_reg) %>%
  summarize(gov_spending = table_summary(gov_spending),
            gov_jobs = table_summary(gov_jobs),
            school_prayer = table_summary(school_prayer),
            abortion = table_summary(abortion),
            gay_book_ban = percent(round(mean(gay_book_ban == "YES", na.rm = TRUE), 2))) %>%
  arrange(local_vote_reg)
voting_issues <- voting_issues[complete.cases(voting_issues), ]
colnames(voting_issues) <- c("voting_habit",
                             "Govt. Spending",
                             "No Govt. Job",
                             "No Religion in Public K-12",
                             "No Abortion",
                             "Gay Book Ban")
