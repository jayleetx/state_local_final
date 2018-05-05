library(foreign)
library(dplyr)
library(stringr)
library(forcats)
load("data/zip_to_pop.RData")

asps_file <- "data/spss_clean/oliverdata.sav"
screener_file <- "data/screener/Data/CitPart_Screener.SAV"
acps <- read.spss(asps_file, to.data.frame=TRUE, add.undeclared.levels = "sort")
screener <- read.spss(screener_file, to.data.frame=TRUE, add.undeclared.levels = "sort")

#population, vote "most" or "all" of the time, length of residence, education, age, home ownership,
# regular/nonreg voter, "very" interested in politics, political knowledge (HOR party, voting age)
acps_vars <- c("screener_id" = "scrnrid",
               "local_vote_reg" = "vtlocall",
               "yrs_in_town" = "townyrs",
               "local_pol" = "locint",
               "gov_spending" = "cutgov7",
               "gov_jobs" = "govjobs7",
               "school_prayer" = "prayer7",
               "abortion" = "abort7",
               "gay_book_ban" = "gaybook")

screener_vars <- c("screener_id" = "SCRNRID",
                   "gender" = "rgender.",
                   "srvy_wt" = "weight.",
                   "home_status" = "ownrent.",
                   "education" = "educ6.",
                   "age" = "age.",
                   "income" = "income.",
                   "correct_vote_age" = "agevote.",
                   "correct_house_maj" =  "majpart.",
                   "zip" = "zip.",
                   "race" = "race.")

sub_acps <- select(acps, acps_vars)
colnames(sub_acps) <- names(acps_vars)
sub_screener <- select(screener, screener_vars) %>%
  filter(SCRNRID %in% sub_acps$screener_id)
colnames(sub_screener) <- names(screener_vars)

full_data <- left_join(sub_screener, sub_acps, by = "screener_id") %>%
  mutate(zip = str_pad(as.character(zip), width = 5, side = "left", pad = "0"),
         age = as.numeric(as.character(age)),
         yrs_in_town = as.numeric(as.character(yrs_in_town))) %>%
  left_join(zip_to_pop, by = "zip") %>%
  filter(pop_1990 < 100000) %>%
  select(-zip, -pop_1990, -screener_id)

survey <- full_data %>%
  mutate(home_status = fct_collapse(home_status,
                                  NULL = c("buying", "other", "refused", "don't know", "missing"),
                                  "Renter" = "no rent",
                                  "Renter" = "rent",
                                  "Homeowner" = "own"),
         education = fct_recode(education,
                                "< H.S. degree" = "8th grade or less",
                                "< H.S. degree" = "9 - 12, no diploma o",
                                "H.S. degree" = "diploma or ged",
                                "Some college" = "some college / 2 yr",
                                "College degree" = "ba",
                                "College degree" = "advanced degree",
                                NULL = "missing"),
         age = cut(age, breaks = c(0,24,39,54,64,100), labels = c("Age < 25",
                                                   "Age 25-39",
                                                   "Age 40-54",
                                                   "Age 55-64",
                                                   "Age 65+")),
         correct_vote_age = as.numeric(as.character(fct_recode(correct_vote_age,
                                                               "1" = "correct (said 18 yrs old)",
                                                               "0" = "don't know",
                                                               "0" = "incorrect (gave incorrect age)"))),
         correct_house_maj = as.numeric(as.character(fct_recode(correct_house_maj,
                                                               "1" = "correct (said Democrats)",
                                                               "0" = "don't know",
                                                               "0" = "incorrect (said Republicans)"))),
         local_vote_reg = fct_collapse(local_vote_reg,
                                 "Regular voter" = c("ALL", "MOST"),
                                 "Non-regular voter" = c("SOME", "RARELY", "NEVER","NOT OLD ENOUGH",
                                                   "never eligible to vote")),
         yrs_in_town = cut(yrs_in_town, breaks = c(0,1,4,9,19, 100), labels = c("<2 years in town",
                                                                           "2-4 years in town",
                                                                           "5-9 years in town",
                                                                           "10-19 years in town",
                                                                           "20+ years in town")),
         local_pol = fct_recode(local_pol,
                                "not" = "NOT INTERESTED",
                                "slightly" = "SLIGHTLY INTERESTED",
                                "somewhat" = "SOMEWHAT INTERESTED",
                                "very" = "VERY INTERESTED"),
         gov_spending = as.numeric(as.character(fct_recode(gov_spending,
                                                           "1" = "REDUCE SPENDING",
                                                           "7" = "INCREASE SPENDING"))),
         gov_jobs = as.numeric(as.character(fct_recode(gov_jobs,
                                                       "1" = "govt provide jobs",
                                                       "7" = "ONE GET AHEAD ON OWN"))),
         school_prayer = as.numeric(as.character(fct_recode(school_prayer,
                                                            "1" = "START DAY W/PRAYER",
                                                            "7" = "DOES NOT BELONG"))),
         abortion = as.numeric(as.character(fct_recode(abortion,
                                                       "1" = "PERSONAL CHOICE",
                                                       "7" = "NEVER PERMITTED")))
         )

save(survey, file = "data/survey.RData")
