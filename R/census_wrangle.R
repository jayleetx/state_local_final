library(readxl)
library(dplyr)
library(foreign)

setClass("num.with.commas")
setAs("character", "num.with.commas", 
      function(from) as.numeric(gsub(",", "", from) ) )
setClass("char.with.dots")
setAs("character", "char.with.dots", 
      function(from) gsub("\\.", "", from) )

city_pop <- read.csv("data/census/city_pop.csv",
                      colClasses=c(rep('character', 6),
                                   'factor','character',
                                   rep('num.with.commas', 6)),
                     na.strings = c("NA", "")) %>%
  filter(summary_level == 157) %>%
  mutate(unique_place = paste(place, state, sep = "_"))

zip_to_zcta <- read_xlsx("data/census/zip_to_zcta.xlsx") %>%
  select(ZIP_CODE, ZCTA) %>%
  mutate(ZCTA = as.numeric(ZCTA))

zcta_to_place <- read.csv("data/census/zip_to_city.csv") %>%
  select(1:3,5)

zip_to_place <- left_join(zip_to_zcta, zcta_to_place, by = c("ZCTA" = "ZCTA5")) %>%
  mutate(unique_place = paste(PLACE, STATE, sep = "_"))

zip_to_pop <- inner_join(zip_to_place, city_pop, by = "unique_place") %>%
  transmute(zip = ZIP_CODE,
            pop_1990 = census_1990) %>%
  group_by(zip) %>%
  summarize(pop_1990 = max(pop_1990))

save(zip_to_pop, file = "data/zip_to_pop.RData")
