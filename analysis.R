# Loading Libraries
library(tidyverse)
library(janitor)

# Cleaning data. Note that `jailed_pop.csv` is not provided in the repository.
df <- read_csv("jailed_pop.csv") %>%
  clean_names() %>%
  mutate(
    arrest_date = as.Date(arrest_date, format = "%m/%d/%Y"),
    dob = as.Date(dob, format = "%m/%d/%Y"),
    scraped_at = as.Date(scraped_at, format = "%Y-%m-%d %H:%M:%S"),
    time_since_arrested = scraped_at - arrest_date,
    arrest_age = floor(as.numeric(difftime(arrest_date, dob, units = "days")) / 365.25),
    age_category = case_when(
      arrest_age < 18 ~ "Under 18",
      arrest_age >= 18 & arrest_age <= 24 ~ "18-24",
      arrest_age >= 25 & arrest_age <= 34 ~ "25-34",
      arrest_age >= 35 & arrest_age <= 44 ~ "35-44",
      arrest_age >= 45 & arrest_age <= 54 ~ "45-54",
      arrest_age >= 55 ~ "55+",
      TRUE ~ NA_character_
    )
  ) %>%
  select(jail = parish_jails, source_url, race, gender, arrest_age, age_category, time_since_arrested, scraped_at, source_url) %>%
  mutate(time_since_arrested = ifelse(time_since_arrested < 0, NA, time_since_arrested))
  
# Saving data
write_csv(df, paste0("individuals_in_jail_", df$scraped_at[1] %>% str_replace_all(., "-","_"), ".csv"))

# Getting demographic breakdown
demographics <- df %>%
  group_by(jail) %>%
  summarize(
    data_date = unique(scraped_at),
    total = n(),
    total_race = sum(!is.na(race)),
    total_gender = sum(!is.na(gender)),
    total_age = sum(!is.na(arrest_age)),
    
    black = sum(race == "Black", na.rm = TRUE),
    white = sum(race == "White", na.rm = TRUE),
    aapi = sum(race == "Asian/PacificIslander", na.rm = TRUE),
    native = sum(race == "American Indian/Alaska Native", na.rm = TRUE),
    race_unknown = sum(race == "Unknown", na.rm = TRUE),
    race_na = sum(is.na(race)),
    
    female = sum(gender == "Female", na.rm = TRUE),
    male = sum(gender == "Male", na.rm = TRUE),
    gender_unknown = sum(gender == "Unknown", na.rm = TRUE),
    gender_na = sum(is.na(gender)),
    
    age_under_18 = sum(age_category == "Under 18", na.rm = TRUE),
    age_18_24 = sum(age_category == "18-24", na.rm = TRUE),
    age_25_34 = sum(age_category == "25-34", na.rm = TRUE),
    age_35_44 = sum(age_category == "35-44", na.rm = TRUE),
    age_45_54 = sum(age_category == "45-54", na.rm = TRUE),
    age_55_plus = sum(age_category == "55+", na.rm = TRUE),
    
    pct_black = round(black / total_race * 100, 2),
    pct_white = round(white / total_race * 100, 2),
    pct_aapi = round(aapi / total_race * 100, 2),
    pct_native = round(native / total_race * 100, 2),
    pct_race_unknown = round(race_unknown / total_race * 100, 2),
    pct_race_na = round(race_na / total * 100, 2),
    
    pct_female = round(female / total_gender * 100, 2),
    pct_male = round(male / total_gender * 100, 2),
    pct_gender_unknown = round(gender_unknown / total_gender * 100, 2),
    pct_gender_na = round(gender_na / total * 100, 2),
    
    pct_age_under_18 = round(age_under_18 / total_age * 100, 2),
    pct_age_18_24 = round(age_18_24 / total_age * 100, 2),
    pct_age_25_34 = round(age_25_34 / total_age * 100, 2),
    pct_age_35_44 = round(age_35_44 / total_age * 100, 2),
    pct_age_45_54 = round(age_45_54 / total_age * 100, 2),
    pct_age_55_plus = round(age_55_plus / total_age * 100, 2)
  )

# Saving the demographic breakdown
write_csv(demographics, paste0("demographics_in_jail_", df$scraped_at[1] %>% str_replace_all(., "-","_"), ".csv"))

# Converting to markdown table using code from
# “Little Useless-Useful R Functions – Transforming Dataframe to Markdown Table | R-Bloggers.”
# www.r-bloggers.com/2023/03/little-useless-useful-r-functions-transforming-dataframe-to-markdown-table/.
df_2_MD <- function(your_df){
  cn <- as.character(names(your_df))
  headr <- paste0(c("", cn),  sep = "|", collapse='')
  sepr <- paste0(c('|', rep(paste0(c(rep('-',3), "|"), collapse=''),length(cn))), collapse ='')
  st <- "|"
  for (i in 1:nrow(your_df)){
    for(j in 1:ncol(your_df)){
      if (j%%ncol(your_df) == 0) {
        st <- paste0(st, as.character(your_df[i,j]), "|", "\n", "" , "|", collapse = '')
      } else {
        st <- paste0(st, as.character(your_df[i,j]), "|", collapse = '')
      }
    }
  }
  fin <- paste0(c(headr, sepr, substr(st,1,nchar(st)-1)), collapse="\n")
  cat(fin)
} 

df_2_MD(demographics)