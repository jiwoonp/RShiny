library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

##### IMPORT DATA

# import covid deaths data
df <- read.csv("covid_deaths.csv")
df$data_period_start <- as.Date(df$data_period_start, format = "%m/%d/%Y")

age_levels <- c("0-4 years", "5-11 years", "12-17 years", "18-29 years", "30-39 years", "40-49 years", "50-64 years", "65-74 years", "75+ years")

df <- df %>% 
  mutate(subgroup1 = recode(subgroup1,
                            "Non-Hispanic American Indian or Alaska Native" = "American Indian or Alaska Native",
                            "Non-Hispanic Asian" = "Asian",
                            "Non-Hispanic Black" = "Black",
                            "Non-Hispanic Native Hawaiian or Other Pacific Islander" = "Native Hawaiian or Other Pacific Islander",
                            "Non-Hispanic White" = "White")) %>%
  mutate(subgroup1 = case_when(
    group == "Age" ~ factor(subgroup1, levels = age_levels, ordered = TRUE), 
    TRUE ~ as.character(subgroup1)))

# import covid vaccination coverage data
vac_cov_df <- read.csv("covid_vaccination_coverage_intent.csv") %>% 
  dplyr::filter(Geographic.Level == "National") %>% 
  mutate(Geographic.Name = recode(Geographic.Name, "National" = "United States")) %>% 
  mutate(Week_ending = gsub(" .*$", "", Week_ending)) %>% 
  mutate(Week_ending = as.Date(Week_ending, format = "%m/%d/%Y")) %>% 
  mutate(Demographic.Name = recode(Demographic.Name,
                                   "American Indian/Alaska Native, Non-Hispanic" = "American Indian or Alaska Native",
                                   "Asian, Non-Hispanic" = "Asian",
                                   "Black, Non-Hispanic" = "Black",
                                   "Pacific Islander/Native Hawaiian, Non-Hispanic" = "Native Hawaiian or Other Pacific Islander",
                                   "White, Non-Hispanic" = "White",
                                   "Multiple Race/Other (Excludes Asian, AIAN, PI/NH), Non-Hispanic" = "Multiple Race, Non-Hispanic")) %>% 
  filter(Demographic.Name != "65+ years" & Demographic.Name != "60+ years" & Demographic.Name != "18-49 years") %>%
  mutate(Demographic.Name = case_when(
    Demographic.Level == "Age" ~ factor(Demographic.Name, levels = age_levels, ordered = TRUE), 
    TRUE ~ as.character(Demographic.Name))) 

df2 <- vac_cov_df %>% 
  filter(indicator_label == "Up-to-date")

df3 <- vac_cov_df %>% 
  filter(indicator_label == "4-level vaccination and intent")
