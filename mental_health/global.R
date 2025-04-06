###### LOAD LIBRARIES
# Shiny
library(shiny)
library(shinyjs)
library(shinycssloaders)

# Data cleaning
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)

# Visualization
library(ggplot2)
library(plotly)

# Modeling
library(MASS)
library(broom)


##### IMPORT DATA
## for shinyapps.io deployment, using subset of original data file ("BRFSS Data short.csv") to avoid memory issues

load_data <- FALSE

if (load_data) {
# import BRFSS data
df <- read.csv("BRFSS Prevalence Data 2011 to present.csv") %>% 
  filter(Topic %in% c("Depression", "Disability status", "Healthy Days", "Overall Health",
                      "Aerobic Activity", "Exercise", "Physical Activity Index", "Strength Activity",
                      "Alcohol Consumption", "Binge Drinking", "Heavy Drinking", "Current Smoker Status", "Smokeless Tobacco",
                      "Income", "Education", "Employment",
                      "Health Care Cost", "Health Care Coverage", "Personal Care Provider")) %>% 
  mutate(Topic = case_when(
    Topic == "Disability status" & Question == "Do you have serious difficulty concentrating, remembering, or making decisions?" ~ "Mental disability",
    Topic == "Disability status" & Question == "Adults who are limited in any activities because of physical, mental, or emotional problems" ~ "Disability",
    Topic == "Disability status" ~ "Physical disability",
    TRUE ~ Topic)) %>% 
  mutate(Response = case_when(
    Topic == "Heavy Drinking" & Response == "Meet criteria for heavy drinking" ~ "Yes",
    Topic == "Heavy Drinking" & Response == "Do not meet criteria for heavy drinking" ~ "No", 
    TRUE ~ Response)) %>% 
  mutate(Response = case_when(
    Topic == "Smokeless Tobacco" & Response %in% c("Every day", "Some days") ~ "Yes",
    Topic == "Smokeless Tobacco" & Response == "Not at all" ~ "No",
    TRUE ~ Response)) %>% 
  mutate(Response = case_when(
    Topic == "Health Care Coverage" & Response == "Have some form of health insurance" ~ "Yes",
    Topic == "Health Care Coverage" & Response == "Do not have some form of health insurance" ~ "No", 
    TRUE ~ Response)) %>% 
  mutate(Response = case_when(
    Topic == "Personal Care Provider" & Response %in% c("Yes, only one", "More than one") ~ "Yes",
    TRUE ~ Response)) %>% 
  select(Year, Locationdesc, Topic, Response, Break_Out, Break_Out_Category, Sample_Size, Data_value, Confidence_limit_Low, Confidence_limit_High)   # activate this row to extract "BRFSS Data short.csv" file

}

#write.csv(df, "BRFSS Data short.csv")
df <- read.csv("BRFSS Data short.csv")

# function for plotting: change plot height depending on demographics checklist

checklist_plot_layout <- function(df, facet_var = "Break_Out", min_cols = 2, panel_height = 300) {
  num_panels <- length(unique(df[[facet_var]]))
  num_cols <- ifelse(num_panels <= 2, 1, min_cols)
  num_rows <- ceiling(num_panels / num_cols)
  plot_height <- panel_height * num_rows
  
  list(num_panels = num_panels, num_cols = num_cols, num_rows = num_rows,plot_height = plot_height)
}

# function for plotting: setting factor levels

set_breakout_levels <- function(df, category) {
  df %>%
    mutate(Break_Out = case_when( 
      Break_Out_Category == "Education Attained" ~ factor(Break_Out, levels = c("Less than H.S.", "H.S. or G.E.D.", "Some post-H.S.", "College graduate")),
      Break_Out_Category == "Household Income" ~ factor(Break_Out, levels = c("Less than $15,000", "$15,000-$24,999", "$25,000-$34,999", "$35,000-$49,999",
                                                                              "$50,000+", "$50,000-$99,999", "$100,000-$199,999", "$200,000+")),
      TRUE ~ factor(Break_Out))) %>% 
    mutate(Break_Out = droplevels(Break_Out))
}

# US states for sidebar map
us_states <- data.frame(state_name = state.name, state_abb = state.abb, stringsAsFactors = FALSE)

##########################################################

# restructuring data for ordinal logistic regression

load_simulation_data <- FALSE

if (load_simulation_data) {
outcome_topic <- "Healthy Days"
predictor_topics <- c("Depression", "Overall Health", "Aerobic Activity","Strength Activity",
                      "Alcohol Consumption", "Binge Drinking", "Heavy Drinking", "Current Smoker Status", "Smokeless Tobacco",
                      "Health Care Cost", "Health Care Coverage", "Primary Care Provider")
all_topics <- c(outcome_topic, predictor_topics)

df_olrmodel <- df %>% filter(Year >= 2019) %>%
  filter(Locationdesc %in% state.name) %>%
  filter(Topic %in% all_topics) %>%
  filter(!is.na(Sample_Size), !is.na(Data_value)) %>%
  mutate(Response = case_when(
    Topic == "Healthy Days" & Response == "Zero days when mental health not good" ~ "0 days",
    Topic == "Healthy Days" & Response == "1-13 days when mental health not good" ~ "1–13 days",
    Topic == "Healthy Days" & Response == "14+ days when mental health not good" ~ "14+ days",
    Topic == "Healthy Days" & Response == "Zero days when physical health not good" ~ "phys_0",
    Topic == "Healthy Days" & Response == "1-13 days when physical health not good" ~ "phys_1_13",
    Topic == "Healthy Days" & Response == "14+ days when physical health not good" ~ "phys_14",
    TRUE ~ Response)) 

df_olrmodel_wide <- df_olrmodel %>%
  dplyr::select(Year, Locationdesc, Break_Out_Category, Break_Out, Topic, Response, Sample_Size, Data_value) %>%
  unite("GroupID", Year, Locationdesc, Break_Out_Category, Break_Out, sep = "|", remove = FALSE) %>%
  mutate(var_name = paste0(Topic, "_", Response)) %>%
  dplyr::select(GroupID, var_name, Data_value, Sample_Size, Year, Locationdesc, Break_Out_Category, Break_Out) %>%
  pivot_wider(names_from = var_name, values_from = Data_value, values_fn = mean) %>%
  group_by(GroupID, Year, Locationdesc, Break_Out_Category, Break_Out) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")


df_indiv <- function(row) {
  row <- as.list(row)
  n <- as.integer(row$Sample_Size)
  if (is.na(n) || n == 0) return(NULL)
  
  outcome_probability <- c(`0 days` = row[["Healthy Days_0 days"]], `1–13 days` = row[["Healthy Days_1–13 days"]], `14+ days` = row[["Healthy Days_14+ days"]])/100
  if (any(is.na(outcome_probability)) || sum(outcome_probability, na.rm = TRUE) == 0) return(NULL)

  outcome_counts <- round(outcome_probability * n)
  diff <- n - sum(outcome_counts)
  outcome_counts[which.max(outcome_counts)] <- outcome_counts[which.max(outcome_counts)] + diff
  mental_health <- unlist(mapply(rep, names(outcome_counts), outcome_counts, SIMPLIFY = FALSE))
  
  df_sim <- data.frame(
    mental_health = factor(mental_health, levels = c("0 days", "1–13 days", "14+ days"), ordered = TRUE),
    year = row$Year, 
    state = row$Locationdesc)
  col_name <- make.names(tolower(gsub(" ", "_", row$Break_Out_Category)))
  df_sim[[col_name]] <- row$Break_Out

  predictor_columns <- names(row)[grepl("_Yes|_No|_Fair|_Good|_Poor|_Excellent|_Very good|phys_", names(row))]
  for (col in predictor_columns) {
    if (!is.na(row[[col]])) {
      prob <- row[[col]] / 100
      values <- rbinom(nrow(df_sim), 1, prob)
      topic_name <- make.names(col)
      df_sim[[topic_name]] <- factor(ifelse(values == 1, "Yes", "No"))
    }
  }
  
  return(df_sim)
}

simulated_data_list <- purrr::map(1:nrow(df_olrmodel_wide), ~ df_indiv(as.list(df_olrmodel_wide[.x, ])))
df_olrmodel_sim <- bind_rows(simulated_data_list)
saveRDS(df_olrmodel_sim, "simulated_brfss_data_for_olrmodel.rds")
}


## for shinyapps.io deployment, selecting a subset of state/year to avoid memory issues

#df_olrmodel_sim <- readRDS("simulated_brfss_data_for_olrmodel.rds")

#for (state in c("California", "Texas", "New York", "Alabama")) {
#   df_select <- df_olrmodel_sim %>%
#    filter(state == state, year == 2022) %>%
#    drop_na(mental_health) %>% 
#    sample_n(100000)
#   
#  saveRDS(df_select, paste0("olrmodel_data_", tolower(state), "_2022_subset.rds"))
#}

#########
# for formatting model results in a table

get_reference_group <- function(term, ref_levels) {
  possible_vars <- names(ref_levels)
  matched_var <- possible_vars[which.max(sapply(possible_vars, function(p) startsWith(term, p)))]
  
  if (length(matched_var) == 0 || is.na(matched_var)) {return(NA)}
  return(ref_levels[[matched_var]])
}

term_labels <- c(
  "sexMale" = "Sex: Male", "sexFemale" = "Sex: Female",
  "age_group18-24" = "Age: 18–24", "age_group25-34" = "Age: 25–34", "age_group35-44" = "Age: 35–44", "age_group45-54" = "Age: 45–54", "age_group55-64" = "Age: 55–64", "age_group65+"    = "Age: 65+",
  "education_attainedCollege graduate" = "Education: College Grad", "education_attainedH.S. or G.E.D." = "Education: High School", "education_attainedLess than H.S." = "Education: < High School", "education_attainedSome post-H.S." = "Education: Some College",
  "household_incomeLess than $15,000" = "Income: < $15k", "household_income$15,000-$24,999" = "Income: $15k–24k", "household_income$25,000-$34,999" = "Income: $25k–34k", "household_income$35,000-$49,999" = "Income: $35k–49k",
  "household_income$50,000+" = "Income: $50k+", "household_income$50,000-$99,999" = "Income: $50k–99k", "household_income$100,000-$199,999" = "Income: $100k–199k", "household_income$200,000+" = "Income: $200k+",
  "race.ethnicityWhite, non-Hispanic" = "Race: White (NH)", "race.ethnicityBlack, non-Hispanic" = "Race: Black (NH)", "race.ethnicityAsian, non-Hispanic" = "Race: Asian (NH)",
  "race.ethnicityAmerican Indian or Alaskan Native, non-Hispanic" = "Race: AI/AN (NH)", "race.ethnicityNative Hawaiian or other Pacific Islander, non-Hispanic" = "Race: NH/PI (NH)",
  "race.ethnicityMultiracial, non-Hispanic" = "Race: Multiracial (NH)", "race.ethnicityHispanic" = "Race: Hispanic", "race.ethnicityOther, non-Hispanic" = "Race: Other (NH)",
  "Overall.Health_ExcellentYes" = "Overall Health: Excellent", "Overall.Health_Very.goodYes" = "Overall Health: Very Good", "Overall.Health_GoodYes" = "Overall Health: Good",
  "Overall.Health_FairYes" = "Overall Health: Fair", "Overall.Health_PoorYes" = "Overall Health: Poor",
  "Healthy.Days_phys_0Yes" = "Physical Health: 0 Bad Days", "Healthy.Days_phys_1_13Yes" = "Physical Health: 1–13 Bad Days", "Healthy.Days_phys_14Yes" = "Physical Health: 14+ Bad Days",
  "Depression_YesYes" = "Depression Diagnosis",
  "Aerobic.Activity_YesYes" = "Aerobic Activity", "Strength.Activity_YesYes" = "Strength Training",
  "Alcohol.Consumption_YesYes" = "Alcohol Consumption", "Binge.Drinking_YesYes" = "Binge Drinking", "Heavy.Drinking_YesYes" = "Heavy Drinking",
  "Current.Smoker.Status_YesYes" = "Smoker", "Smokeless.Tobacco_YesYes" = "Smokeless Tobacco",
  "Health.Care.Coverage_YesYes" = "Has Healthcare Coverage",
  "Health.Care.Cost_YesYes" = "Cost Barrier to Care",
  "Personal.Care.Provider_YesYes" = "Has Primary Care Provider"
)

predictor_labels <- c(
  "sex" = "Sex", "age_group" = "Age", "education_attained" = "Education", "household_income" = "Income", "race.ethnicity" = "Race", 
  "Overall.Health_Excellent" = "Overall Health: Excellent", "Overall.Health_Very.good" = "Overall Health: Very Good", "Overall.Health_Good" = "Overall Health: Good",
  "Overall.Health_Fair" = "Overall Health: Fair", "Overall.Health_Poor" = "Overall Health: Poor",
  "Healthy.Days_phys_0" = "Physical Health: 0 Bad Days", "Healthy.Days_phys_1_13" = "Physical Health: 1–13 Bad Days", "Healthy.Days_phys_14" = "Physical Health: 14+ Bad Days",
  "Depression_Yes" = "Depression Diagnosis",
  "Aerobic.Activity_Yes" = "Aerobic Activity", "Strength.Activity_Yes" = "Strength Training",
  "Alcohol.Consumption_Yes" = "Alcohol Consumption", "Binge.Drinking_Yes" = "Binge Drinking", "Heavy.Drinking_Yes" = "Heavy Drinking",
  "Current.Smoker.Status_Yes" = "Smoker", "Smokeless.Tobacco_YesYes" = "Smokeless Tobacco",
  "Health.Care.Coverage_Yes" = "Has Healthcare Coverage", "Health.Care.Cost_Yes" = "Cost Barrier to Care", "Personal.Care.Provider_Yes" = "Has Primary Care Provider"
)


#######################################################################################

# selecting relevant data from raw BRFSS dataset

# Demographics (Break_Out_Category): "Age Group", "Education Attained", "Household Income", "Overall", "Race/Ethnicity", "Sex" 
# "Overall" is a summary of each break out category (e.g. male + female = overall)

# 1. Questions related to depression
# (1) "Ever told you that you have a form of depression?" (Topic == "Depression")
# (2) "Do you have serious difficulty concentrating, remembering, or making decisions?" (Topic == "Disability status")  # data starting from 2013
# (3) "Days when mental health status not good (variable calculated from one or more BRFSS questions)" (Topic == "Healthy Days") # data starting from 2019
# (4) "How is your general health?" (Topic == "Overall Health")

# 2. Questions related to physical activity levels
# (1) "Participated in 150 minutes or more of Aerobic Physical Activity per week (variable calculated from one or more BRFSS questions)" (Topic == "Aerobic Activity")  # data from odd years starting from 2011
# (2) "During the past month, did you participate in any physical activities? (variable calculated from one or more BRFSS questions)" (Topic == "Exercise")
# (3) "Participated in enough Aerobic and Muscle Strengthening exercises to meet guidelines (variable calculated from one or more BRFSS questions)" (Topic == "Physical Activity Index")   # data from odd years starting from 2011
# (4) "Participated in muscle strengthening exercises two or more times per week (variable calculated from one or more BRFSS questions)" (Topic == "Strength Activity")   # data from odd years starting from 2011

# 3. Questions related to substance use
# (1) "Adults who have had at least one drink of alcohol within the past 30 days" (Topic == "Alcohol Consumption")
# (2) "Binge drinkers (males having five or more drinks on one occasion, females having four or more drinks on one occasion) (variable calculated from one or more BRFSS questions)" (Topic == "Binge Drinking")
# (3) "Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week) (variable calculated from one or more BRFSS questions)" (Topic == "Heavy Drinking")
# (4) "Adults who are current smokers (variable calculated from one or more BRFSS questions)" (Topic == "Current Smoker Status")
# (5) "Do you currently use chewing tobacco, snuff, or snus?" (Topic == "Smokeless Tobacco")  # data starting from 2013

# 4. Questions related to socioeconomic factors
# (1) "What is your annual household income?" (Topic == "Income")
# (2) "What is the highest grade or year of school you completed?" (Topic == "Education")
# (3) "What is your employment status?" (Topic == "Employment")

# 5. Questions related to health access
# (1) "Was there a time in the past 12 months when you needed to see a doctor but could not because you could not afford it?" (Topic == "Health Care Cost")  # data starting from 2013
# (2) "Adults who had some form of health insurance (variable calculated from one or more BRFSS questions)" (Topic == "Health Care Coverage")
# (3) "Do you have one person (or a group of doctors) that you think of as your personal health care provider?" (Topic == "Personal Care Provider")