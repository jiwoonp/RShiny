library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinyjs)
library(stringr)

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
  select(Year, Locationdesc, Topic, Response, Break_Out, Break_Out_Category, Data_value, Confidence_limit_Low, Confidence_limit_High)

}

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


# select relevant data

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