###### LOAD LIBRARIES
# Shiny
library(shiny)
library(bslib)

# Data cleaning
library(dplyr)
library(tidyr)
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(plotly::layout)

# Visualization
library(ggplot2)
library(gridExtra)
library(plotly)

# Modeling
library(prophet)


##### IMPORT DATA

load_data <- FALSE

if (load_data) {
# import NHANES data: filtering a subset of demographics to reduce memory use in shinyapps.io
df <- read.csv("NHANES_anxiety_cleandata.csv") %>%
  filter(group_category %in% c("total", "age", "sex_at_birth", "gender", "sexual_orientation", "hispanic_origin_and_race", "education", "marital_status",
                               "household_income", "respondent_employed_in_the_last_7_days", "respondent_or_household_member_experienced_loss_of_employment_income_in_last_4_weeks",
                               "active_duty_military", "difficulty_remembering_or_concentrating")) %>% 
  mutate(group_name = case_when(group_category == "active_duty_military*" ~ factor(group_name, levels = c("No active duty service (self or spouse)",
                                                                                                          "Serving in Reserve or National Guard",
                                                                                                          "Serving on active duty",
                                                                                                          "Spouse serving in Reserve or National Guard",
                                                                                                          "Spouse serving on active duty",
                                                                                                          "Did not report")),
                                #group_category == "difficulty_hearing" ~ factor(group_name, levels = c("No difficulty", "Some difficulty", "A lot of difficulty", "Cannot do at all", "Did not report")),
                                group_category == "difficulty_remembering_or_concentrating" ~ factor(group_name, levels = c("No difficulty", "Some difficulty", "A lot of difficulty", "Cannot do at all", "Did not report")),
                                #group_category == "difficulty_seeing" ~ factor(group_name, levels = c("No difficulty", "Some difficulty", "A lot of difficulty", "Cannot do at all", "Did not report")),
                                #group_category == "difficulty_walking_or_climbing_stairs" ~ factor(group_name, levels = c("No difficulty", "Some difficulty", "A lot of difficulty", "Cannot do at all", "Did not report")),
                                #group_category == "difficulty_understanding_or_being_understood" ~ factor(group_name, levels = c("No difficulty", "Some difficulty", "A lot of difficulty", "Cannot do at all", "Did not report")),
                                #group_category == "difficulty_with_self-care" ~ factor(group_name, levels = c("No difficulty", "Some difficulty", "A lot of difficulty", "Cannot do at all", "Did not report")),
                                group_category == "education" ~ factor(group_name, levels = c("Less than high school", "High school or GED",
                                                                                              "Some college/associate’s degree", "Bachelor’s degree or higher")),
                                group_category == "gender" ~ factor(group_name, levels = c("Cisgender female", "Cisgender male", "Transgender", "None of these", "Did not report")),
                                group_category == "household_income" ~ factor(group_name, levels = c( "Less than $25,000", "$25,000 - $34,999", "$35,000 - $49,999",
                                                                                                      "$50,000 - $74,999", "$75,000 - $99,999", "$100,000 - $149,999",
                                                                                                      "$150,000 - $199,999", "$200,000 and above", "Did not report")),
                                #group_category == "lesbian_gay_bisexual_and_transgender" ~ factor(group_name, levels = c("No", "Yes", "Other", "Did not report")),
                                group_category == "marital_status" ~ factor(group_name, levels = c("Never married", "Married", "Divorced/separated", "Widowed", "Did not report")),
                                #group_category == "previous_military_service*" ~ factor(group_name, levels = c("No", "Served in the Reserve or National Guard",
                                #                                                                               "Served on active duty",
                                #                                                                               "Spouse served in the Reserve or National Guard",
                                #                                                                               "Spouse served on active duty",
                                #                                                                               "Did not report")),
                                group_category == "respondent_employed_in_the_last_7_days" ~ factor(group_name, levels = c("Yes", "No", "Did not report")),
                                group_category == "respondent_or_household_member_experienced_loss_of_employment_income_in_last_4_weeks" ~ factor(group_name, levels = c("Yes", "No", "Did not report")),
                                group_category == "sexual_orientation" ~ factor(group_name, levels = c("Straight", "Gay or lesbian", "Bisexual", "Something else", "I don’t know", "Did not report")),
                                TRUE ~ group_name)) %>% 
  mutate(frequency = factor(frequency, levels = c("Not at all", "Several days", "More than half the days", "Nearly every day", "Did not report"))) %>% 
  mutate(mid_date = as.Date(mid_date, format = "%m/%d/%Y"))

saveRDS(df, "NHANES_anxiety_data_short.rds")

}

df <- readRDS("NHANES_anxiety_data_short.rds")


# Demographics group names for UI
group_category_ui_names <- c("total" = "Total Population", "age" = "Age", "sex_at_birth" = "Sex",  "gender" = "Gender Identity", "sexual_orientation" = "Sexual Orientation",
                             "hispanic_origin_and_race" = "Race/Ethnicity", "education" = "Education", "marital_status" = "Marital Status", 
                             "household_income" = "Household Income", "respondent_employed_in_the_last_7_days" = "Employment (Last 7 Days)",
                             "respondent_or_household_member_experienced_loss_of_employment_income_in_last_4_weeks" = "Income Loss (Last 4 Weeks)",
                             "active_duty_military" = "Active Duty Military",
                             "difficulty_remembering_or_concentrating" = "Difficulty Remembering/Concentrating")

# Colors for plotting
frequency_colors <- c("Not at all" = "#F8766D", "Several days" = "#7CAE00", "More than half the days" = "#00BFC4",
                      "Nearly every day" = "#C77CFF", "Did not report" = "#999999")

# Function to create Forecast Plot in the "Forecasting" Panel
make_forecast_plot <- function(prophet_result, group_name, value_type) {
  forecast_data <- bind_rows(lapply(prophet_result, function(x) x$forecast))
  train_data <- bind_rows(lapply(prophet_result, function(x) x$train)) %>%
    mutate(frequency = rep(names(prophet_result), sapply(prophet_result, function(x) nrow(x$train))))
  
  forecast_data$frequency <- factor(forecast_data$frequency,
                                    levels = c("Not at all", "Several days", "More than half the days", "Nearly every day"))
  train_data$frequency <- factor(train_data$frequency,
                                 levels = c("Not at all", "Several days", "More than half the days", "Nearly every day"))
  
  last_train <- train_data %>%
    group_by(frequency) %>%
    summarise(last_date = max(ds), .groups = "drop")
  
  forecast_data <- forecast_data %>%
    left_join(last_train, by = "frequency") %>%
    mutate(ds = as.Date(ds), last_date = as.Date(last_date), segment = ifelse(ds <= last_date, "Historical", "Forecast"))
  
  ggplot() +
    geom_point(data = train_data, aes(x = ds, y = y, color = frequency), size = 1.5) +
    geom_line(data = forecast_data, aes(x = ds, y = yhat, linetype = segment, color = frequency)) +
    geom_ribbon(data = forecast_data %>% filter(segment == "Forecast"),
                aes(x = ds, ymin = yhat_lower, ymax = yhat_upper, fill = frequency),
                alpha = 0.2, show.legend = FALSE) +
    facet_wrap(~frequency, scales = "free_y") +
    scale_color_manual(values = frequency_colors, guide = "none") +
    scale_fill_manual(values = frequency_colors, guide = "none") +
    scale_linetype_manual(values = c("Historical" = "dashed", "Forecast" = "solid"), name = "Data Type") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          panel.border = element_rect(color = "gray80", fill = NA)) +
    labs(title = paste("Forecasts of Anxiety Trends for", group_name),
         x = "Date", y = ifelse(value_type == "percent", "Percent (%)", "Estimated Count"))
  
}

# Function to create Forecast Summary Table in the "Forecasting" Panel
make_summary_table <- function(prophet_result, forecast_months, value_type, show_accuracy = FALSE) {
  summary_output <- lapply(names(prophet_result), function(freq) {
    data <- prophet_result[[freq]]
    forecast <- data$forecast
    train <- data$train
    
    last_forecast <- tail(forecast, forecast_months) %>% tail(1)
    three_months_ago <- train$y[nrow(train) - 3]
    
    if (nrow(train) >= 4) {
      three_months_ago <- train$y[nrow(train) - 3]
      change <- round(100 * (last_forecast$yhat - three_months_ago) / three_months_ago, 2)
    } else {change <- NA}
    mae <- mean(abs(train$y - forecast$yhat[1:nrow(train)]), na.rm = TRUE)
    mape <- mean(abs((train$y - forecast$yhat[1:nrow(train)]) / train$y), na.rm = TRUE) * 100
    
    if (value_type == "percent") {
      forecast_estimate <- round(last_forecast$yhat, 2)
      lower <- round(last_forecast$yhat_lower, 2)
      upper <- round(last_forecast$yhat_upper, 2)
    } else {
      forecast_estimate <- as.integer(round(last_forecast$yhat, 0))
      lower <- as.integer(round(last_forecast$yhat_lower, 0))
      upper <- as.integer(round(last_forecast$yhat_upper, 0))
    }
    
    df <- data.frame(`Response Intensity` = freq,
                     `Forecast Estimate` = forecast_estimate,
                     `Forecast Range` = paste0("[", lower, ", ", upper, "]"),
                     `3-month Change (%)` = change, 
                     check.names = FALSE)
    
    if (show_accuracy) {df$MAE <- round(mae, 2); df$`MAPE (%)` <- round(mape, 2)}
    
    return(df)
  })
  
  do.call(rbind, summary_output)
}
