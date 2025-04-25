library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

sheet_names <- excel_sheets("C:/Users/JiwoonPark/OneDrive - Key Proteo/Desktop/RShiny/anxiety/HPS_anxiety_rawdata.xlsx")
wave_to_date <- data.frame(wave = c("Week41", "Week42", "Week43", "Week44", "Week45", "Week46", "Week47", "Week48", "Week49", "Week50",
                                    "Week51", "Week52", "Week53", "Week54", "Week55", "Week56", "Week57", "Week58", "Week59", "Week60",
                                    "Week61", "Week62", "Week63"),
                           start_date = as.Date(c("2021-12-29", "2022-01-26", "2022-03-02", "2022-03-30", "2022-04-27", "2022-06-01", "2022-06-29", "2022-07-27", "2022-09-14", "2022-10-05",
                                                  "2022-11-02", "2022-12-09", "2023-01-04", "2023-02-01", "2023-03-01","2023-03-29", "2023-04-26", "2023-06-07", "2023-06-28", "2023-07-26",
                                                  "2023-08-23", "2023-09-20", "2023-10-18")),
                           end_date = as.Date(c("2022-01-10", "2022-02-07", "2022-03-14", "2022-04-11", "2022-05-09", "2022-06-13", "2022-07-11", "2022-08-08", "2022-09-26", "2022-10-17",
                                                "2022-11-14", "2022-12-19", "2023-01-16", "2023-02-13", "2023-03-13", "2023-04-10", "2023-05-08", "2023-06-19", "2023-07-10", "2023-08-07",
                                                "2023-09-04", "2023-10-02", "2023-10-30")))
wave_to_date$mid_date <- wave_to_date$start_date + ((wave_to_date$end_date - wave_to_date$start_date) / 2)


tidy_data <- function(sheet) {
  
  df <- read_excel("C:/Users/JiwoonPark/OneDrive - Key Proteo/Desktop/RShiny/anxiety/HPS_anxiety_rawdata.xlsx", sheet = sheet, skip = 4) %>%
    filter(!startsWith(`Select Characteristics`, "*"), !is.na(`Select Characteristics`)) %>%
    mutate(across(-`Select Characteristics`, ~ suppressWarnings(as.numeric(gsub(",", "", .))))) %>%
    mutate(is_group_header = if_else(rowSums(!is.na(across(-`Select Characteristics`))) == 0, TRUE, FALSE)) %>%
    pivot_longer(cols = -c(`Select Characteristics`, is_group_header),
                 names_to = "question_freq", values_to = "estimate") %>%
    separate(question_freq, into = c("question", "frequency"), sep = " - ", extra = "merge") %>%
    mutate(question = str_trim(question), frequency = str_trim(frequency), wave = sheet,group_name = `Select Characteristics`) %>%
    select(wave, group_name, question, frequency, estimate, is_group_header)
  
  return(df)
}

all_waves <- map_dfr(sheet_names, tidy_data) %>%
  mutate(group_category = if_else(is_group_header, group_name, NA_character_)) %>%
  fill(group_category, .direction = "down") %>%
  filter(!is_group_header) %>%
  mutate(group_category = if_else(str_to_lower(group_name) == "total", "total", group_category),
         group_category = group_category %>% str_replace_all(",", "") %>% str_remove_all("\\*") %>% str_to_lower() %>% str_replace_all("\\s+", "_"),
         group_name = str_trim(group_name)) %>% 
  select(-is_group_header) %>%  
  left_join(., wave_to_date %>% select(wave, mid_date), by = "wave") 

write.csv(all_waves, "HPS_anxiety_cleandata.csv")

