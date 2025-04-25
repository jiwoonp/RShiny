## About
This repository includes Shiny dashboards for data visualization. Each subfolder includes datasets and R codes used to create the Shiny application.

## Live Apps
### [Covid-19 Dashboard](https://jpark23.shinyapps.io/covid_deaths/)
This interactive Shiny app visualizes trends in COVID-19 deaths, vaccination coverage, and vaccination intent across U.S. regions and demographic groups, using data from the CDC.

**Users can:**
- Explore COVID-19 death trends, vaccination coverage and intent by sex, age, and race across U.S. regions


### [Mental Health Dashboard](https://jpark23.shinyapps.io/mental_health/)
This interactive Shiny app explores how various health-related behaviors, healthcare access, and demographic factors relate to mental health outcomes in the U.S., using data from the Behavioral Risk Factor Surveillance System (BRFSS).

**Users can:**
- Visualize trends across states and over time in overall health, physical activity, substance use, and healthcare access by age, race, education, income, or sex
- Build an ordinal logistic regression model using a subset of simulated individual-level BRFSS data to explore predictors significantly associated with mental health outcomes
- Train a random forest model to explore key predictors of mental health, and estimate the probability of poor mental health based on selected risk factors


### [Anxiety Trends Dashboard](https://jpark23.shinyapps.io/anxiety/)
This Shiny app visualizes trends in anxiety symptoms in the U.S. and generates short-term forecasts using national survey data from the NHANES Household Pulse Survey.

**Users can:**
- Visualize changes in anxiety symptoms over time across demographic groups
- Generate 1–6 months forecasts of anxiety trends using the Prophet time series model, and download plots and forecast summaries