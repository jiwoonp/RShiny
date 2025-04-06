##### User Interface Object

shared_sidebar <- function(year_id, demographics_id, location_id, map_output_id, filter_ui_output_id){
  tagList(
    sliderInput(
      inputId = year_id, label = "Select Year Range:", 
      min = min(df$Year, na.rm = TRUE), max = max(df$Year, na.rm = TRUE),
      value = c(min(df$Year), max(df$Year)),
      step = 1, sep = ""
    ),
    
    radioButtons(
      inputId = demographics_id, label = "Demographics:",
      choices = list(
        "Sex" = "Sex", "Age" = "Age Group", "Race" = "Race/Ethnicity", 
        "Education Level" = "Education Attained", "Income" = "Household Income"
      ), inline = TRUE
    ),
    
    selectizeInput(
      inputId = location_id, label = "Select State (click map or choose from dropdown):",
      choices = c("", state.name), selected = "Alabama", options = list(placeholder = 'Type a state')
    ),
    
    plotlyOutput(map_output_id, height = "200px"),
    
    uiOutput(filter_ui_output_id)
  )
}

ui <- fluidPage(
  useShinyjs(),
  
  tags$style(HTML("summary.hover-summary {color: #337ab7;cursor: pointer;} summary.hover-summary:hover {text-decoration: underline;}")),
  
  titlePanel("Factors that may affect mental health"), 
  
  tabsetPanel(
    id = "tabs",
    
    # Introduction Tab
    tabPanel("Introduction",
             tags$summary(style = "margin-top: 15px;"),
             p("This Shiny app explores various factors that may influence mental health. Use the tabs above to navigate through different data visualizations and analyses."),
             tags$ul(
               tags$li(strong("General and Mental Health:"), " Self-reported overall health and number of mentally unhealthy days, and clinical depression diagnosis"),
               tags$li(strong("Physical Activity:"), " Reported participation in weekly aerobic activity (150+ minutes) and strength training (2+ times)"),
               tags$li(strong("Substance Use:"), " Self-reported use of alcohol and tobacco products"),
               tags$li(strong("Health Access:"), " Health care coverage, cost barriers, and access to primary care providers")
             ),
             tags$p(tags$a("Data from BRFSS", href = "https://data.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-P/dttw-5yxu/about_data", target = "_blank")),
             p("BRFSS (Behavioral Risk Factor Surveillance System) collects information about risk factors for various causes of death, data is from 2011 to present. Go to ",
               tags$a("BRFSS website", href = "https://www.cdc.gov/brfss/", target = "_blank"), " for additional information.")
    ),
    
    # Tab 1: General and Mental Health
    tabPanel("General and Mental Health",
             sidebarLayout(
               sidebarPanel(
                 h4("General and Mental Health"),
                 shared_sidebar("year_tab1", "demographics_tab1", "location_tab1", "us_map_tab1", "filter_ui_tab1")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Overall Health", plotlyOutput("plot_overallhealth")),
                   tabPanel("Bad Mental Health Days", plotlyOutput("plot_healthydays")),
                   tabPanel("Depression Diagnosis", plotlyOutput("plot_depression"))
                 )
               )
             )
    ),
    
    # Tab 2: Physical Activity
    tabPanel("Physical Activity",
             sidebarLayout(
               sidebarPanel(
                 h4("Physical Activity"),
                 shared_sidebar("year_tab2", "demographics_tab2", "location_tab2", "us_map_tab2", "filter_ui_tab2")
               ),
               mainPanel(
                 plotlyOutput("plot_physicalactivity")
               )
             )
    ),
    
    # Tab 3: Substance Use
    tabPanel("Substance Use",
             sidebarLayout(
               sidebarPanel(
                 h4("Substance Use"),
                 shared_sidebar("year_tab3", "demographics_tab3", "location_tab3", "us_map_tab3", "filter_ui_tab3")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Alcohol Consumption", 
                            tags$details(
                              open = TRUE,
                              tags$summary(class = "hover-summary", style = "margin-top: 15px; color: #337ab7; cursor: pointer;", "Click here to view or hide definitions of drinking patterns"),
                              div(
                                style = "background-color: #e6f2ff; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                                HTML("<strong>Alcohol Consumption:</strong> Adults who have had at least one drink of alcohol within the past 30 days<br>
                                      <strong>Binge Drinking:</strong> Adult males having 5 or more drinks on one occasion and adult females having 4 or more drinks on one occasion<br>
                                      <strong>Heavy Drinking:</strong> Adult males having more than 14 drinks per week and adult females having more than 7 drinks per week")
                              )
                            ),
                            plotlyOutput("plot_drinking")),
                   tabPanel("Smoking", 
                            tags$details(
                              open = TRUE,
                              tags$summary(class = "hover-summary", style = "margin-top: 15px; color: #337ab7; cursor: pointer;", "Click here to view or hide definitions of smoking categories"),
                              div(
                                style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                                HTML("<strong>Current Smoker:</strong> Adults who have smoked at least 100 cigarettes in their lifetime and currently smoke every day or some days<br>
                                     <strong>Smokeless Tobacco:</strong> Adults who currently use chewing tobacco, snuff, or snus every day or some days")
                              )
                            ),
                            plotlyOutput("plot_smoking"))
                 )
               )
             )
    ),
    
    # Tab 4: Health Access
    tabPanel("Health Access",
             sidebarLayout(
               sidebarPanel(
                 h4("Health Access"),
                 shared_sidebar("year_tab4", "demographics_tab4", "location_tab4", "us_map_tab4", "filter_ui_tab4")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Healthcare Coverage", plotlyOutput("plot_coverage")),
                   tabPanel("Healthcare Cost Barrier", 
                            div(
                              style = "background-color: #e6f2ff; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                              HTML("<strong>Note:</strong> Plot below shows the percentage of adults who reported that they needed to see a doctor but could not due to cost in the past 12 months")
                            ),
                            plotlyOutput("plot_cost")),
                   tabPanel("Healthcare Provider", 
                            div(
                              style = "background-color: #e6f2ff; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                              HTML("<strong>Note:</strong> Plot below shows the percentage of adults who reported having a personal doctor or healthcare provider")
                            ),
                            plotlyOutput("plot_provider"))
                 )
               )
             )
    ),
    
    # Tab 5: Ordinal Logistic Regression
    tabPanel("Ordinal Logistic Regression Model",
             
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput(
                   inputId = "state_olr",
                   label = "Select a state:",
                   choices = c("California (West Coast)" = "California", "Texas (Southwest)" = "Texas", "New York (Northeast)" = "New York", "Alabama (Southeast)" = "Alabama"),
                   selected = "California"),
                 
                 checkboxGroupInput(
                   inputId = "predictors_olr",
                   label = 'Choose predictors to include in the model and click "Run Model" below:',
                   choices = c("Age Group" = "age_group",
                               "Sex" = "sex",
                               "Education" = "education_attained",
                               "Income" = "household_income",
                               "Race/Ethnicity" = "race.ethnicity",
                               "Depression Diagnosis" = "Depression_Yes",
                               "Overall Health - Very Good" = "Overall.Health_Very.good",
                               "Overall Health - Good" = "Overall.Health_Good",
                               "Overall Health - Fair" = "Overall.Health_Fair",
                               "Overall Health - Poor" = "Overall.Health_Poor",
                               "Physical Health: 1-13 Bad Days" = "Healthy.Days_phys_1_13",
                               "Physical Health: 14+ Bad Days" = "Healthy.Days_phys_14",
                               "Aerobic Activity" = "Aerobic.Activity_Yes",
                               "Strength Activity" = "Strength.Activity_Yes",
                               "Alcohol Consumption" = "Alcohol.Consumption_Yes",
                               "Binge Drinking" = "Binge.Drinking_Yes",
                               "Heavy Drinking" = "Heavy.Drinking_Yes",
                               "Current Smoker" = "Current.Smoker.Status_Yes",
                               "Smokeless Tobacco" = "Smokeless.Tobacco_Yes",
                               "Health Care Coverage" = "Health.Care.Coverage_Yes",
                               "Health Care Cost Barrier" = "Health.Care.Cost_Yes"),
                   selected = c("Depression_Yes", "Alcohol.Consumption_Yes")
                ),
                
                actionButton("run_model", "Run Model", icon = icon("play")),
                
                br(), br(),
                
                radioButtons(
                  inputId = "p_cutoff_olr",
                  label = "Significance Threshold (p-value)",
                  choices = c("0.05 (standard)" = 0.05,
                              "0.01 (strict)" = 0.01,
                              "0.001 (very strict)" = 0.001),
                  selected = 0.05)
                ),
      
                mainPanel(
                      h4("Model Summary"),
                      div(
                          style = "background-color: #e6f2ff; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                          HTML("Tables below are created using a <strong>random ~10% sample of the available data</strong> to speed up performance. For full analysis, please download the dataset and run the model locally on your desktop.")
                        ),
                      HTML('<details>
                              <summary style="margin-top: 15px; color: #337ab7; cursor: pointer;">
                              Click here to view or hide how to interpret model results
                              </summary>
                              
                              <div style="background-color: #e6f2ff; padding: 15px; border-radius: 5px; margin-bottom: 10px;">
                              <strong>Estimate:</strong> The change in mental health outcome associated with the selected predictor. Positive values suggest a positive relationship with mental health, while negative values suggest a negative relationship. Larger values indicate stronger effects.<br><br>
                              <strong>Confidence Interval:</strong> This range represents where the true effect is likely to fall. A narrower CI suggests a higher certainty about the effect.<br><br>
                              <strong>p value:</strong> This value indicates the statistical significance of the predictor. A value less than the threshold you selected is highlighted in red, and suggests that the predictor has a significant impact on mental health.<br><br>
                              <strong>AIC (Akaike Information Criterion):</strong> A measure that balances model fit and simplicity. Lower AIC values indicate a better model.<br><br>
                              <strong>BIC (Bayesian Information Criterion):</strong> Similar to AIC, but is more conservative than AIC. Lower BIC values indicate a better model.<br><br>
                              <strong>Log-Likelihood:</strong> Represents how well the model fits the data. Higher values are better, but it does not account for how complex the model is (e.g., high number of variables).
                              </div>
                            </details>'),
                      DT::dataTableOutput("olrmodel_table"),
                      
                      hr(),
                      h4("Model Comparison Table (max. 5 models)"),
                      DT::dataTableOutput("model_comparison_table"),
                      actionButton("reset_history", "Reset Model History", icon = icon("undo"))
                )
           
         )
    )
  )  
)  