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
    )
    
  )  
)  