##### User Interface Object

ui <- fluidPage(
  
  titlePanel("COVID-19 Number of Deaths and Vaccination Coverage"), 
  
  tabsetPanel(
    
    # Tab 1: number of deaths
    tabPanel("COVID Deaths",
             fluidRow(
               # Control Widgets for Plot 1
               sidebarPanel(
                 h4("Controls for Number of Deaths Plot"),
                 
                 # Slider (Date)
                 sliderInput(inputId = "date_range",
                             label = "Select Date Range:", 
                             min = min(df$data_period_start, na.rm = TRUE),
                             max = max(df$data_period_start, na.rm = TRUE),
                             value = c(min(df$data_period_start), max(df$data_period_start)),  
                             timeFormat = "%Y-%m",  
                             step = 30),
                 
                 # Radio Button (Demographics)
                 radioButtons(inputId = "demographics",
                              label = "Demographics:",
                              choices = list(
                                "Sex" = "Sex",
                                "Age" = "Age",
                                "Race" = "Race"),
                              inline = TRUE),
                 
                 # Dropdown (Jurisdiction)
                 selectInput(inputId="jurisdiction",
                             label="Select HHS Region:",
                             choices = list(
                               "United States"="United States",
                               "Region 1 (CT, ME, MA, NH, RI, VT)" = "Region 1",
                               "Region 2 (NJ, NY, PR, VI)" = "Region 2",
                               "Region 3 (DE, DC, MD, PA, VA, WV)" = "Region 3",
                               "Region 4 (AL, FL, GA, KY, MS, NC, SC, TN)" = "Region 4",
                               "Region 5 (IL, IN, MI, MN, OH, WI)" = "Region 5",
                               "Region 6 (AR, LA, NM, OK, TX)" = "Region 6",
                               "Region 7 (IA, KS, MO, NE)" = "Region 7",
                               "Region 8 (CO, MT, ND, SD, UT, WY)" = "Region 8",
                               "Region 9 (AZ, CA, HI, NV, AS, GU, MP)" = "Region 9",
                               "Region 10 (AK, ID, OR, WA)" = "Region 10")),
                 
                 # Text
                 "Data collected from CDC:",
                 htmltools::a("COVID deaths", href="https://data.cdc.gov/Public-Health-Surveillance/Monthly-COVID-19-Death-Rates-per-100-000-Populatio/exs3-hbne/about_data"),
                 br(),
                 "Data includes COVID-19 Monthly Death Rates per 100,000 Population from January 2020 to December 2024."),
               
               # Main panel output
               mainPanel(plotlyOutput("plot1", height = "500px"))
             )
    ),
    
    # Tab 2: Vaccination Coverage
    tabPanel("Vaccination Coverage",
             fluidRow(
               # Control Widgets for Plot 2
               sidebarPanel(
                 h4("Controls for Vaccination Coverage Plot"),
                 
                 # Slider (Date)
                 sliderInput(inputId = "date_range_df2",
                             label = "Select Date Range:", 
                             min = min(df2$Week_ending, na.rm = TRUE),
                             max = max(df2$Week_ending, na.rm = TRUE),
                             value = c(min(df2$Week_ending), max(df2$Week_ending)),  
                             timeFormat = "%Y-%m-%d",  
                             step = 30),
                 
                 # Radio Button (Demographics)
                 radioButtons(inputId = "demographics_df2",
                              label = "Demographics:",
                              choices = list(
                                "Sex" = "Sex",
                                "Age" = "Age",
                                "Race" = "Race and Ethnicity",
                                "Urbanicity" = "Urbanicity",
                                "Disability Status" = "Disability Status",
                                "Poverty Status" = "Poverty Status",
                                "Health Insurance" = "Health Insurance"),
                              inline = TRUE),
                 
                 # Text
                 "Data collected from CDC:",
                 htmltools::a("COVID Vaccination Coverage", href="https://data.cdc.gov/Vaccinations/Weekly-Cumulative-COVID-19-Vaccination-Coverage-an/ksfb-ug5d/about_data"),
                 br(),
                 "Data includes COVID-19 Vaccination Coverage from September 2023 to March 2025.",
                 br(),
                 "Gray dashed line represents the boundary between 2023-2024 and 2024-2025 vaccine seasons."),
               
               # Main panel output
               mainPanel(plotlyOutput("plot2", height = "500px"))
             )
    ),
    
    # Tab 3: Vaccination Intent
    tabPanel("Vaccination Intent", 
             fluidRow(
               # Control Widgets for Plot 3
               sidebarPanel(
                 h4("Controls for Vaccination Intent Plot"),
                 
                 # Slider (Date)
                 sliderInput(inputId = "date_range_df3",
                             label = "Select Date Range:", 
                             min = min(df3$Week_ending, na.rm = TRUE),
                             max = max(df3$Week_ending, na.rm = TRUE),
                             value = c(min(df3$Week_ending), max(df3$Week_ending)),  
                             timeFormat = "%Y-%m-%d", 
                             step = 30),
                 
                 # List (Demographics)
                 selectInput(
                   inputId = "demographics_df3",
                   label = "Select Demographic Level:",
                   choices = list(
                     "Sex" = "Sex",
                     "Age" = "Age",
                     "Race" = "Race and Ethnicity",
                     "Urbanicity" = "Urbanicity",
                     "Disability Status" = "Disability Status",
                     "Poverty Status" = "Poverty Status",
                     "Health Insurance" = "Health Insurance")),
                 
                 uiOutput("demographic_choices_ui_df3"),
                 uiOutput("plot3_height"),
                 
                 # Text
                 "Data collected from CDC:",
                 htmltools::a("COVID Vaccination Coverage", href="https://data.cdc.gov/Vaccinations/Weekly-Cumulative-COVID-19-Vaccination-Coverage-an/ksfb-ug5d/about_data"),
                 br(),
                 "Data includes COVID-19 Vaccination Coverage from September 2023 to March 2025.",
                 br(),
                 "Gray dashed line represents the boundary between 2023-2024 and 2024-2025 vaccine seasons."),
               
               # Main panel output
               mainPanel(uiOutput("plot3_height"))
             )
    )
  )
)
