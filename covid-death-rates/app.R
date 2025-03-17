library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)


# import data
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


# 1. User Interface Object
ui <- fluidPage(
  
  titlePanel("COVID-19 Number of Deaths"), 
  
  sidebarLayout(
    
    # Control Widgets
    sidebarPanel(
      
      # Slider (Date)
      sliderInput(inputId = "date_range",
                  label = "Select Date Range:", 
                  min = min(df$data_period_start, na.rm = TRUE),
                  max = max(df$data_period_start, na.rm = TRUE),
                  value = c(min(df$data_period_start), max(df$data_period_start)),  # Default full range
                  timeFormat = "%Y-%m",  # Display as "YYYY-MM"
                  step = 30),
      
      # Radio Button (Demographics)
      radioButtons(inputId = "demographics",
                   label = "Demographics:",
                   choices = list(
                     "Sex" = "Sex",
                     "Age" = "Age",
                     "Race" = "Race")),

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
      "Data includes COVID-19 Monthly Death Rates per 100,000 Population from January 2020 to December 2024."
    ),
    
    # Main panel output
    mainPanel(
      plotlyOutput("plot1", height = "500px")
    )
  )
)

# 2. Server Function
server <- function(input, output, session){

  # Filter data for plotting
  df_filtered <- reactive({
    req(input$demographics)
    
    df_filtered <- df %>%
      filter(group == input$demographics) %>%
      filter(data_period_start >= input$date_range[1] & data_period_start <= input$date_range[2])  # Date range filter
    
    if (input$jurisdiction == "United States") {
      df_filtered <- df_filtered %>% filter(jurisdiction_residence == "United States")
    } else {
      df_filtered <- df_filtered %>% filter(jurisdiction_residence == input$jurisdiction)
    }
    
    return(df_filtered)
  })

  # PLOT
  output$plot1<-renderPlotly({
    
    df_plot <- df_filtered()
    demographic_column <- ifelse(input$demographics %in% unique(df_plot$group), "subgroup1", input$demographics)
    
    date_range_days <- as.numeric(difftime(input$date_range[2], input$date_range[1], units = "days"))
    
    if (date_range_days <= 365) {
      tick_interval <- "1 month"   
    } else if (date_range_days <= 3 * 365) {
      tick_interval <- "3 months" 
    } else {
      tick_interval <- "6 months"    
    }

    p <- ggplot(df_plot, aes(x = data_period_start, y = covid_deaths, 
                             group = .data[[demographic_column]],
                             color = .data[[demographic_column]], 
                             text = paste0(
                               "Death Rate: ", round(crude_rate, 2), "<br>",
                               "95% CI: [", round(conf_int_95pct_lower_crude, 2), ", ", round(conf_int_95pct_upper_crude, 2), "]"
                             ))) +
      geom_line(linewidth = 1) +  # Light connecting lines
      geom_point(size = 2) +  # Points for scatter plot
      theme_bw(base_size = 15, base_family = "Arial") +
      labs(color = input$demographics, y = "# of Deaths", x = "Date") +
      ggtitle(paste0("# of deaths by ", input$demographics, " from ", 
                     format(input$date_range[1], "%b %Y"), " to ", 
                     format(input$date_range[2], "%b %Y"), 
                     " in ", input$jurisdiction)) +
      scale_x_date(date_labels = "%b %Y", date_breaks = tick_interval)
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })

  
}

# 3. Create the Shiny Application
shinyApp(ui, server)
