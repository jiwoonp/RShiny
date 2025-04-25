ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "cerulean", font_scale = 0.9),
  tags$style(HTML("summary.hover-summary {color: #337ab7;cursor: pointer;} summary.hover-summary:hover {text-decoration: underline;}")),
  tags$style(HTML(".forecast-box p {margin-top: 2px;margin-bottom: 2px;line-height: 1.3;}")),
  tags$head(tags$script(HTML("$(document).on('click', 'a', function(event){
                                                              event.preventDefault();
                                                              window.open(this.href, '_blank');});"))),
  
  br(),
  
  titlePanel("Anxiety Trends & Forecasting"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Select Question (anxiety vs worry)
      radioButtons("question", "Select Question:",
                   choices = c("Feeling nervous/anxious/on edge" = "anxiety",
                               "Not able to stop/control worrying" = "worry")),
      
      # Select Date Range
      sliderInput("date_range", "Select Date Range:",
                  min = min(df$mid_date, na.rm = TRUE),
                  max = max(df$mid_date, na.rm = TRUE),
                  value = c(min(df$mid_date, na.rm = TRUE), max(df$mid_date, na.rm = TRUE)),
                  timeFormat = "%Y-%m-%d"),
      
      # Select Demographics
      selectInput("group_category", "Select Demographic Category:",
                  choices = setNames(names(group_category_ui_names), group_category_ui_names),
                  selected = "sex_at_birth"),
      
      uiOutput("group_name_ui"), 
      
      # Select plot y axis
      radioButtons("value_type", "Display Values As:",
                   choices = c("Counts" = "count", "% Respondents" = "percent"),
                   selected = "count"),
      
      # Download table and plot
      div(style = "display: flex; gap: 8px;",
          downloadButton("download_plot", "Download Plot as PNG")),
          downloadButton("download_forecast", "Download Forecast as CSV"),
      
      # Data source
      br(),
      tags$hr(),
      tags$p("Data collected from the U.S. Census Bureau:",
             htmltools::a("Household Pulse Survey", 
                          href = "https://www.census.gov/programs-surveys/household-pulse-survey.html", 
                          target = "_blank"))
          
    ),
    
    mainPanel(
      tabsetPanel(
        id = "active_tab",
        
        # Trends Tab
        tabPanel("Trends",
                 br(),
                 plotlyOutput("total_trend_plot", height = "300px"),
                 hr(),
                 plotlyOutput("group_trend_plot", height = "300px"),
                 hr(),
                 fluidRow(h5("Trend Summary Table"),
                          textOutput("trend_summary_caption"),
                          tableOutput("trend_summary_table"))
                 ),
        
        # Forecasting Tab
        tabPanel("Forecasting",
                 br(),
                 fluidRow(column(4,
                                 tags$label("Forecast Horizon (months) ", style = "font-weight: normal;"),
                                 div(style = "margin-top: -10px; margin-bottom: -10px;",
                                     sliderInput("forecast_months", NULL, min = 1, max = 6, value = 3, step = 1))),
                          column(8,
                                 checkboxInput("show_accuracy", "  Show model accuracy (MAE / MAPE) in the summary table", value = FALSE, width = "100%"))),
                 
                 hr(),
                 
                 fluidRow(column(12,
                                 tags$details(
                                   tags$summary(class = "hover-summary",
                                                "Click here to view tips for understanding the forecast plot and summary table"),
                                 div(class = "forecast-box",
                                    style = "background-color: #e6f2ff; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                                    
                                    tags$b("Forecast Plot:"),
                                    p("• Points show training data used to fit the model."),
                                    p("• The dashed line shows the model’s best fit trend with the training data, and the solid line shows the forecasted trend."),
                                    p("• Shaded area shows the forecast uncertainty (wider = less certain)."),
                                    p("• Forecasting more weeks ahead increases uncertainty. If your selected date range is less than 1 year, limit forecasts to 1–3 months.
                                      For longer date ranges, forecasts up to 6 months are more reliable."),
                                    br(),
                                    tags$b("Summary Table:"),
                                    p("• MAE (Mean Absolute Error) shows the average difference between the model’s predictions and the actual data."),
                                    p("• MAPE (Mean Absolute Percentage Error) shows how far off the model’s predictions are, as a percentage of the actual data."))
                                )
                            )
                          ),
                 
                 hr(),
                 
                 # Forecast plot
                 fluidRow(h5("Forecast Plot"),
                          plotOutput("forecast_plot", height = "350px")),
                 
                 hr(),
                 
                 # Forecast summary table
                 fluidRow(h5("Forecast Summary Table"),
                          tableOutput("summary_stats"))
                 
        )
      )
    )
  )
)