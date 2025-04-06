# code to select which plots to show (before switching to sub-tabs)


##### User Interface Object

ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Factors that may affect mental health"), 
  
  tabsetPanel(
    
    # Introduction Tab
    tabPanel("Introduction",
             p("This Shiny app explores various factors that may influence mental health. Use the tabs above to navigate through different data visualizations and analyses."),
             tags$ul(
               tags$li(strong("General and Mental Health:"), " Evaluate general health patterns perceived by individuals"),
               tags$li(strong("General Health:"), " Evaluate general health patterns."),
               tags$li(strong("Mental Health Metrics:"), " Dive into mental health indicators."),
               tags$li(strong("Demographic Breakdown:"), " Explore data by age, gender, etc.")
             ),
             tags$p(tags$a("Data from BRFSS", href="https://data.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-P/dttw-5yxu/about_data", target = "_blank")),
             p("BRFSS (Behavioral Risk Factor Surveillance System) collects information about risk factors for various causes of death, data is from 2011 to present. Go to ",
               tags$a("BRFSS website", href = "https://www.cdc.gov/brfss/", target = "_blank"), " for additional information.")
    ),
    
    # Tab 1: General and Mental Health Evaluations
    tabPanel("General and Mental Health",
             sidebarLayout(
               
               sidebarPanel(
                 h4("General and Mental Health"),
                 
                 checkboxGroupInput(
                    inputId = "selected_plots_panel1", label = "Select categories to display:",
                    choices = c(
                      "Overall Health" = "Overall Health",
                      "Mentally Healthy Days" = "Healthy Days",
                      "Depression Diagnosis" = "Depression"),
                    selected = "Overall Health"),
                 
                 sliderInput(
                   inputId = "year", label = "Select Year Range:", 
                   min = min(df$Year, na.rm = TRUE), max = max(df$Year, na.rm = TRUE), value = c(min(df$Year), max(df$Year)),
                   step = 1, sep = ""),
                 
                 radioButtons(
                   inputId = "demographics", label = "Demographics:",
                   choices = list(
                     "Sex" = "Sex",
                     "Age" = "Age Group",
                     "Race" = "Race/Ethnicity",
                     "Education Level" = "Education Attained",
                     "Income" = "Household Income"), 
                   inline = TRUE),
                 
                 selectizeInput(
                   inputId = "location", label = "Select State:",
                   choices = c("", state.name), selected = "Alabama", options = list(placeholder = 'Type a state')),
                 
                 uiOutput("demographics_filter_ui")
               ),
               
               mainPanel(
                  div(id = "plot_overallhealth_container", style = "margin-bottom: 50px;", plotlyOutput("plot_overallhealth")),
                  div(id = "plot_healthydays_container", style = "margin-bottom: 50px;", plotlyOutput("plot_healthydays")),
                  div(id = "plot_depression_container", style = "margin-bottom: 50px;", plotlyOutput("plot_depression"))
                )
               
               
             )
    )
    
    
    
  )
)



##### Server Function

server <- function(input, output, session){
  
  # filter data for plotting
  df_filtered <- reactive({
    
    req(input$demographics)
    req(input$location != "")
    
    df_subset <- df %>%
      filter(Break_Out_Category == input$demographics) %>%
      filter(Year >= input$year[1] & Year <= input$year[2]) %>% 
      filter(Locationdesc == input$location)
    
    return(df_subset)
  })
  
  # checklist for selecting demographics
  
  output$demographics_filter_ui <- renderUI({
    req(input$demographics)
    
    df_sub <- df %>%
      filter(Break_Out_Category == input$demographics) %>%
      set_breakout_levels(input$demographics)
    
    choices <- levels(df_sub$Break_Out)
    
    checkboxGroupInput(
      inputId = "breakout_values", label = paste("Select", input$demographics, "to display:"),
      choices = choices, selected = choices, inline = FALSE)
    
  })
  
  # select which plots to show
  observe({
    if ("Overall Health" %in% input$selected_plots_panel1) {show("plot_overallhealth_container")} else {hide("plot_overallhealth_container")}
    if ("Healthy Days" %in% input$selected_plots_panel1) {show("plot_healthydays_container")} else {hide("plot_healthydays_container")}
    if ("Depression" %in% input$selected_plots_panel1) {show("plot_depression_container")} else {hide("plot_depression_container")}
  })
  
  # rendering plots in panel 1:
  
  output$plot_overallhealth <- renderPlotly({
    
    if (!("Overall Health" %in% input$selected_plots_panel1)) {return(NULL)}
    req("Overall Health" %in% input$selected_plots_panel1)
    
    df_plot <- df_filtered()  %>%
      filter(Topic == "Overall Health") %>% 
      filter(Break_Out %in% input$breakout_values)
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    df_plot <- set_breakout_levels(df_plot, input$demographics)
    layout_info <- checklist_plot_layout(df_plot)
    
    p1 <- ggplot(df_plot, aes(x = Year, y = Data_value, color = Response,
                              group = interaction(Response, Break_Out),
                              text = paste0(Response, ": ", Data_value, " %", "<br>"))) +
      geom_line(linewidth = 1) + 
      geom_point(size = 2) +  
      facet_wrap(~Break_Out, ncol = layout_info$num_cols, scales = "free_y") +
      theme_bw(base_size = 15, base_family = "Arial") +
      theme(plot.margin = margin(t = 30, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9)) +
      labs(color = "Response", y = "% Response", x = "Year") +
      ggtitle(paste0("Overall Health by ", input$demographics, " from ", 
                     input$year[1], " to ", input$year[2], " in ", input$location))
    
    ggplotly(p1, tooltip = "text", height = layout_info$plot_height) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$plot_healthydays <- renderPlotly({
    
    if (!("Healthy Days" %in% input$selected_plots_panel1)) {return(NULL)}
    req("Healthy Days" %in% input$selected_plots_panel1)
    
    df_plot <- df_filtered()  %>%
      filter(Topic == "Healthy Days") %>% 
      filter(Response %in% c("Zero days when mental health not good", "1-13 days when mental health not good","14+ days when mental health not good")) %>% 
      mutate(Response = recode(Response,
                               "Zero days when mental health not good" = "0 days",
                               "1-13 days when mental health not good" = "1–13 days",
                               "14+ days when mental health not good" = "14+ days"
      )) %>% 
      filter(Break_Out %in% input$breakout_values) 
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    df_plot <- set_breakout_levels(df_plot, input$demographics)
    layout_info <- checklist_plot_layout(df_plot)
    
    p2 <- ggplot(df_plot, aes(x = Year, y = Data_value, color = Response,
                              group = interaction(Response, Break_Out),
                              text = paste0(Response, ": ", Data_value, " %", "<br>"))) +
      geom_line(linewidth = 1) + 
      geom_point(size = 2) +  
      facet_wrap(~Break_Out, ncol = layout_info$num_cols, scales = "free_y") +
      theme_bw(base_size = 15, base_family = "Arial") +
      theme(plot.margin = margin(t = 30, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9)) +
      labs(color = "Response", y = "% Response", x = "Year") +
      ggtitle(paste0("Number of Bad Mental Health Days by ", input$demographics, " from ", 
                     input$year[1], " to ", input$year[2], " in ", input$location))
    
    ggplotly(p2, tooltip = "text", height = layout_info$plot_height) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$plot_depression <- renderPlotly({
    
    if (!("Depression" %in% input$selected_plots_panel1)) {return(NULL)}
    req("Depression" %in% input$selected_plots_panel1)
    
    df_plot <- df_filtered()  %>%
      filter(Topic == "Depression") %>% 
      filter(Response == "Yes") %>%
      filter(Break_Out %in% input$breakout_values)
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    df_plot <- set_breakout_levels(df_plot, input$demographics)
    layout_info <- checklist_plot_layout(df_plot)
    
    p3 <- ggplot(df_plot, aes(x = Year, y = Data_value, fill = Break_Out,
                              group = interaction(Response, Break_Out),
                              text = paste0(Response, ": ", Data_value, " %", "<br>"))) +
      geom_col(width = 0.6) +
      facet_wrap(~Break_Out, ncol = layout_info$num_cols, scales = "free_y") +
      theme_bw(base_size = 15, base_family = "Arial") +
      theme(plot.margin = margin(t = 30, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9)) +
      labs(color = "Response", y = "% Response", x = "Year") +
      ggtitle(paste0("Depression Diagnosed by ", input$demographics, " from ", 
                     input$year[1], " to ", input$year[2], " in ", input$location))
    
    ggplotly(p3, tooltip = "text", height = layout_info$plot_height) %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$plot1 <- renderUI({
    plot_list <- list()
    
    if ("Overall Health" %in% input$selected_plots_panel1) {plot_list <- c(plot_list, plotlyOutput("plot_overallhealth"))}
    if ("Healthy Days" %in% input$selected_plots_panel1) {plot_list <- c(plot_list, plotlyOutput("plot_healthydays"))}
    if ("Depression" %in% input$selected_plots_panel1) {plot_list <- c(plot_list, plotlyOutput("plot_depression"))}
    
    if (length(plot_list) == 0) {
      plot_list <- list(tags$h4("No plots selected. Please choose one or more categories from the checkboxes."))}
    
    do.call(tagList, plot_list)
  })
  
  
}

