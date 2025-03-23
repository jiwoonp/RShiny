##### Server Function

server <- function(input, output, session){
  
  # Filter covid deaths data for plotting
  df_filtered <- reactive({
    req(input$demographics)
    
    df_filtered <- df %>%
      filter(group == input$demographics) %>%
      filter(data_period_start >= input$date_range[1] & data_period_start <= input$date_range[2]) 
    
    if (input$jurisdiction == "United States") {
      df_filtered <- df_filtered %>% filter(jurisdiction_residence == "United States")
    } else {
      df_filtered <- df_filtered %>% filter(jurisdiction_residence == input$jurisdiction)
    }
    
  })
  
  # PLOT for covid deaths data
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
    
    p1 <- ggplot(df_plot, aes(x = data_period_start, y = covid_deaths, 
                              group = .data[[demographic_column]],
                              color = .data[[demographic_column]], 
                              text = paste0(
                                "# of Deaths: ", covid_deaths, "<br>"))) +
      geom_line(linewidth = 1) + 
      geom_point(size = 2) +  
      theme_bw(base_size = 15, base_family = "Arial") +
      labs(color = input$demographics, y = "# of Deaths", x = "Date") +
      ggtitle(paste0("# of deaths by ", input$demographics, " from ", 
                     format(input$date_range[1], "%b %Y"), " to ", 
                     format(input$date_range[2], "%b %Y"), " in ", input$jurisdiction)) +
      scale_x_date(date_labels = "%b %Y", date_breaks = tick_interval)
    
    ggplotly(p1, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  
  # Filter covid vaccine coverage data for plotting
  
  df2_filtered <- reactive({
    req(input$demographics_df2)
    
    df2_filtered <- df2 %>%
      filter(Demographic.Level == input$demographics_df2) %>%
      filter(Week_ending >= input$date_range_df2[1] & Week_ending <= input$date_range_df2[2])  
  })
  
  # PLOT for covid vaccine coverage data
  
  output$plot2<-renderPlotly({
    
    df2_plot <- df2_filtered()
    demographic_column_df2 <- ifelse(input$demographics_df2 %in% unique(df2_plot$Demographic.Level), "Demographic.Name", input$demographics_df2)
    
    date_range_days_df2 <- as.numeric(difftime(input$date_range_df2[2], input$date_range_df2[1], units = "days"))
    
    if (date_range_days_df2 <= 365) {
      tick_interval_df2 <- "1 month"   
    } else if (date_range_days_df2 <= 3 * 365) {
      tick_interval_df2 <- "3 months" 
    } else {
      tick_interval_df2 <- "6 months"    
    }
    
    
    p2 <- ggplot(df2_plot, aes(x = Week_ending, y = Estimate, 
                               group = .data[[demographic_column_df2]],
                               color = .data[[demographic_column_df2]], 
                               text = paste0(
                                 "Vaccination Coverage Estimate: ", round(Estimate, 1), "%", "<br>",
                                 "95% CI: [", round((Estimate - CI_Half_width_95pct), 1), "%, ", round((Estimate + CI_Half_width_95pct), 1), "%]"))) +
      geom_line(linewidth = 1) +  
      geom_point(size = 2) +  
      geom_vline(xintercept = as.numeric(as.Date("2024-09-07")), linetype = "dashed", color = "grey", size = 1) +
      theme_bw(base_size = 15, base_family = "Arial") +
      labs(color = input$demographics_df2, y = "Vaccination Coverage Estimate (%)", x = "Date") +
      ggtitle(paste0("Vaccination Coverage Estimate by ", input$demographics_df2, " from ", 
                     format(input$date_range_df2[1], "%b %Y"), " to ", 
                     format(input$date_range_df2[2], "%b %Y"), " in United States")) +
      scale_x_date(date_labels = "%b %Y", date_breaks = tick_interval_df2)
    
    ggplotly(p2, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  
  # Checkboxes for demographic level
  
  output$demographic_choices_ui_df3 <- renderUI({
    req(input$demographics_df3)
    
    choices <- unique(df3$Demographic.Name[df3$Demographic.Level == input$demographics_df3])
    
    checkboxGroupInput(
      inputId = "demographic_values_df3",
      label = paste("Select", input$demographics_df3, "Group(s):"),
      choices = choices,
      selected = choices
    )
  })
  
  # Controlling facetted plot heights
  
  output$plot3_height <- renderUI({
    req(input$demographic_values_df3)
    
    n_panels <- length(input$demographic_values_df3)
    height_per_panel <- 300  
    total_height <- max(500, n_panels * height_per_panel)
    
    plotlyOutput("plot3", height = paste0(total_height, "px"))
  })
  
  # Filter covid vaccine intention data for plotting
  
  df3_filtered <- reactive({
    req(input$demographics_df3, input$demographic_values_df3)
    
    df3_filtered <- df3 %>%
      filter(Demographic.Level == input$demographics_df3, Demographic.Name %in% input$demographic_values_df3) %>%
      filter(Week_ending >= input$date_range_df3[1] & Week_ending <= input$date_range_df3[2])  
  })
  
  # PLOT for covid vaccine coverage data
  
  output$plot3<-renderPlotly({
    
    req(input$demographics_df3, input$demographic_values_df3)
    
    df3_plot <- df3_filtered()
    demographic_column_df3 <- ifelse(input$demographics_df3 %in% unique(df3_plot$Demographic.Level), "Demographic.Name", input$demographics_df3)
    
    date_range_days_df3 <- as.numeric(difftime(input$date_range_df3[2], input$date_range_df3[1], units = "days"))
    
    if (date_range_days_df3 <= 365) {
      tick_interval_df3 <- "1 month"   
    } else if (date_range_days_df3 <= 3 * 365) {
      tick_interval_df3 <- "3 months" 
    } else {
      tick_interval_df3 <- "6 months"    
    }
    
    p3 <- ggplot(df3_plot, aes(x = Week_ending, y = Estimate, 
                               fill = indicator_category_label,
                               text = paste0(
                                 "Category: ", indicator_category_label, "<br>",
                                 "Intent Estimate: ", round(Estimate, 1), "%", "<br>",
                                 "95% CI: [", round((Estimate - CI_Half_width_95pct), 1), "%, ", round((Estimate + CI_Half_width_95pct), 1), "%]"
                               ))) +
      geom_bar(stat = "identity", position = "stack") + 
      facet_wrap(~Demographic.Name, ncol = 2) +
      geom_vline(xintercept = as.numeric(as.Date("2024-09-07")), linetype = "dashed", color = "grey", size = 1) +
      theme_bw(base_size = 15, base_family = "Arial") +
      labs(fill = "Intent Category", y = "Vaccination Coverage Estimate (%)", x = "Date") +
      ggtitle(paste0("Vaccination Intent Estimate by ", input$demographics_df3, " from ", 
                     format(input$date_range_df3[1], "%b %Y"), " to ", 
                     format(input$date_range_df3[2], "%b %Y"), " in United States")) +
      scale_x_date(date_labels = "%b %Y", date_breaks = tick_interval_df3) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "none")
    
    ggplotly(p3, tooltip = "text") %>%
      layout(margin = list(t=80)) %>% 
      config(displayModeBar = FALSE)
  })
  
}