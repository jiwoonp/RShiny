
##### Server Function

server <- function(input, output, session){

  # reactives for parameters selected from each panel
  
  selected_year <- reactive({
    switch(input$tabs,
           "General and Mental Health" = input$year_tab1,
           "Physical Activity" = input$year_tab2,
           "Substance Use" = input$year_tab3,
           "Health Access" = input$year_tab4,
           NULL)
  })
  
  selected_demographics <- reactive({
    switch(input$tabs,
           "General and Mental Health" = input$demographics_tab1,
           "Physical Activity" = input$demographics_tab2,
           "Substance Use" = input$demographics_tab3,
           "Health Access" = input$demographics_tab4,
           NULL)
  })
  
  selected_location <- reactive({
    switch(input$tabs,
           "General and Mental Health" = input$location_tab1,
           "Physical Activity" = input$location_tab2,
           "Substance Use" = input$location_tab3,
           "Health Access" = input$location_tab4,
           NULL)
  })
  

  
  # filter data for plotting
  df_filtered <- reactive({
    req(selected_year() != "")
    req(selected_demographics() != "")
    req(selected_location() != "")
    
    df_subset <- df %>%
      filter(Break_Out_Category == selected_demographics()) %>%
      filter(Year >= selected_year()[1] & Year <= selected_year()[2]) %>% 
      filter(Locationdesc == selected_location())
    
    return(df_subset)
  })
  
  # checklist for selecting demographics
  
  render_demographics_filter_ui <- function(filter_ui_output_id) {
    output[[filter_ui_output_id]] <- renderUI({
      req(selected_demographics())
      
      df_sub <- df %>%
        filter(Break_Out_Category == selected_demographics()) %>%
        set_breakout_levels(selected_demographics())
      
      choices <- levels(df_sub$Break_Out)
      
      if (length(choices) == 0 || is.null(choices)) {
        return(tags$em("No demographic options available."))
      }
      
      checkboxGroupInput(
        inputId = "breakout_values", label = paste("Select", selected_demographics(), "to display:"),
        choices = choices, selected = choices, inline = FALSE)
    })
  }

  # map in sidebar
  
  render_map <- function(map_output_id, location_id) {
    output[[map_output_id]] <- renderPlotly({
      
      color_area <- rep(0, nrow(us_states))
      
      map_location <- input[[location_id]] 
      if (!is.null(map_location) && map_location != "") {
        selected_abb <- us_states$state_abb[us_states$state_name == map_location]
        color_area[us_states$state_abb == selected_abb] <- 1
      }
      
      plot_ly(us_states, source = map_output_id) %>%
        add_trace(type = "choropleth", locationmode = "USA-states", locations = ~state_abb, z = color_area, text = ~state_name, hoverinfo = "text",
                  colorscale = list(c(0, "lightblue"), c(1, "blue")), showscale = FALSE, marker = list(line = list(width = 0.5, color = "gray"))) %>%
        layout(geo = list(scope = "usa", fixedrange = TRUE), margin = list(l = 0, r = 0, t = 0, b = 0)) %>% 
        config(displayModeBar = FALSE, scrollZoom = FALSE) %>% 
        event_register(., "plotly_click")
      })
  }
  
  click_map <- function(map_source_id, location_id) {
    observeEvent(event_data("plotly_click", source = map_source_id), {
      click_data <- event_data("plotly_click", source = map_source_id)

      if (!is.null(click_data)) {
        idx <- click_data$pointNumber + 1
        if (!is.null(idx) && idx >= 1 && idx <= nrow(us_states)) {
          clicked_state <- us_states$state_name[idx]
          updateSelectizeInput(session, location_id, selected = clicked_state)
        }
      }
    })
  }
  
  # produce checklist and map for all tabs
  
  tab_ids <- c("tab1", "tab2", "tab3", "tab4") 
  
  lapply(tab_ids, function(id) {
    render_map(paste0("us_map_", id), paste0("location_", id))
    click_map(paste0("us_map_", id), paste0("location_", id))
    render_demographics_filter_ui(paste0("filter_ui_", id))
  })
  
  
  # rendering plots in panel 1:

  output$plot_overallhealth <- renderPlotly({
    
    df_plot <- df_filtered()  %>%
      filter(Topic == "Overall Health") %>% 
      filter(Break_Out %in% input$breakout_values) %>% 
      filter(!is.na(Confidence_limit_Low) & !is.na(Confidence_limit_High)) %>%
      set_breakout_levels(selected_demographics())
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    layout_info <- checklist_plot_layout(df_plot)

    p1 <- ggplot(df_plot, aes(x = Year, y = Data_value, color = Response, fill = Response,
                              group = interaction(Response, Break_Out),
                              text = paste0(Response, ": ", Data_value, " %", "<br>", "CI: [", round(Confidence_limit_Low, 1), "%, ", round(Confidence_limit_High, 1), "%]<br>"))) +
      geom_ribbon(aes(ymin = Confidence_limit_Low, ymax = Confidence_limit_High, fill = Response),
                  alpha = 0.2, color = NA, show.legend = FALSE) +
      geom_line(linewidth = 1) + 
      geom_point(size = 2) +  
      facet_wrap(~Break_Out, ncol = layout_info$num_cols, scales = "free_y") +
      theme_bw(base_size = 12, base_family = "Arial") +
      theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9)) +
      labs(color = "Response", y = "% Response", x = "Year") +
      #scale_fill_discrete(guide = "none") +
      ggtitle(paste0("Overall Health by ", selected_demographics(), " from ", 
                     input$year_tab1[1], " to ", input$year_tab1[2], " in ", input$location_tab1))
    
    ggplotly(p1, tooltip = "text", height = layout_info$plot_height) %>%
      style(showlegend = FALSE,
        traces = which(sapply(.$x$data, function(tracestyle) isTRUE(tracestyle$fill == "toself")))) %>%
      config(displayModeBar = FALSE)
  })
  
  output$plot_healthydays <- renderPlotly({
 
    df_plot <- df_filtered()  %>%
      filter(Topic == "Healthy Days") %>% 
      filter(Response %in% c("Zero days when mental health not good", "1-13 days when mental health not good","14+ days when mental health not good")) %>% 
      mutate(Response = recode(Response,
                               "Zero days when mental health not good" = "0 days",
                               "1-13 days when mental health not good" = "1–13 days",
                               "14+ days when mental health not good" = "14+ days"
      )) %>% 
      filter(Break_Out %in% input$breakout_values) %>% 
      filter(!is.na(Confidence_limit_Low) & !is.na(Confidence_limit_High)) %>%
      set_breakout_levels(selected_demographics())
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters.")) 
    layout_info <- checklist_plot_layout(df_plot)
    
    p2 <- ggplot(df_plot, aes(x = Year, y = Data_value, color = Response, fill = Response,
                              group = interaction(Response, Break_Out),
                              text = paste0(Response, ": ", Data_value, " %", "<br>", "CI: [", round(Confidence_limit_Low, 1), "%, ", round(Confidence_limit_High, 1), "%]<br>"))) +
      geom_ribbon(aes(ymin = Confidence_limit_Low, ymax = Confidence_limit_High, fill = Response),
                  alpha = 0.2, color = NA, show.legend = FALSE) +
      geom_line(linewidth = 1) + 
      geom_point(size = 2) +  
      facet_wrap(~Break_Out, ncol = layout_info$num_cols, scales = "free_y") +
      theme_bw(base_size = 12, base_family = "Arial") +
      theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9)) +
      labs(color = "Response", y = "% Response", x = "Year") +
      ggtitle(paste0("Number of Bad Mental Health Days by ", selected_demographics(), " from ", 
                     input$year_tab1[1], " to ", input$year_tab1[2], " in ", input$location_tab1))
    
    ggplotly(p2, tooltip = "text", height = layout_info$plot_height) %>% 
      style(showlegend = FALSE,
            traces = which(sapply(.$x$data, function(tracestyle) isTRUE(tracestyle$fill == "toself")))) %>%
      config(displayModeBar = FALSE)
    
  })
  
  output$plot_depression <- renderPlotly({
 
    df_plot <- df_filtered()  %>%
      filter(Topic == "Depression") %>% 
      filter(Response == "Yes") %>%
      filter(Break_Out %in% input$breakout_values) %>% 
      filter(!is.na(Confidence_limit_Low) & !is.na(Confidence_limit_High)) %>%
      set_breakout_levels(selected_demographics())
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    layout_info <- checklist_plot_layout(df_plot)
    
    p3 <- ggplot(df_plot, aes(x = Year, y = Data_value, fill = Break_Out,
                              group = interaction(Response, Break_Out),
                              text = paste0(Response, ": ", Data_value, " %", "<br>", "CI: [", round(Confidence_limit_Low, 1), "%, ", round(Confidence_limit_High, 1), "%]<br>"))) +
      geom_col(width = 0.6) +
      facet_wrap(~Break_Out, ncol = layout_info$num_cols, scales = "free_y") +
      theme_bw(base_size = 12, base_family = "Arial") +
      theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9)) +
      labs(color = "Response", y = "% Response", x = "Year") +
      ggtitle(paste0("Depression Diagnosed by ", selected_demographics(), " from ", 
                     input$year_tab1[1], " to ", input$year_tab1[2], " in ", input$location_tab1))
    
    ggplotly(p3, tooltip = "text", height = layout_info$plot_height) %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  
  # rendering plots in panel 2:
  
  output$plot_physicalactivity  <- renderPlotly({
    
    df_plot <- df_filtered()  %>%
      filter(Topic %in% c("Aerobic Activity","Strength Activity")) %>% 
      filter(Response == "Yes") %>%
      filter(Break_Out %in% input$breakout_values) %>% 
      filter(!is.na(Confidence_limit_Low) & !is.na(Confidence_limit_High)) %>% 
      complete(Year, Break_Out, Topic = c("Aerobic Activity","Strength Activity")) %>%
      set_breakout_levels(selected_demographics())
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    layout_info <- checklist_plot_layout(df_plot)
    
    p4 <- ggplot(df_plot, aes(x = as.factor(Year), y = Data_value, fill = Topic,
                              text = paste0(Topic, "<br>", Response, ": ", Data_value, " %", "<br>", "CI: [", round(Confidence_limit_Low, 1), "%, ", round(Confidence_limit_High, 1), "%]<br>"))) +
      geom_col(position = position_dodge(width = 0.6), width = 0.5) +
      facet_wrap(~Break_Out, ncol = layout_info$num_cols) +
      theme_bw(base_size = 12, base_family = "Arial") +
      theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9)) +
      labs(fill = "Activity Type", y = "% Responded Yes", x = "Year") +
      ggtitle(paste0("Reported Physical Activity by ", selected_demographics(), " from ", 
                     input$year_tab2[1], " to ", input$year_tab2[2], " in ", input$location_tab2))
    
    ggplotly(p4, tooltip = "text", height = layout_info$plot_height) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  
  
  # rendering plots in panel 3:
  
  output$plot_drinking  <- renderPlotly({
    
    df_plot <- df_filtered()  %>%
      filter(Topic %in% c("Alcohol Consumption", "Binge Drinking", "Heavy Drinking")) %>% 
      filter(Response == "Yes") %>%
      filter(Break_Out %in% input$breakout_values) %>% 
      filter(!is.na(Confidence_limit_Low) & !is.na(Confidence_limit_High)) %>% 
      complete(Year, Break_Out, Topic = c("Alcohol Consumption", "Binge Drinking", "Heavy Drinking")) %>%
      set_breakout_levels(selected_demographics())
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    layout_info <- checklist_plot_layout(df_plot)
    
    p5 <- ggplot(df_plot, aes(x = as.factor(Year), y = Data_value, fill = Topic,
                              text = paste0(Topic, "<br>", Response, ": ", Data_value, " %", "<br>", "CI: [", round(Confidence_limit_Low, 1), "%, ", round(Confidence_limit_High, 1), "%]<br>"))) +
      geom_col(position = position_dodge(width = 0.6), width = 0.5) +
      facet_wrap(~Break_Out, ncol = layout_info$num_cols) +
      theme_bw(base_size = 12, base_family = "Arial") +
      theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9),
            axis.text.x = element_text(angle= 90, hjust = 0.5)) +
      labs(fill = "Drinking Patterns", y = "% Responded Yes", x = "Year") +
      ggtitle(paste0("Reported Drinking Patterns by ", selected_demographics(), " from ", 
                     input$year_tab2[1], " to ", input$year_tab2[2], " in ", input$location_tab2))
    
    ggplotly(p5, tooltip = "text", height = layout_info$plot_height) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$plot_smoking  <- renderPlotly({
    
    df_plot <- df_filtered()  %>%
      filter(Topic %in% c("Current Smoker Status", "Smokeless Tobacco")) %>% 
      mutate(Topic = ifelse(Topic == "Current Smoker Status", "Current Smoker", Topic)) %>% 
      mutate(Response = case_when(
        Topic == "Smokeless Tobacco" & Response %in% c("Every day", "Some days") ~ "Yes",
        Topic == "Smokeless Tobacco" & Response == "Not at all" ~ "No",
        TRUE ~ Response)) %>%
      group_by(Year, Break_Out, Break_Out_Category, Topic, Response) %>%
      summarize(Data_value = sum(Data_value, na.rm = TRUE),
                Confidence_limit_Low = sum(Confidence_limit_Low, na.rm = TRUE),
                Confidence_limit_High = sum(Confidence_limit_High, na.rm = TRUE),
                .groups = "drop") %>%
      filter(Response == "Yes") %>%
      filter(Break_Out %in% input$breakout_values) %>% 
      filter(!is.na(Confidence_limit_Low) & !is.na(Confidence_limit_High)) %>% 
      complete(Year, Break_Out, Topic = c("Current Smoker", "Smokeless Tobacco")) %>%
      set_breakout_levels(selected_demographics()) 
      
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    layout_info <- checklist_plot_layout(df_plot)
    
    p6 <- ggplot(df_plot, aes(x = as.factor(Year), y = Data_value, fill = Topic,
                              text = paste0(Topic, "<br>", Response, ": ", Data_value, " %", "<br>", "CI: [", round(Confidence_limit_Low, 1), "%, ", round(Confidence_limit_High, 1), "%]<br>"))) +
      geom_col(position = position_dodge(width = 0.6), width = 0.5) +
      facet_wrap(~Break_Out, ncol = layout_info$num_cols) +
      theme_bw(base_size = 12, base_family = "Arial") +
      theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9),
            axis.text.x = element_text(angle= 90, hjust = 0.5)) +
      labs(fill = "Smoking Patterns", y = "% Responded Yes", x = "Year") +
      ggtitle(paste0("Reported Smoking Patterns by ", selected_demographics(), " from ", 
                     input$year_tab2[1], " to ", input$year_tab2[2], " in ", input$location_tab2))
    
    ggplotly(p6, tooltip = "text", height = layout_info$plot_height) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  
  # rendering plots in panel 4:
  
  output$plot_coverage  <- renderPlotly({
    
    df_plot <- df_filtered()  %>%
      filter(Topic == "Health Care Coverage") %>% 
      filter(Response == "Yes") %>%
      filter(Break_Out %in% input$breakout_values) %>% 
      filter(!is.na(Confidence_limit_Low) & !is.na(Confidence_limit_High)) %>%
      set_breakout_levels(selected_demographics())
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    layout_info <- checklist_plot_layout(df_plot)
    
    p7 <- ggplot(df_plot, aes(x = as.factor(Year), y = Data_value, fill = Break_Out,
                              group = interaction(Response, Break_Out),
                              text = paste0(Response, ": ", Data_value, " %", "<br>", "CI: [", round(Confidence_limit_Low, 1), "%, ", round(Confidence_limit_High, 1), "%]<br>"))) +
      geom_col(width = 0.6) +
      facet_wrap(~Break_Out, ncol = layout_info$num_cols) +
      theme_bw(base_size = 12, base_family = "Arial") +
      theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9),
            axis.text.x = element_text(angle= 90, hjust = 0.5)) +
      labs(y = "% Responded Yes", x = "Year") +
      ggtitle(paste0("Healthcare Coverage by ", selected_demographics(), " from ", 
                     input$year_tab1[1], " to ", input$year_tab1[2], " in ", input$location_tab1))
    
    ggplotly(p7, tooltip = "text", height = layout_info$plot_height) %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$plot_cost  <- renderPlotly({
    
    df_plot <- df_filtered()  %>%
      filter(Topic == "Health Care Cost") %>% 
      filter(Response == "Yes") %>%
      filter(Break_Out %in% input$breakout_values) %>% 
      filter(!is.na(Confidence_limit_Low) & !is.na(Confidence_limit_High)) %>%
      set_breakout_levels(selected_demographics())
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    layout_info <- checklist_plot_layout(df_plot)
    
    p8 <- ggplot(df_plot, aes(x = as.factor(Year), y = Data_value, fill = Break_Out,
                              group = interaction(Response, Break_Out),
                              text = paste0(Response, ": ", Data_value, " %", "<br>", "CI: [", round(Confidence_limit_Low, 1), "%, ", round(Confidence_limit_High, 1), "%]<br>"))) +
      geom_col(width = 0.6) +
      facet_wrap(~Break_Out, ncol = layout_info$num_cols) +
      theme_bw(base_size = 12, base_family = "Arial") +
      theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9),
            axis.text.x = element_text(angle= 90, hjust = 0.5)) +
      labs(y = "% Responded Yes", x = "Year") +
      ggtitle(paste0("Cost Barriers to Healthcare by ", selected_demographics(), " from ", 
                     input$year_tab1[1], " to ", input$year_tab1[2], " in ", input$location_tab1))
    
    ggplotly(p8, tooltip = "text", height = layout_info$plot_height) %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$plot_provider  <- renderPlotly({
    
    df_plot <- df_filtered()  %>%
      filter(Topic == "Health Care Coverage") %>% 
      mutate(Response = case_when(
        Response %in% c("Yes, only one", "More than one") ~ "Yes",
        TRUE ~ Response)) %>%
      group_by(Year, Break_Out, Break_Out_Category, Topic, Response) %>%
      summarize(Data_value = sum(Data_value, na.rm = TRUE),
                Confidence_limit_Low = sum(Confidence_limit_Low, na.rm = TRUE),
                Confidence_limit_High = sum(Confidence_limit_High, na.rm = TRUE),
                .groups = "drop") %>%
      filter(Response == "Yes") %>%
      filter(Break_Out %in% input$breakout_values) %>% 
      filter(!is.na(Confidence_limit_Low) & !is.na(Confidence_limit_High)) %>%
      set_breakout_levels(selected_demographics())
    validate(need(nrow(df_plot) > 0, "No data available for the selected parameters."))
    layout_info <- checklist_plot_layout(df_plot)
    
    p9 <- ggplot(df_plot, aes(x = as.factor(Year), y = Data_value, fill = Break_Out,
                              group = interaction(Response, Break_Out),
                              text = paste0(Response, ": ", Data_value, " %", "<br>", "CI: [", round(Confidence_limit_Low, 1), "%, ", round(Confidence_limit_High, 1), "%]<br>"))) +
      geom_col(width = 0.6) +
      facet_wrap(~Break_Out, ncol = layout_info$num_cols) +
      theme_bw(base_size = 12, base_family = "Arial") +
      theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10),
            strip.text = element_text(size = 9),
            axis.text.x = element_text(angle= 90, hjust = 0.5)) +
      labs(y = "% Responded Yes", x = "Year") +
      ggtitle(paste0("Healthcare Provider Access by ", selected_demographics(), " from ", 
                     input$year_tab1[1], " to ", input$year_tab1[2], " in ", input$location_tab1))
    
    ggplotly(p9, tooltip = "text", height = layout_info$plot_height) %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
    
  })

  
}

