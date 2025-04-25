
##### Server Function

server <- function(input, output, session) {
  
  # Question Selection
  question_type <- c("anxiety" = "Frequency of feeling nervous, anxious, or on edge",
                     "worry" = "Frequency of not being able to stop or control worrying")
  
  # Demographic Category Selection
  output$group_name_ui <- renderUI({
    req(input$group_category)
    if (input$group_category == "total") return(NULL)
    
    groups <- unique(df$group_name[df$group_category == input$group_category])
    selectInput("group_name", "Select Group:", choices = groups, selected = groups[1], selectize = TRUE)
  })
  
  observeEvent(input$group_category, {
    if (input$group_category != "total") {
      valid_groups <- unique(df$group_name[df$group_category == input$group_category])
      updateSelectInput(session, "group_name", choices = valid_groups, selected = valid_groups[1])
    }
  })
  
  # Filter data for Trends Panel
  df_filtered <- reactive({
    req(input$question, input$date_range)
    
    df %>% filter(mid_date >= input$date_range[1],
                  mid_date <= input$date_range[2],
                  question == question_type[[input$question]])
  })

  # Filter data for Forecasting Panel
  df_filtered_prophet <- reactive({
    if (input$group_category == "total") {
      df %>% filter(group_category == "total", group_name == "Total",
                    mid_date >= input$date_range[1],
                    mid_date <= input$date_range[2],
                    question == question_type[[input$question]],
                    frequency != "Did not report")
    } else {
      req(input$group_name)
      df %>% filter(group_category == input$group_category,
                    group_name == input$group_name,
                    mid_date >= input$date_range[1],
                    mid_date <= input$date_range[2],
                    question == question_type[[input$question]],
                    frequency != "Did not report")
    }
  })
  
  # Plot Anxiety Trends for Total Group
  
  output$total_trend_plot <- renderPlotly({
 
    df_plot <- df_filtered() %>% 
                  filter(group_category == "total", group_name == "Total") %>%
                  group_by(mid_date) %>%
                  mutate(total = sum(estimate, na.rm = TRUE)) %>%
                  group_by(mid_date, frequency) %>%
                  summarise(estimate = if (input$value_type == "percent") {mean(estimate / total, na.rm = TRUE) * 100} 
                                                 else {mean(estimate, na.rm = TRUE)}, .groups = "drop") %>%
                  mutate(hover_text = paste0("Date: ", format(mid_date, "%Y-%m-%d"), "<br>",
                                             if (input$value_type == "percent") {paste0("Percent: ", round(estimate, 1), "%")} 
                                             else {paste0("Count: ", format(round(estimate, 0), big.mark = ","))}))
    
     p <- ggplot(df_plot, aes(x = mid_date, y = estimate, color = frequency, group = frequency, text = hover_text)) +
            geom_line() +
            geom_point(size = 1.5) +
            scale_color_manual(values = frequency_colors) +
            labs(title = "Frequency of Reported Anxiety Symptoms ( Total Group )",
                 x = "Date", y = ifelse(input$value_type == "percent", "Percent (%)", "Counts"), color = "Response Intensity") +
            theme_minimal(base_size = 12) +
            theme(plot.title = element_text(size = 14))
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(t = 30), legend = list(orientation = "h", y = -0.5)) %>% 
      config(displayModeBar = FALSE)
  })
  
  # Plot Anxiety Trends for for Selected Demographic Group

  selected_group <- reactive({
    req(input$group_category)
    if (input$group_category == "total") {return("Total")}
    
    valid_groups <- unique(df$group_name[df$group_category == input$group_category])
    
    if (!is.null(input$group_name) && input$group_name %in% valid_groups) {return(input$group_name)} else {return(valid_groups[1])}
  })
  
  output$group_trend_plot <- renderPlotly({
    req(selected_group())  
    
    df_plot <- df_filtered() %>%
      filter(group_category == input$group_category,
             group_name == selected_group(),
             !is.na(frequency))%>%
      group_by(mid_date) %>%
      mutate(total = sum(estimate, na.rm = TRUE)) %>%
      group_by(mid_date, frequency) %>%
      summarise(estimate = if (input$value_type == "percent") {mean(estimate / total, na.rm = TRUE) * 100} else {mean(estimate, na.rm = TRUE)}, 
                .groups = "drop") %>%
      mutate(frequency = factor(frequency, levels = names(frequency_colors)),
             hover_text = paste0("Date: ", format(mid_date, "%Y-%m-%d"), "<br>",
                                 if (input$value_type == "percent") {paste0("Percent: ", round(estimate, 1), "%")} else {
                                   paste0("Count: ", format(round(estimate, 0), big.mark = ","))}))
    
    p <- ggplot(df_plot, aes(x = mid_date, y = estimate, color = frequency, group = frequency, text = hover_text)) +
      geom_line() +
      geom_point(size = 1.5) +
      scale_color_manual(values = frequency_colors[names(frequency_colors) %in% df_plot$frequency]) +
      labs(title = paste("Frequency of Reported Anxiety Symptoms (", selected_group(), ")"),
           x = "Date", y = ifelse(input$value_type == "percent", "Percent (%)", "Counts"), color = "Response Intensity") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(size = 14))
    
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(t = 30), legend = list(orientation = "h", y = -0.5)) %>%
        config(displayModeBar = FALSE)
  })
  
  # Trend Summary Table Caption
  output$trend_summary_caption <- renderText({
    start <- format(input$date_range[1], "%B %Y")
    end <- format(input$date_range[2], "%B %Y")
    selected_group <- if (input$group_category == "total") "Total Population" else input$group_name
    
    paste0("From ", start, " to ", end, " for the group '", selected_group, "', showing trends by response:")
  })
  
  # Trend Summary Table
  output$trend_summary_table <- renderTable({
    df <- df_filtered()
    req(nrow(df) > 1)
    
    selected_group <- if (input$group_category == "total") {
      match_total <- unique(df$group_name[df$group_category == "total"])
      if (length(match_total) == 0) return(NULL)
      match_total[1]
    } else {
      req(input$group_name)
      input$group_name
    }
    
    if (!selected_group %in% unique(df$group_name)) {
      showNotification("Selected group not found in data.", type = "error")
      return(NULL)
    }
    
    summary_df <- df %>%
      filter(group_category == input$group_category, group_name == selected_group) %>%
      group_by(mid_date) %>%
      mutate(total = sum(estimate, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(mid_date, frequency) %>%
      summarise(value = if (input$value_type == "percent") {mean(estimate / total, na.rm = TRUE) * 100}
                else {mean(estimate, na.rm = TRUE)}, .groups = "drop") %>%
      group_by(frequency) %>%
      summarise(avg = mean(value, na.rm = TRUE), start = first(value), end = last(value),
                pct_change = round((end - start) / start * 100, 1), cv = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE) * 100,
                .groups = "drop") %>%
      mutate(`Trend` = case_when(pct_change > 0 ~ "Increasing", pct_change < 0 ~ "Decreasing", TRUE ~ "Flat"),
             `Average` = if (input$value_type == "percent") {round(avg, 1)} else {as.integer(round(avg, 0))},
             `% Change` = pct_change,
             `Volatility (%)` = round(cv, 1),
             `Volatility Level` = case_when(cv < 10 ~ "Low", cv < 30 ~ "Moderate", TRUE ~ "High")) %>%
      select(`Response Intensity` = frequency, `Trend`, `Average`, `% Change`, `Volatility (%)`, `Volatility Level`)
    
    summary_df
  })
  
  
  # Applying Prophet model
  group_forecast <- reactive({
    df <- df_filtered_prophet()
    req(nrow(df) > 0)
    
    total_df <- df %>% group_by(mid_date) %>% summarise(group_total = sum(estimate, na.rm = TRUE), .groups = "drop")     # for % calculation
    
    result_list <- list()
    
    for (freq in unique(df$frequency)) {
      df_freq <- df %>% 
        filter(frequency == freq) %>%
        group_by(mid_date) %>%
        summarise(estimate_value = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
        left_join(total_df, by = "mid_date") %>%
        mutate(y = if (input$value_type == "percent") {(estimate_value / group_total) * 100} else {estimate_value}) %>%
        select(ds = mid_date, y)
      
      if (nrow(df_freq) < 10 || all(is.na(df_freq$y))) {
        showNotification(paste("Skipping", freq, "- not enough data or all values are NA."), type = "warning")
        next
      }
      
      m <- prophet(df_freq, yearly.seasonality = FALSE, weekly.seasonality = FALSE)
      future <- make_future_dataframe(m, periods = input$forecast_months, freq = "month")
      forecast <- predict(m, future)
      df_freq$frequency <- freq
      forecast$frequency <- freq
      
      result_list[[as.character(freq)]] <- list(forecast = forecast, model = m, train = df_freq)
    }
    
    return(result_list)
  })
  
  # Forecast Plot
  output$forecast_plot <- renderPlot({
    prophet_result <- group_forecast()
    req(length(prophet_result) > 0) 
    
    group_name_label <- ifelse(input$group_category == "total", "Total Population", input$group_name)
    
    make_forecast_plot(prophet_result = prophet_result,
                       group_name = group_name_label,
                       value_type = input$value_type)
  })
  
  # Forecast Summary Table
  output$summary_stats <- renderTable({
    prophet_result <- group_forecast()
    req(length(prophet_result) > 0)
    
    make_summary_table(prophet_result = prophet_result,
                       forecast_months = input$forecast_months,
                       value_type = input$value_type,
                       show_accuracy = input$show_accuracy)
  })
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      if (input$active_tab == "Trends") {paste0("anxiety_plot_trends_", Sys.Date(), ".png")} 
      else if (input$active_tab == "Forecasting") {paste0("anxiety_plot_forecast_", Sys.Date(), ".png")}
    },
    
    content = function(file) {

      p <- NULL
      
      if (input$active_tab == "Trends") {

        p <- isolate({
          total_plot <- df_filtered() %>% 
            filter(group_category == "total", group_name == "Total") %>%
            group_by(mid_date) %>%
            mutate(total = sum(estimate, na.rm = TRUE)) %>%
            group_by(mid_date, frequency) %>%
            summarise(estimate = if (input$value_type == "percent") {mean(estimate / total, na.rm = TRUE) * 100} 
                      else {mean(estimate, na.rm = TRUE)}, .groups = "drop") %>%
            ggplot(aes(x = mid_date, y = estimate, color = frequency)) +
            geom_line() +
            geom_point(size = 1.5) +
            scale_color_manual(values = frequency_colors) +
            labs(title = "Frequency of Reported Anxiety Symptoms ( Total Group )",
                 x = "Date", y = ifelse(input$value_type == "percent", "Percent (%)", "Estimated Count"), color = "Response Intensity") +
            theme_minimal(base_size = 12) +
            theme(legend.position = "bottom",
                  panel.background = element_rect(fill = "white", color = NA),
                  plot.background = element_rect(fill = "white", color = NA))
          
          selected_group <- if (input$group_category == "total") "Total" else selected_group()
          
          select_plot <- df_filtered() %>% 
            filter(group_category == input$group_category, group_name == selected_group) %>% 
            group_by(mid_date) %>%
            mutate(total = sum(estimate, na.rm = TRUE)) %>%
            group_by(mid_date, frequency) %>%
            summarise(estimate = if (input$value_type == "percent") {mean(estimate / total, na.rm = TRUE) * 100} 
                      else {mean(estimate, na.rm = TRUE)}, .groups = "drop") %>%
            ggplot(aes(x = mid_date, y = estimate, color = frequency)) +
            geom_line() +
            geom_point(size = 1.5) +
            scale_color_manual(values = frequency_colors) +
            labs(title = if (input$group_category == "total") {
              "Selected Group: Total"
            } else {
              paste("Frequency of Reported Anxiety Symptoms (", selected_group, ")")
            },
                 x = "Date", y = ifelse(input$value_type == "percent", "Percent (%)", "Counts"), color = "Response Intensity") +
            theme_minimal(base_size = 12) +
            theme(legend.position = "bottom",
                  panel.background = element_rect(fill = "white", color = NA),
                  plot.background = element_rect(fill = "white", color = NA))
          
          grid.arrange(total_plot, select_plot, ncol = 1)
        })
        
      } else if (input$active_tab == "Forecasting") {

        p <- isolate({
          prophet_result <- group_forecast()
          req(length(prophet_result) > 0)
          
          group_name_label <- ifelse(input$group_category == "total", "Total Population", input$group_name)
          
          make_forecast_plot(prophet_result = prophet_result,
                             group_name = group_name_label,
                             value_type = input$value_type)
        })
      }
      
      ggsave(file, p, width = 10, height = 6)
    }
  )
  
  # Download table as CSV
  output$download_forecast <- downloadHandler(
    filename = function() {paste0("forecast_summary_", Sys.Date(), ".csv")},
    content = function(file) {
      
      prophet_result <- group_forecast()
      req(length(prophet_result) > 0)
      
      result_df <- make_summary_table(prophet_result = prophet_result,
                                      forecast_months = input$forecast_months,
                                      value_type = input$value_type,
                                      show_accuracy = TRUE)
      
      write.csv(result_df, file, row.names = FALSE)
    }
  )
  
}