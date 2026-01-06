library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)

function(input, output, session) {
  
  # Reset logic
  observeEvent(input$reset, {
    updateSelectInput(session, "country", selected = character(0))
    updateSelectInput(session, "year", selected = character(0))
    updateSelectInput(session, "scale", selected = character(0))
    updateSelectInput(session, "topic", selected = character(0))
  })
  
  # Reactive data filtering
  filtered_data <- reactive({
    df <- read_excel("DATABASE.xlsx", sheet = 1)
    df <- df %>% mutate(across(everything(), as.character))
    df[is.na(df)] <- ""
    
    if (!is.null(input$country)) { for (val in input$country) { df <- df[grepl(val, df$Country, fixed = TRUE), ] } }
    if (!is.null(input$year)) { for (val in input$year) { df <- df[df$Year == as.character(val), ] } }
    if (!is.null(input$scale)) { for (val in input$scale) { df <- df[grepl(val, df$Scale_family, fixed = TRUE), ] } }
    if (!is.null(input$topic)) { for (val in input$topic) { df <- df[grepl(val, df$Topic, fixed = TRUE), ] } }
    df
  })
  
  # Database Table with horizontal scroll and no-sort
  output$table <- renderDT({
    datatable(
      filtered_data(),
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        ordering = FALSE,
        pageLength = 12,
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = 'Brtip',
        buttons = list(list(extend = 'colvis', text = 'Select Columns'), 'csv', 'excel')
      ),
      escape = FALSE
    )
  }, server = FALSE)
  
  # Analytics Plot
  output$distPlot <- renderPlot({
    df_plot <- filtered_data()
    if(nrow(df_plot) == 0) return(NULL)
    
    plot_ready <- df_plot %>%
      separate_rows(!!sym(input$plot_var), sep = ",\\s*|;\\s*") %>%
      filter(!!sym(input$plot_var) != "") %>%
      group_by(!!sym(input$plot_var)) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    ggplot(plot_ready, aes(x = reorder(!!sym(input$plot_var), count), y = count)) +
      geom_bar(stat = "identity", fill = "#2c3e50") +
      coord_flip() +
      labs(title = paste("Distribution of", input$plot_var), x = "", y = "Count") +
      theme_minimal(base_size = 14)
  })
}