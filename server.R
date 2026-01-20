library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(stringr)

function(input, output, session) {
  
  # Reset logic - Now resets the checkbox too
  observeEvent(input$reset, {
    updateSelectInput(session, "country", selected = character(0))
    updateSelectInput(session, "year", selected = character(0))
    updateSelectInput(session, "scale", selected = character(0))
    updateSelectInput(session, "topic", selected = character(0))
    updateSelectInput(session, "type", selected = character(0))
    updateSelectInput(session, "population", selected = character(0))
    updateCheckboxInput(session, "comp_only", value = FALSE)
  })
  
  # Reactive filtering
  filtered_data <- reactive({
    df <- read_excel("DATABASE.xlsx", sheet = 1)
    df <- df %>% mutate(across(everything(), as.character))
    df[is.na(df)] <- ""
    
    # 1. Apply Checkbox Logic (Comparative = contains at least one comma)
    if (input$comp_only) {
      df <- df[grepl(",", df$Country), ]
    }
    
    # 2. Apply standard Dropdown Filters
    if (!is.null(input$country)) { for (val in input$country) { df <- df[grepl(val, df$Country, fixed = TRUE), ] } }
    if (!is.null(input$year)) { for (val in input$year) { df <- df[df$Year == as.character(val), ] } }
    if (!is.null(input$scale)) { for (val in input$scale) { df <- df[grepl(val, df$Scale_family, fixed = TRUE), ] } }
    if (!is.null(input$topic)) { for (val in input$topic) { df <- df[grepl(val, df$Topic, fixed = TRUE), ] } }
    if (!is.null(input$type)) { for (val in input$type) { df <- df[grepl(val, df$Type, fixed = TRUE), ] } }
    if (!is.null(input$population)) { for (val in input$population) { df <- df[grepl(val, df$Population, fixed = TRUE), ] } }
    
    df
  })
  
  # Render Table
  output$table <- renderDT({
    datatable(
      filtered_data(),
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        ordering = FALSE, pageLength = 12, scrollX = TRUE, autoWidth = FALSE,
        dom = 'Bfrtip',
        language = list(search = "Global Search:"),
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
