library(shiny)
library(DT)
library(readxl)
library(shinythemes)

# Helper function
get_unique_items <- function(column_data) {
  items <- unlist(strsplit(as.character(column_data), ",\\s*|;\\s*"))
  items <- unique(trimws(items))
  items <- items[items != "" & !is.na(items)]
  return(sort(items))
}

data_init <- read_excel("DATABASE.xlsx", sheet = 1)

fluidPage(
  title = "Survey Explorer",
  theme = shinytheme("flatly"),
  
  div(style = "display: flex; align-items: center; padding: 20px 0;",
      img(src = "logo.png", height = "60px", style = "margin-right: 20px;"),
      h1("EU Loneliness Survey Explorer", style = "margin: 0; color: #2c3e50;")
  ),
  
  tabsetPanel(
    tabPanel("Database",
             br(),
             wellPanel(
               fluidRow(
                 column(3, selectInput("country", "Countries (AND):", choices = get_unique_items(data_init$Country), multiple = TRUE)),
                 column(3, selectInput("year", "Years (AND):", choices = sort(unique(as.numeric(data_init$Year)), decreasing = TRUE), multiple = TRUE)),
                 column(3, selectInput("scale", "Scales (AND):", choices = get_unique_items(data_init$Scale_family), multiple = TRUE)),
                 column(2, selectInput("topic", "Topics (AND):", choices = get_unique_items(data_init$Topic), multiple = TRUE)),
                 column(1, br(), actionButton("reset", "Reset", icon = icon("refresh"), class = "btn-warning"))
               )
             ),
             DTOutput("table")
    ),
    
    tabPanel("Analytics",
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Chart Settings"),
                 selectInput("plot_var", "Select Variable to Visualize:", 
                             choices = c("Country", "Year", "Topic", "Scale_family", "Mode", "Type", "Population")),
                 helpText("Charts update automatically based on your active filters.")
               ),
               mainPanel(
                 plotOutput("distPlot", height = "600px")
               )
             )
    ),
    
    tabPanel("How to use the app",
             br(),
             h4("User Guide"),
             tags$ul(
               tags$li(tags$b("Database:"), " Filters use 'AND' logic. Export buttons (CSV/Excel) will save ALL filtered results, across all pages."),
               tags$li(tags$b("Analytics:"), " Visualize distributions. Multi-value cells are split so each item is counted individually."),
               tags$li(tags$b("Reset:"), " Clears all selections to show the full database.")
             )
    ),
    
    tabPanel("Column Legend",
             br(),
             h4("Data Dictionary"),
             HTML("
             <table class='table table-striped' style='width:100%'>
                <thead>
                  <tr style='background-color: #f2f2f2;'>
                    <th>Column Name</th>
                    <th>Description</th>
                  </tr>
                </thead>
                <tbody>
                  <tr><td><b>Survey_id</b></td><td>Unique identifier for each survey. Matches the corresponding file in the 'codebook' folder.</td></tr>
                  <tr><td><b>Source</b></td><td>The origin or platform from which the dataset was retrieved.</td></tr>
                  <tr><td><b>Study_name</b></td><td>Name of the research paper associated with the survey (if applicable).</td></tr>
                  <tr><td><b>Article_link</b></td><td>Direct link to the published research paper.</td></tr>
                  <tr><td><b>Survey_name</b></td><td>Full official name of the survey.</td></tr>
                  <tr><td><b>Survey_family</b></td><td>Acronym or series name for the survey.</td></tr>
                  <tr><td><b>Year</b></td><td>The year(s) of data collection.</td></tr>
                  <tr><td><b>Link</b></td><td>Direct URL to the survey data or primary source documentation.</td></tr>
                  <tr><td><b>Country</b></td><td>List of countries included in the study.</td></tr>
                  <tr><td><b>N</b></td><td>Total number of participants (sample size).</td></tr>
                  <tr><td><b>Mode</b></td><td>Data collection method (e.g., Face-to-Face, CAWI, Mixed).</td></tr>
                  <tr><td><b>Type</b></td><td>Research design: Panel, Cross-section, or Repeated Cross-section.</td></tr>
                  <tr><td><b>Sample</b></td><td>Sampling methodology: Representative or Convenience sample.</td></tr>
                  <tr><td><b>Population</b></td><td>Targeted age demographics: Young, Adults, or Elderly.</td></tr>
                  <tr><td><b>Var_name</b></td><td>Variable name(s) for loneliness in the codebook. Multiple entries separated by semicolons (;).</td></tr>
                  <tr><td><b>Item_text</b></td><td>Exact wording of the loneliness question(s). Multiple items separated by semicolons (;).</td></tr>
                  <tr><td><b>Response_scale</b></td><td>Number of answer options provided. Multiple entries separated by semicolons (;).</td></tr>
                  <tr><td><b>Anchors</b></td><td>Labels for the extreme endpoints of the answer scale. Multiple entries separated by semicolons (;).</td></tr>
                  <tr><td><b>Scale_family</b></td><td>Type of validated loneliness scale used (e.g., UCLA, DJG-6). Multiple entries separated by semicolons (;).</td></tr>
                  <tr><td><b>Topic</b></td><td>Primary research topics covered by the survey.</td></tr>
                </tbody>
             </table>
             ")
    )
  )
)