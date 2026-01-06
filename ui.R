library(shiny)
library(DT)
library(readxl)
library(shinythemes)

# Helper function for dropdowns
get_unique_items <- function(column_data) {
  items <- unlist(strsplit(as.character(column_data), ",\\s*|;\\s*"))
  items <- unique(trimws(items))
  items <- items[items != "" & !is.na(items)]
  return(sort(items))
}

data_init <- read_excel("DATABASE.xlsx", sheet = 1)

fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      .dataTables_wrapper { overflow-x: auto; } 
      .container-fluid { max-width: 100%; }
      table.dataTable tbody td { vertical-align: top; }
    "))
  ),
  
  title = "Survey Explorer",
  theme = shinytheme("flatly"),
  
  div(style = "display: flex; align-items: center; padding: 20px 0;",
      img(src = "logo.png", height = "50px", style = "margin-right: 15px;"),
      h2("EU Loneliness Explorer", style = "margin: 0; color: #2c3e50;")
  ),
  
  tabsetPanel(
    # --- TAB 1: DATABASE ---
    tabPanel("Database",
             br(),title = "Survey Explorer",
             wellPanel(
               fluidRow(
                 column(width = 12, sm = 3, selectInput("country", "Country (AND):", choices = get_unique_items(data_init$Country), multiple = TRUE)),
                 column(width = 12, sm = 3, selectInput("year", "Year (AND):", choices = sort(unique(as.numeric(data_init$Year)), decreasing = TRUE), multiple = TRUE)),
                 column(width = 12, sm = 3, selectInput("scale", "Scale (AND):", choices = get_unique_items(data_init$Scale_family), multiple = TRUE)),
                 column(width = 10, sm = 2, selectInput("topic", "Topic (AND):", choices = get_unique_items(data_init$Topic), multiple = TRUE)),
                 column(width = 2, sm = 1, br(), actionButton("reset", "", icon = icon("refresh"), class = "btn-warning", style="width:100%"))
               )
             ),
             DTOutput("table")
    ),
    
    # --- TAB 2: ANALYTICS ---
    tabPanel("Analytics",
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Chart Settings"),
                 selectInput("plot_var", "Visualize Distribution of:", 
                             choices = c("Country", "Year", "Topic", "Scale_family", "Mode", "Type", "Population")),
                 helpText("Charts update automatically based on filters set in the Database tab.")
               ),
               mainPanel(plotOutput("distPlot", height = "600px"))
             )
    ),
    
    # --- TAB 3: HOW TO USE THE APP ---
    tabPanel("How to use the app",
             br(),
             h4("Quick Start Guide"),
             tags$b("1. Filtering"),
             p("Filters use 'AND' logic. If you select multiple values (e.g., Italy and France), only surveys containing BOTH will be shown."),
             tags$b("2. Navigation"),
             p("The database table supports horizontal scrolling. Swipe or scroll sideways to view all columns."),
             tags$b("3. Column Visibility"),
             p("Use the 'Select Columns' button to choose which fields to display on screen."),
             tags$b("4. Exporting"),
             p("The CSV and Excel buttons save ALL filtered results, not just the currently visible page.")
    ),
    
    # --- TAB 4: LEGEND ---
    tabPanel("Legend",
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