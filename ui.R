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
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      .dataTables_wrapper { overflow-x: auto; } 
      .container-fluid { max-width: 100%; }
      table.dataTable tbody td { vertical-align: top; }
      .checkbox-inline { margin-top: 25px; font-weight: bold; }
    "))
  ),
  
  theme = shinytheme("flatly"),
  
  div(style = "display: flex; align-items: center; padding: 20px 0;",
      img(src = "logo.png", height = "50px", style = "margin-right: 15px;"),
      h2("EU Loneliness Explorer", style = "margin: 0; color: #2c3e50;")
  ),
  
  tabsetPanel(
    
    # TAB 1: MAIN 
    tabPanel("Database",
             br(),
             wellPanel(
               fluidRow(
                 column(width = 4, sm = 2, selectInput("country", "Country (AND):", choices = get_unique_items(data_init$Country), multiple = TRUE)),
                 column(width = 4, sm = 2, selectInput("year", "Year (AND):", choices = sort(unique(as.numeric(data_init$Year)), decreasing = TRUE), multiple = TRUE)),
                 column(width = 4, sm = 2, selectInput("scale", "Scale (AND):", choices = get_unique_items(data_init$Scale_family), multiple = TRUE)),
                 column(width = 4, sm = 2, selectInput("topic", "Topic (AND):", choices = get_unique_items(data_init$Topic), multiple = TRUE)),
                 column(width = 4, sm = 2, selectInput("type", "Type (AND):", choices = get_unique_items(data_init$Type), multiple = TRUE)),
                 column(width = 4, sm = 1, selectInput("population", "Population (AND):", choices = get_unique_items(data_init$Population), multiple = TRUE)),
                 column(width = 8, sm = 1, checkboxInput("comp_only", "Comparative surveys", value = FALSE)),
                 column(width = 4, sm = 1, br(), actionButton("reset", "", icon = icon("refresh"), class = "btn-warning", style="width:100%"))
               )
             ),
             DTOutput("table")
    ),
    
    # TAB 2: ANALYTICS 
    tabPanel("Analytics",
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Chart Settings"),
                 selectInput("plot_var", "Visualize Distribution of:", 
                             choices = c("Country", "Year", "Topic", "Scale_family", "Availability", "Type", "Population")),
                 helpText("Charts reflect the filters applied in the Database tab.")
               ),
               mainPanel(plotOutput("distPlot", height = "600px"))
             )
    ),
    
    # TAB 3: HOW TO USE 
    tabPanel("How to use the app",
             br(),
             h4("Quick Start Guide"),
             tags$ul(
               tags$li(tags$b("Comparative Filter:"), " Ticking 'Comparative only' will filter the database to show only surveys fielded in more than one country (detected by commas in the Country column)."),
               tags$li(tags$b("Filters:"), " Standard dropdowns use 'AND' logic."),
               tags$li(tags$b("Export:"), " Buttons for CSV/Excel save all currently filtered rows.")
             )
    ),
    
    # TAB 4: LEGEND 
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
                  <tr><td><b>Availability</b></td><td>Indicates if the survey codebook is publicly available (Yes) or requires registration (No).</td></tr>
                </tbody>
             </table>
             ")
    )
  )
)