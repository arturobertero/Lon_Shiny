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
      .landing-text { font-size: 1.2em; line-height: 1.6; max-width: 900px; margin: 20px 0; }
    "))
  ),
  
  theme = shinytheme("flatly"),
  
  div(style = "display: flex; align-items: center; padding: 20px 0;",
      img(src = "logo.png", height = "60px", style = "margin-right: 20px;"),
      h2("EU Loneliness Explorer Pro", style = "margin: 0; color: #2c3e50;")
  ),
  
  tabsetPanel(
    # --- NEW TAB 1: HOME (LANDING PAGE) ---
    tabPanel("Home",
             br(),
             div(class = "landing-text",
                 h4("Welcome to the LONELY-EU Database Explorer"),
                 p("This Shiny app provides interactive access to the ", tags$b("LONELY-EU WP3_1A database"), ", which maps datasets measuring loneliness across the European Union and the United Kingdom."),
                 p("Full methodological documentation—including the preregistered search protocol, inclusion criteria, data extraction procedures, and variable definitions—is provided in the accompanying technical report hosted on OSF: ", 
                   tags$a(href = "https://osf.io/3t9kg/overview", "https://osf.io/3t9kg/overview", target = "_blank"), "."),
                 hr(),
                 p(tags$i("Citation Notice:"), " If you use this app or the underlying database in any publication, report, or presentation, please cite the OSF technical report. This helps ensure appropriate attribution and allows others to reference the exact version and methodology underlying the database.")
             )
    ),
    
    # --- TAB 2: DATABASE ---
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
    
    # --- TAB 3: ANALYTICS ---
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
    
    # --- TAB 4: USER GUIDE ---
    tabPanel("User Guide",
             br(),
             h4("How to use this app"),
             tags$ul(
               tags$li(tags$b("Comparative Filter:"), " Ticking 'Comparative surveys' filters for datasets fielded in more than one country."),
               tags$li(tags$b("Dropdowns:"), " Standard filters use 'AND' logic (results match all selected criteria)."),
               tags$li(tags$b("Global Search:"), " Use the search bar at the top right of the table to find keywords across all cells."),
               tags$li(tags$b("Scrolling:"), " The table supports horizontal scrolling/swiping for full column visibility."),
               tags$li(tags$b("Export:"), " Buttons for CSV/Excel save your currently filtered dataset.")
             )
    ),
    
    # TAB 5: LEGEND 
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