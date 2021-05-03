# Header
header <- shinydashboard::dashboardHeader(
  title = "GeoMx Cloud"
)

# Sidebar
sidebar <- shinydashboard::dashboardSidebar(
  conditionalPanel(
    condition = "output.analysisRan == false",
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Overview",
                               tabName = "overview", icon = icon("dashboard")),
      shinydashboard::menuItem(
        "Data upload",
        tabName = "data",
        icon = icon("database")
      ),
      shinydashboard::menuItem(
        "Methods",
        tabName = "methods",
        icon = icon("flask")
      ),
      shinydashboard::menuItem(
        "Analysis",
        tabName = "analysis",
        icon = icon("bar-chart-o")
      ),
      shinydashboard::menuItem(
        "Generate Report",
        tabName = "report",
        icon = icon("clipboard-list")
      )
    )
  ),
  conditionalPanel(
    condition = "output.analysisRan == true",
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Overview",
                               tabName = "overview", icon = icon("dashboard")),
      shinydashboard::menuItem(
        "Data upload",
        tabName = "data",
        icon = icon("database")
      ),
      shinydashboard::menuItem(
        "Methods",
        tabName = "methods",
        icon = icon("flask")
      ),
      shinydashboard::menuItem(
        "Analysis",
        tabName = "analysis",
        icon = icon("bar-chart-o"),
        shinydashboard::menuSubItem("Metadata Analysis",
                                    tabName = "subitem1"),
        shinydashboard::menuSubItem("Exploratory Data Analysis",
                                    tabName = "subitem2"),
        shinydashboard::menuSubItem("Differential Expression",
                                    tabName = "subitem3"),
        shinydashboard::menuSubItem("Biomarker Panels",
                                    tabName = "subitem4")
      ),
      shinydashboard::menuItem(
        "Generate Report",
        tabName = "report",
        icon = icon("clipboard-list")
      )
    )
  )
)

## Body
body <- shinydashboard::dashboardBody(
    # tags$style(".content {background-color: white;}"),
    # tags$style(type="text/css", ".modelInput label{ display: table-cell; }
    #             .modelInput .form-group { display: table-row;}"),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )),
  dashboardthemes::shinyDashboardThemes(
    theme = "grey_dark"
  ),
    fluidPage(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "overview",
          fluidRow(align = "center",
            valueBoxOutput("author"),
            valueBoxOutput("source"),
            valueBoxOutput("contest")
          ),
          fluidRow(HTML("<img src='app-architecture.png'
            id='app-architecture'/>")),
        ),
        shinydashboard::tabItem(
          tabName = "data",
          geomxCloud::data_upload_ui("data_upload")
        ),
        shinydashboard::tabItem("subitem1",
                                geomxCloud::metadata_ui("metadata")
        ),
        shinydashboard::tabItem("subitem2",
                                geomxCloud::eda()
        ),
        shinydashboard::tabItem("subitem3",
                                geomxCloud::dea()
        ),
        shinydashboard::tabItem("subitem4",
                                geomxCloud::biomarker_discovery_analysis()
        ),
        shinydashboard::tabItem(
          tabName = "methods",
          h5("R version 3.6.1"),
          h5("RStudio version 1.2.5019."),
          fluidRow(column(6, h3("Analyses")),
            column(6, h3("R-packages"))),
          fluidRow(column(6, h5("Development of web application")),
            column(6, h5("shiny (v1.1), shinydashboard (v0.7.1),
              shinyBS (0.61)"))),
          fluidRow(column(6, h5("Data wrangling")),
            column(6, h5("tidyverse (v1.2.1)"))),
          fluidRow(column(6, h5("Exploratory Data Analysis")),
            column(6, h5("stats (v3.5.1)"))),
          fluidRow(column(6, h5("Differential expression analysis")),
            column(6, h5("limma (v3.40.6)"))),
          fluidRow(column(6, h5("Biomarker analysis")),
            column(6, h5("caret (v6.0-84), glmnet (v2.0-18), pROC (v1.15.3)"))),
          fluidRow(column(6, h5("Pathway enrichment analysis")),
            column(6, h5("enrichR (v2.1)"))),
          fluidRow(column(6, h5("graphical visualizations")),
            column(6, h5("plotly (v4.9.1), d3heatmap (v0.6.1.2),
              RColorBrewer (v1.1-2), visNetwork (v2.0.9), googleVis (v0.6.4),
              igraph (v1.2.4.1), canvasXpress (v1.26.5), DT (v0.11), UpSetR (v1.4.0)")))
        ),
        shinydashboard::tabItem(
          tabName = "report",
          geomxCloud::report_ui("report")
        )
      )
    )
)


shinydashboard::dashboardPage(
  skin = "blue",
  header,
  sidebar,
  body
)
