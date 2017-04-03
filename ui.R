library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(leaflet)

##################################################
# Header
##################################################
header <- shinydashboard::dashboardHeader(
  title = "Benthic Analysis Online Tool - Development Version",
  titleWidth = 550,
  dropdownMenu(type = "notifications",
               notificationItem(
                 text = "Development version",
                 icon("question")
               )
  )
)

##################################################
# Sidebar
##################################################
sidebar <- shinydashboard::dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
              useShinyjs(),
    menuItem("Introduction", tabName = "introduction", icon = icon("list-alt")
    ),
    menuItem("Data Input", icon = icon("th"), tabName = "datainput",
             menuItem("Raw Data upload",tabName = "rawdatainput",icon=NULL),
            conditionalPanel("input.sidebarmenu === 'rawdatainput'",
                             useShinyjs(),
                             fileInput(inputId="inrawbioFile",multiple = FALSE, label = h5("Choose CSV file"),accept=c(".csv",".txt")),
                             checkboxInput("metdata",label=h5("Input data are metrics"),value=F),
                             radioButtons(inputId="rawFormat", label = h5("Input File Format"),choices = list("None Selected"=0,"Long"="Long" ,"Wide"="Wide" ),inline=F, selected = 0)
                             ),
            conditionalPanel("output.finalizeRaw==true && input.sidebarmenu === 'rawdatainput'",
                             actionButton("finalize_raw","Finalize"),
                             actionButton("clear_all","Clear")
                             ),
            menuItem("Metric Transformations",tabName = "transSummaryMetrics",icon=NULL),
            menuItem("User Matched Reference Sites",tabName = "userMatchRef",icon=NULL),
            menuItem("Mapping",tabName = "Mapping",icon=NULL)
    ),
    menuItem("RCA",tabname="RCA",icon=icon("tint", lib = "glyphicon")),
    menuItem("Trends",tabname="Trends",icon=icon("tree-deciduous", lib = "glyphicon"))
  )
)

##################################################
# Body
##################################################
body <- shinydashboard::dashboardBody(
  tabItems(
    ##################################################
    # Intruduction Page
    ##################################################
    
    tabItem(tabName = "introduction",
            box(title="Introduction",width=12,status="info",collapsible = T, collapsed = F,solidHeader = T,
                h3("UNDER HEAVY DEVELOPMENT"),
                helpText("Online tool for analysis of aquatic biomonitoring data")
                ),
            box(title="Implemented features",width=12,status="info",collapsible = T, collapsed = T,solidHeader = T,
                helpText("- Single input file for the follow data types: taxa or metrics, habitat descriptors, coordinates of sampling point"),
                helpText("- Support for long and wide data formats (long format recommended for lowest-practical-level taxonomy)"),
                helpText("- Support for wide data formats with multiple column headers"),
                helpText("- Calculation of indicator metrics for Benthic Macroinvertebrate taxa"),
                helpText("- HBI will be calculated at family level only - for now"),
                helpText("- Functional metrics can be calculated below family level"),
                helpText("- Download calculated Summary metrics as .csv file")
                ),
            box(title="Planned features",width=12,status="info",collapsible = T, collapsed = T,solidHeader = T,
                helpText("- Better support for lowest-practical-level taxonomy"),
                helpText("- GIS intigration for mapping of test and reference sites"),
                helpText("- GIS intigration for calculation of catchment and site level attributes"),
                helpText("- Reference Condition Approach Bioassessment using Test Site Analysis and Assessment by Nearest-Neighbour Analysis"),
                helpText("- Trend analysis using Generalized Linear Mixed-Effects models")
                ),
            box(title="Known Bugs",width=12,status="info",collapsible = T, collapsed = T,solidHeader = T,
                helpText("- none")
                )
    ),
    ##################################################
    # Raw Data Input
    ##################################################
    
    tabItem(tabName = "rawdatainput",
            useShinyjs(),
            
            h2("Raw Data Input"),
            fluidRow(
              tabBox("Data",width=12,
                tabPanel("Raw",status="warning",collapsible = T,solidHeader = T,
                         fluidRow(
                           DT::dataTableOutput("rawDataView"),
                           br(),
                           br()
                         ),
                         fluidRow(
                           column(width=6,
                                  box(title="Columns",width=12,status="primary",collapsible = T,solidHeader = T,collapsed = T,
                                      helpText("Highlight columns (multiple with shift) then assign them to an attribute."),
                                      conditionalPanel(condition="input.rawFormat == 'Wide'",
                                                       numericInput("rawData_taxarows", label = h5("Rows of column Identifiers"), min=1,value=1)
                                      ),
                                      conditionalPanel("input.rawFormat == 'Wide' && input.rawData_taxarows == 1 ",
                                                       textInput("taxa_sep", label = h5("Character that separates taxa names"), value = ";")
                                      ),
                                      uiOutput("wideTaxaCols1")
                                  )
                                  
                           ),
                           column(width=6,
                                  conditionalPanel("input.rawFormat == 'Wide'||input.rawFormat == 'Long'",
                                                   box(title="Assign Columns to Attributes", width=NULL,status="success",collapsible = T,solidHeader = T,collapsed = T,                                                       column(width=6,
                                                              actionButton("raw.siteID.cols", "Site/Sampling Events"),
                                                              br(),
                                                              actionButton("raw.taxa.cols", "Taxa or Metrics"),
                                                              br(),
                                                              actionButton("raw.habitat.cols", "Habitat Descriptors (Optional)"),
                                                              br(),
                                                              conditionalPanel("input.rawFormat == 'Long'",actionButton("raw.abund.cols", "Abundances"))
                                                       ),
                                                       column(width=6,
                                                              actionButton("raw.siteID.cols.rem", "Undo"),
                                                              br(),
                                                              actionButton("raw.taxa.cols.rem", "Undo"),
                                                              br(),
                                                              actionButton("raw.habitat.cols.rem", "Undo"),
                                                              br(),
                                                              conditionalPanel("input.rawFormat == 'Long'",actionButton("raw.abund.cols.rem", "Undo"))
                                                       )
                                      )
                                  ),
                                  conditionalPanel("input.rawFormat == 'Wide'||input.rawFormat == 'Long'",
                                                   box(title="Test vs. Reference (Optional)",width=NULL,status="success",collapsible = T,solidHeader = T,collapsed = T,
                                                       uiOutput("test.vs.ref"),
                                                       actionButton("raw.testref.cols", "Test and Reference Sites"),
                                                       actionButton("raw.testref.cols.rem", "Undo")
                                                   ),
                                                   box(title="Coordinates (Optional)",width=NULL,status="success",collapsible = T,solidHeader = T,collapsed = T,
                                                       uiOutput("eastingCols"),
                                                       uiOutput("northingCols"),
                                                       uiOutput("ESPGCols"),
                                                       actionButton("raw.coord.cols", "Coordinates"),
                                                       actionButton("raw.coord.cols.rem", "Undo")
                                                   )
                                  )
                           )
                         )
                         
                         ),
                tabPanel("Taxa",
                         useShinyjs(),
                         
                         DT::dataTableOutput("view.taxa"),
                         br(),
                         br(),
                         conditionalPanel(condition="input.metdata == false && input.finalize_raw>0 && output.show_mets == true",
                                          fluidRow(
                                            column(width=6,
                                                   box(title="Calculate Summary Metrics",width=12,status="primary",collapsible = T,solidHeader = T,collapsed = F,
                                                       actionButton("calculate_metrics","Calculate Summary Metrics"),
                                                       conditionalPanel(condition="input.metdata == false && input.calculate_metrics>0 && output.show_mets2 == true",
                                                                        downloadButton("download_raw_mets","Download Summary Metrics")
                                                                        )
                                                       )
                                                   )
                                            )
                                          ),
                         conditionalPanel(condition="input.metdata == false && input.calculate_metrics>0 && output.show_mets == true",
                                          fluidRow(
                                            column(width=12,
                                                   box(title="Summary Metrics",width=12,status="primary",collapsible = T,solidHeader = T,collapsed = T,
                                                       DT::dataTableOutput("view.metrics.raw")
                                                   )
                                            )
                                          )
                         )
                         ),
                tabPanel("Habitat",
                         DT::dataTableOutput("view.habitat"),
                         br(),
                         br()
                         )
              )
            )
            )#,
    #tabItem(tabname="summary.metrics")
    )
  )





dashboardPage(
  header,
  sidebar,
  body
)



