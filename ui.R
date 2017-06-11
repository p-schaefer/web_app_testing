library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(leaflet)
#library(mapview)
library(leaflet.minicharts)

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
              menuItem("Introduction", tabName = "introduction", icon = icon("list-alt"),
                       menuSubItem("Details",tabName = "Details")
              ),
              menuItem("Data Input", icon = icon("th"), tabName = "datainput",
                       menuSubItem("Raw Data upload",tabName = "rawdatainput",icon=NULL),
                       conditionalPanel("input.sidebarmenu === 'rawdatainput'",
                                        useShinyjs(),
                                        fileInput(inputId="inrawbioFile",multiple = FALSE, label = h5("Choose CSV file"),accept=c(".csv",".txt")),
                                        checkboxInput("metdata",label=h5("Input data are metrics"),value=F),
                                        radioButtons(inputId="rawFormat", label = h5("Input File Format"),choices = list("None Selected"=0,"Long"="Long" ,"Wide"="Wide" ),inline=F, selected = 0),
                                        conditionalPanel("output.finalizeRaw==true",
                                                         actionButton("finalize_raw","Finalize"),
                                                         actionButton("clear_all","Clear")
                                        )
                       ),
                       menuSubItem("Metric Transformations",tabName = "transSummaryMetrics",icon=NULL),
                       menuSubItem("User Matched Reference Sites",tabName = "userMatchRef",icon=NULL)
              ),
              menuItem("Mapping",tabName="Mapping", icon=icon("map")#, 
                       #menuSubItem("Setup",tabName = "Mappingsetup",icon=NULL),
                       #conditionalPanel("input.sidebarmenu === 'Mappingsetup'",
                       #                  h5("Mapping options")
                       # )
              ),
              menuItem("Data Exploration",tabname="DataExploration",icon=icon("tint", lib = "glyphicon"),
                       menuSubItem("Setup",tabName = "eaxplorationSetup",icon=NULL)
              ),
              menuItem("RCA Single",tabname="RCA_single",icon=icon("gear"),
                       menuSubItem("Setup",tabName = "RCA_singleSetup",icon=NULL)
              ),
              menuItem("RCA Batch",tabname="RCA_batch",icon=icon("gears"),
                       menuSubItem("Setup",tabName = "RCA_batchSetup",icon=NULL)
              ),
              menuItem("Trends",tabname="Trends",icon=icon('line-chart'),
                       menuSubItem("Setup",tabName = "TrendsSetup",icon=NULL)
              )
  )
)

##################################################
# Body
##################################################
body <- shinydashboard::dashboardBody(
  tabItem(tabName="introduction",
          
          ##################################################
          # Introduction
          ##################################################
          #conditionalPanel("input.sidebarmenu === 'Details'",
          #),
          
          conditionalPanel("input.sidebarmenu === 'Details'",
                           column(width=6,
                                  box(title=h3("Online tool for analysis of aquatic biomonitoring data"),width=12,status="info",collapsible = F, collapsed = F,solidHeader = F,
                                      h4("UNDER HEAVY DEVELOPMENT"),
                                      hr(),
                                      helpText("A website for the analysis of Benthic Macroinvertebrate (BMI) data
                                     using a Reference Condition Approach. Impairment is determined using the Test Site Analysis (TSA). 
                                     This package provides functionallity for:"),
                                      helpText("1) calculation of many commonly used indicator metrics for assessing the status BMI communities;"),
                                      helpText("2) nearest-neighbour site matching using Assessment by Nearest-Neighbour
                                        Analysis (ANNA), the Redundancy Analysis variant of ANNA (RDA-ANNA) or user-defined reference sites;"),
                                      helpText("3) calculation of common Test Site Analysis parameters, including: F-statistic, non-centrality parameter, interval and equivalnce tests,
                                     z-scores for all calculated metrics, mahalanobis distance scores for all sites, partial mahalanobis distance scores
                                     for assessing significance of individual metrics, as well as upper and lower thresholds for impairment ranks;"),
                                      helpText("4) a variety of diagnostic plots and tools for assessing the confidence of the impairment rank. These include a non-paramtetric randomization
                                     test for the impairment rank, jacknife confidence intervals and consistency scores of the selected reference sites and jacknife consistancy of the 
                                     entire reference set.")
                                      
                                  )
                           ),
                           column(width=4,offset=1,
                                  box(title="Implemented features",width=12,status="info",collapsible = T, collapsed = T,solidHeader = T,
                                      helpText("- Single input file for the follow data types: taxa or metrics, habitat descriptors, coordinates of sampling point"),
                                      helpText("- Support for long and wide data formats (long format recommended for lowest-practical-level taxonomy)"),
                                      helpText("- Support for wide data formats with multiple column headers"),
                                      helpText("- Calculation of indicator metrics for Benthic Macroinvertebrate taxa"),
                                      helpText("- HBI will be calculated at family level only - for now"),
                                      helpText("- Functional metrics can be calculated below family level"),
                                      helpText("- Download calculated Summary metrics as .csv file"),
                                      helpText("- Download calculated Taxa Attribute Data as .csv file"),
                                      helpText("- Common transformations for metrics"),
                                      helpText("- Mapping of sampling locations")
                                      
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
                           )
          ),
          
          ##################################################
          # Raw Data Input
          ##################################################
          
          conditionalPanel("input.sidebarmenu === 'rawdatainput'",
                           useShinyjs(),
                           h2("Raw Data Input"),
                           actionLink("raw.help","Help"),
                           br(),
                           br(),
                           fluidRow(
                             tabBox("Data",width=12,
                                    tabPanel("Raw",status="warning",collapsible = T,solidHeader = T,
                                             fluidRow(
                                               DT::dataTableOutput("rawDataView"),
                                               br(),
                                               br()
                                             ),
                                             fluidRow(
                                               column(width=4,
                                                      box(title="Columns",width=12,status="primary",collapsible = F,solidHeader = T,collapsed = F,
                                                          helpText("Highlight columns (multiple with shift) then assign them to an attribute on the right."),
                                                          conditionalPanel(condition="input.rawFormat == 'Wide'",
                                                                           numericInput("rawData_taxarows", label = h5("Rows of column Identifiers"), min=1,value=1)
                                                          ),
                                                          conditionalPanel("input.rawFormat == 'Wide' && input.rawData_taxarows == 1 ",
                                                                           textInput("taxa_sep", label = h5("Character that separates taxa names"), value = ";")
                                                          ),
                                                          uiOutput("wideTaxaCols1")
                                                      )
                                                      
                                               ),
                                               column(width=4,
                                                      conditionalPanel("input.rawFormat == 'Wide'||input.rawFormat == 'Long'",
                                                                       box(title="Assign Columns", width=NULL,status="success",collapsible = F,solidHeader = T,collapsed = F,
                                                                           column(width=6,
                                                                                  actionButton("raw.siteID.cols", "Site/Sampling Events"),
                                                                                  br(),
                                                                                  actionButton("raw.taxa.cols", "Taxa or Metrics"),
                                                                                  br(),
                                                                                  actionButton("raw.habitat.cols", "Habitat Descriptors (Optional)"),
                                                                                  br(),
                                                                                  conditionalPanel("input.rawFormat == 'Long'",actionButton("raw.abund.cols", "Abundances"))
                                                                           ),
                                                                           column(width=4,
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
                                                                           actionButton("raw.testref.cols", "Finalize Test and Reference Sites"),
                                                                           actionButton("raw.testref.cols.rem", "Undo")
                                                                       ),
                                                                       box(title="Coordinates (Optional)",width=NULL,status="success",collapsible = T,solidHeader = T,collapsed = T,
                                                                           uiOutput("eastingCols"),
                                                                           uiOutput("northingCols"),
                                                                           uiOutput("ESPGCols"),
                                                                           actionButton("raw.coord.cols", "Finalize Coordinates"),
                                                                           actionButton("raw.coord.cols.rem", "Undo"),
                                                                           hr(),
                                                                           verbatimTextOutput("view.coord.cols")
                                                                       )
                                                      )
                                               ),
                                               column(width=4,
                                                      conditionalPanel("input.rawFormat == 'Wide'||input.rawFormat == 'Long'",
                                                                       box(title="Date Field (Optional)",width=NULL,status="success",collapsible = T,solidHeader = T,collapsed = F,
                                                                           actionLink("Date_field.help","?"),
                                                                           uiOutput("time_ID")
                                                                       )
                                                                       
                                                      )
                                               )
                                             )
                                             
                                    ),
                                    tabPanel("Taxa",
                                             useShinyjs(),
                                             conditionalPanel(condition="input.metdata == false && input.finalize_raw>0 && output.show_mets == true",
                                                              DT::dataTableOutput("view.taxa"),
                                                              br(),
                                                              br(),
                                                              fluidRow(
                                                                column(width=4,
                                                                       box(title="Calculate Summary Metrics",width=12,status="primary",collapsible = T,solidHeader = T,collapsed = F,
                                                                           actionButton("calculate_metrics","Calculate Summary Metrics"),
                                                                           br(),
                                                                           conditionalPanel(condition="input.metdata == false",
                                                                                            downloadButton("download_raw_taxa","Download Taxa Table")
                                                                           ),
                                                                           conditionalPanel(condition="input.metdata == false && input.calculate_metrics>0 && output.show_mets2 == true",
                                                                                            downloadButton("download_raw_mets","Download Summary Metrics"),
                                                                                            downloadButton("download_taxa_atts","Download Taxa Attributes")
                                                                           )
                                                                       )
                                                                )
                                                              )
                                             ),
                                             conditionalPanel(condition="output.show_mets == true",
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
                                             hr(),
                                             fluidRow(
                                               box(title="Factor variables",width=5,status="success",collapsible = F,solidHeader = T,collapsed = F,
                                                   uiOutput("dispaly_habitat_factors"),
                                                   br(),
                                                   actionButton("habitat_convert_fact_to_numb","Convert to numeric")
                                               ),
                                               box(title="Numeric variables",width=5,status="success",collapsible = F,solidHeader = T,collapsed = F,
                                                   uiOutput("dispaly_habitat_numeric"),
                                                   br(),
                                                   actionButton("habitat_convert_numb_to_fact","Convert to factor")
                                               )
                                             )
                                    ),
                                    tabPanel("Coordinates",
                                             DT::dataTableOutput("view.coords"),
                                             br(),
                                             br()
                                    )
                                    
                             )
                           )
          ),
          
          ##################################################
          # Metric Transformations
          ##################################################
          
          
          conditionalPanel("input.sidebarmenu === 'transSummaryMetrics'",
                           tabBox("Transformations",width=12,
                                  tabPanel("Single Metrics",
                                           fluidRow(
                                             column(width=3,
                                                    box(title="Transformations",width=12,
                                                        uiOutput("mets_for_trans_out"),
                                                        uiOutput("out_trans_selected"),
                                                        actionButton("apply.trans",label="Apply Selection")
                                                    )
                                             ),
                                             box(title="", width=3,
                                                 tableOutput("applied_transformations")
                                             ),
                                             box(title="Plots",width=6,
                                                 plotOutput("met.trans.plot1"),
                                                 plotOutput("met.trans.plot2")
                                             )
                                           )
                                  ),
                                  tabPanel("Batch",
                                           fluidRow(
                                             column(width=3,
                                                    box(title="Batch Transformations",width=12,
                                                        helpText("Select Multiple metrics by keywords (i.e. Richness, Percent, per, as, etc)"),
                                                        textInput("trans_keyword", label = "", value = ""),
                                                        verbatimTextOutput("batch_met_sel"),
                                                        hr(),
                                                        radioButtons("trans.batch", label = "",choices = list("None" = "None", "Log10" = "Log10", "Log10+1" = "Log10+1", "Square Root" = "Square Root", "Inverse" = "Inverse", "Arcsine Sqare Root"= "Arcsine Sqare Root", "Logit" = "Logit", "Delete"="Delete"), 
                                                                     selected = "None"
                                                        ),
                                                        actionButton("apply.trans.batch",label="Apply Selection")
                                                    )
                                             ),
                                             box(title="", width=3,
                                                 tableOutput("applied_transformations.batch")
                                             )
                                           )
                                  ),
                                  tabPanel("Transformed Metrics",
                                           DT::dataTableOutput("view.transformed.metrics")
                                  )
                           )
          ),
          

          ##################################################
          # User Matched Reference Sites
          ##################################################
          
          
          conditionalPanel("input.sidebarmenu === 'userMatchRef'",
                           h5("User Matched Reference Sites"),
                           verbatimTextOutput("testout1"),
                           verbatimTextOutput("testout3"),
                           DT::dataTableOutput("testout2")
          ),
          
          ##################################################
          # Mapping
          ##################################################
          
          
          conditionalPanel("input.sidebarmenu === 'Mapping'",
                           leafletOutput("mymap",height = 800),
                           
                           absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                                         draggable = F, top = 80, left = "auto", right = 40, bottom = "auto",
                                         width = 250, height = "auto",
                                         box(title=NULL,width=12,background= "olive",
                                             h3("Controls"),
                                             radioButtons("basemap_input",label="Basemap",choices=c("Street","Satellite")),
                                             checkboxInput("map_admin",label="Administrative Boundaries"),
                                             hr(),
                                             selectInput("map_pointtype", "Map symbols",choices=c("Points"="Points","Pie Chart"="pie", "Bar Chart"="bar")),
                                             hr(),
                                             conditionalPanel("input.map_pointtype == 'pie' || input.map_pointtype == 'bar'",
                                                              sliderInput("map_chart_site","Chart Size", min=1,max=100,value=45),
                                                              uiOutput("out.map_chart_variables"),
                                                              hr()
                                                              
                                             ),
                                             conditionalPanel("input.map_pointtype == 'Points'",
                                                              selectInput("map_pointcolgroup", "Colour",choices=c("None","Habitat","Taxa","Metrics","Impairment")),
                                                              uiOutput("map_pointcolselect_out")
                                             )
                                         )
                           )
                           
          )

          ##################################################
          # End of Body
          ##################################################
  )
)





dashboardPage(
  header,
  sidebar,
  body
)



