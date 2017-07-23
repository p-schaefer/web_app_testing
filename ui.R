library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(leaflet)
library(ggplot2)
library(leaflet.minicharts)
#library(biotools)
#library(reshape2)
#library(rpanel)
#library(tcltk)
#library(vegan)


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
              menuItem("Introduction", tabName = "introduction", icon = icon("list-alt")#,
                       #menuSubItem("Details",tabName = "Details")
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
              menuItem("Mapping",tabName="Mapping", icon=icon("map")
              ),
              menuItem("Data Exploration",tabname="DataExploration",icon=icon("tint", lib = "glyphicon"),
                       menuSubItem("Setup",tabName = "explorationSetup",icon=NULL)
              ),
              menuItem("Integrity Assessment",tabName="RCA_main", icon=icon("gears"),
                       menuSubItem("NN and TSA",tabName="RCA_sub",icon=NULL),
                       conditionalPanel("input.sidebarmenu === 'RCA_sub'",
                                        uiOutput("out_test.site.select")
                       )
                       
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
          
          conditionalPanel("input.sidebarmenu === 'introduction'",
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
                                                                       box(title="Date/Time Field (Optional)",width=NULL,status="success",collapsible = T,solidHeader = T,collapsed = T,
                                                                           actionLink("Date_field.help",h4("?")),
                                                                           uiOutput("time_ID")
                                                                       ),
                                                                       box(title="Test vs. Reference (Optional)",width=NULL,status="success",collapsible = T,solidHeader = T,collapsed = T,
                                                                           uiOutput("test.vs.ref")#,
                                                                           #actionButton("raw.testref.cols", "Finalize Test and Reference Sites"),
                                                                           #actionButton("raw.testref.cols.rem", "Undo")
                                                                       ),
                                                                       box(title="Coordinates (Optional)",width=NULL,status="success",collapsible = T,solidHeader = T,collapsed = T,
                                                                           uiOutput("eastingCols"),
                                                                           uiOutput("northingCols"),
                                                                           uiOutput("ESPGCols")#,
                                                                           #actionButton("raw.coord.cols", "Finalize Coordinates"),
                                                                           #actionButton("raw.coord.cols.rem", "Undo"),
                                                                           #hr(),
                                                                           #verbatimTextOutput("view.coord.cols")
                                                                       )
                                                      )
                                               )
                                             )
                                             
                                    ),
                                    tabPanel("Taxa",
                                             useShinyjs(),
                                             conditionalPanel(condition="input.metdata == false && input.finalize_raw>0 && output.show_mets == true",
                                                              box(title="Taxa",width=12,status="primary",collapsible = T,solidHeader = T,collapsed = F,
                                                                  DT::dataTableOutput("view.taxa")
                                                              ),
                                                              box(title="Summary Metrics",width=12,status="primary",collapsible = T,solidHeader = T,collapsed = F,
                                                                  DT::dataTableOutput("view.metrics.raw")
                                                              ),
                                                              br(),
                                                              fluidRow(
                                                                column(width=3,
                                                                       box(title="Download Data",width=12,status="primary",collapsible = T,solidHeader = T,collapsed = F,
                                                                           conditionalPanel(condition="input.metdata == false",
                                                                                            downloadButton("download_raw_taxa","Download Taxa Table"),
                                                                                            downloadButton("download_raw_mets","Download Summary Metrics"),
                                                                                            downloadButton("download_taxa_atts","Download Taxa Attributes")
                                                                           )
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
                           fluidRow(
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
                             
                           )
          ),
          
          
          ##################################################
          # User Matched Reference Sites
          ##################################################
          
          
          conditionalPanel("input.sidebarmenu === 'userMatchRef'",
                           h5("User Matched Reference Sites"),
                           helpText("This feature is still in Development"),
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
                           
          ),

          ##################################################
          # RCA
          ##################################################
          conditionalPanel("input.sidebarmenu === 'RCA_sub'",
                           fluidRow(
                             h3("Integrity Assessment by Reference Condition Approach"),
                             tabBox("RCA",width=12,
                                    tabPanel(h4("Nearest-Neighbour Site Matching"),
                                             fluidRow(
                                               column(width=8,
                                                      fluidRow(
                                                        column(width=8,
                                                               plotOutput("nn.ord", dblclick = "nn.ord_dblclick",
                                                                          brush = brushOpts(
                                                                            id = "nn.ord_brush",
                                                                            resetOnNew = TRUE
                                                                          )),
                                                               helpText("Drag a box and double-click to zoom to that area. Double-click again to zoom out")
                                                        ),
                                                        column(width=4,
                                                               box(width=12,
                                                                   uiOutput("out_nn.axis1"),
                                                                   uiOutput("out_nn.axis2"),
                                                                   checkboxInput("nnplot.hab.points","Show Habitat variables"),
                                                                   checkboxInput("nnplot.hab.names","Show Habitat variables"),
                                                                   conditionalPanel("input.nn_method=='RDA-ANNA'",
                                                                                    checkboxInput("nnplot.metric","Show Metric Vectors")
                                                                   )
                                                               ),
                                                               hr(),
                                                               box(width=12,
                                                                   conditionalPanel("input.in_test_site_select!='None'",
                                                                                    checkboxInput("nnplot.hull","Show Convex Hull", value=T),
                                                                                    checkboxInput("nnplot.refnames","Show Reference Site Names", value=T),
                                                                                    checkboxInput("nnplot.testsite","Show Test Site", value=T)
                                                                   )
                                                               )
                                                        )
                                                      )
                                               ),
                                               column(width=4,
                                                      box(title="Metrics",width=12,
                                                          #conditionalPanel("input.nn_method=='ANNA'",
                                                                           checkboxInput("useMD","Maximal-Distance Metric Selection", value=T)
                                                          #)
                                                          ,
                                                          uiOutput("out_metric.select")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(width=9,
                                                      column(width=3,
                                                             selectInput("nn_method","NN Method", multiple=F,selectize=F,
                                                                         choices=c("ANNA","RDA-ANNA","User Selected")),
                                                             numericInput("nn.k","Number of Reference Sites", value = 0,min=0,step=1),
                                                             checkboxInput("nn.scale","Scale Ordination",value=T)
                                                      ),
                                                      column(width=3,
                                                             conditionalPanel("input.nn_method!='User Selected'",
                                                                              box(title="Distance-Decay Site Selection",width=12,
                                                                                  checkboxInput("nn_useDD","Use Distance-Decay Site Selection", value=T),
                                                                                  numericInput("nn.factor","Distance Decay factor", value = 2),
                                                                                  numericInput("nn.constant","Distance Decay constant", value = 1)
                                                                              )
                                                             )
                                                      ),
                                                      column(width=6,
                                                             plotOutput("nn.dist")
                                                      )
                                               ),
                                               box(title="TSA Options", width=3,
                                                   checkboxInput("tsa_weighted","Weight Reference sites by habitat distance?",value=F),
                                                   checkboxInput("tsa_outlier_rem","Remove optential outlier reference sites?",value=F),
                                                   conditionalPanel("input.tsa_outlier_rem==true",
                                                                    sliderInput("tsa_outbound", "Outlier Coefficient", min=0.01,max=0.99,value=0.1)
                                                   )
                                               )
                                               
                                             )
                                    ),
                                    tabPanel(h4("Test Site Analysis"),
                                             fluidRow(
                                               fluidRow(
                                                 column(width=6,
                                                        fluidRow(
                                                          uiOutput("tsa.result.printed")
                                                        )
                                                        
                                                 ),
                                                 column(width=4,
                                                        tabsetPanel(
                                                          tabPanel(title=("Circle Plot"),plotOutput("tsa.circle.plot") ),
                                                          tabPanel(title=("TSA Distance"),plotOutput("tsa.distance.plot") ),
                                                          tabPanel(title=("Correspondence Analysis"),plotOutput("tsa.ca.plot") ),
                                                          tabPanel(title=("Metric Dimensional Scaling"),plotOutput("tsa.pcoa.plot") )
                                                        )
                                                 )
                                               ),
                                               column(width=10,plotOutput("tsa.metric.plot"))
                                               
                                             )
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



