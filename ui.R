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
  title="EcoPulse",
  #title = "Benthic Analysis Online Tool - Development Version",
  #titleWidth = 550,
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
  conditionalPanel(condition="output.loggedin1", 
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
                                        menuSubItem("User Matched Reference Sites",tabName = "userMatchRef",icon=NULL),
                                        conditionalPanel("input.sidebarmenu === 'userMatchRef'",
                                                         useShinyjs(),
                                                         fileInput(inputId="inUserMatchRefFile",multiple = FALSE, label = h5("Choose CSV file"),accept=c(".csv",".txt"))
                                                         )
                               ),
                               menuItem("Mapping",tabName="Mapping", icon=icon("map"),
                                        menuSubItem("Options",tabName = "map_options",icon=NULL),
                                        conditionalPanel("input.sidebarmenu === 'map_options'",
                                                         selectInput("map_pointtype", "Map symbols",choices=c("Points"="Points"#,"Pie"="Pie"
                                                                                                              )),
                                                         conditionalPanel("input.map_pointtype ='Points'",
                                                                          selectInput("map_pointcol","Colours", 
                                                                                      choices=list(
                                                                                        Sequential=c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
                                                                                                     "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu" ,"Reds" ,"YlGn" ,
                                                                                                     "YlGnBu", "YlOrBr" ,"YlOrRd"),
                                                                                        Diverging=c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                                                                          )
                                                         ),
                                                         uiOutput("out.map_chart_variables"),
                                                         checkboxInput("map_legend", "Show Legend", value=F),
                                                         uiOutput("out.map_time_variables")
                                        )
                               ),
                               menuItem("Data Summaries",tabname="DataExploration",icon=icon("eye", lib = "font-awesome"),
                                        menuSubItem("Options",tabName = "explorationSetup",icon=NULL)
                               ),
                               menuItem("Integrity Assessment",tabName="RCA_main", icon=icon("gears"),
                                        menuSubItem("Single Site",tabName="RCA_sub",icon=NULL),
                                        conditionalPanel("input.sidebarmenu === 'RCA_sub'",
                                                         uiOutput("out_test.site.select")
                                        ),
                                        menuSubItem("Multiple Sites",tabName="RCA_sub_batch",icon=NULL)
                                        
                               )#,
                               #menuItem("Trends",tabname="Trends",icon=icon('line-chart'),
                               #         menuSubItem("In Development",tabName = "TrendsSetup",icon=NULL)
                               #)#,
                               #menuItem("Generate Reports",tabname="Reports",icon=icon('file-text'),
                               #         menuSubItem("In Development",tabName = "ReportsSetup",icon=NULL)
                               #)
                               
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
                                  box(title=h2("Online tool for Aquatic Ecosystem Assessments"),width=12,status="info",collapsible = F, collapsed = F,solidHeader = F,
                                      h5("Early Release Version"),
                                      hr(),
                                      helpText("This website provides tools to assist in making assessments on aquatic ecosystems."),
                                      helpText("This package provides functionallity for:"),
                                      helpText("1) calculation of indicator benthic macroinvertebrate indicator metrics from taxon count data"),
                                      helpText("  - Users can supply additional metrics such as: fisheries, diatoms, algae, etc."),
                                      helpText("2) visualizing spatial patterns in taxa and indicator metrics through built-in GIS functionallity (still limited)"),
                                      helpText("3) An implimentation of a Reference Condition Approach for making assessments of aquatic ecosystem integrity. The following tools are available:"),
                                      helpText("   - Nearest-Neighbour Site Matching between 'test' and 'reference' sites (ANNA and RDA-ANNA) using user supplied habitat descriptors"),
                                      helpText("   - user-supplied matrix of matched 'test' and 'reference' sites"),
                                      helpText("   - Test Site Analysis (TSA) to evaluate whether biological communities at test sites are significantly different from variation observed in nearest-neighbour reference sites"),
                                      helpText("   - A variety of diagnostic plots and tools for assessing confidence of impairment ranks"),
                                      hr(),
                                      actionButton("getting_started", "Getting Started")
                                  )
                           ),
                           column(width=4,offset=1,
                                  box(title="Implemented features",width=12,status="info",collapsible = T, collapsed = F,solidHeader = T,
                                      helpText("- Single input file for the follow data types: taxa or metrics, habitat descriptors, coordinates of sampling point"),
                                      helpText("- Support for long and wide data formats (long format recommended for lowest-practical-level taxonomy)"),
                                      helpText("- Support for wide data formats with multiple column headers"),
                                      helpText("- Calculation of indicator metrics for Benthic Macroinvertebrate taxa"),
                                      helpText("- HBI will be calculated at family level only - for now"),
                                      helpText("- Functional metrics can be calculated below family level"),
                                      helpText("- Download calculated Summary metrics as .csv file"),
                                      helpText("- Download calculated Taxa Attribute Data as .csv file"),
                                      helpText("- Transformations for metrics"),
                                      helpText("- GIS mapping of sampling locations coloured by taxa, indicator metrics or habitat variables"),
                                      helpText("- Reference Condition Approach Integrity assessment by Test Site Analysis"),
                                      helpText("- Matching of Test and Reference sites by nearest-neighbour methods (ANNA and RDA-ANNA)"),
                                      helpText("- Use of user supplied reference site matches for Test Site Analysis")
                                  ),
                                  box(title="Planned features",width=12,status="info",collapsible = T, collapsed = T,solidHeader = T,
                                      helpText("- Download individual figures and automated generation of summary reports"),
                                      helpText("- Better support for lowest-practical-level taxonomy"),
                                      helpText("- Built-in support for fish community data"),
                                      helpText("- Better GIS intigration for mapping of test and reference sites"),
                                      helpText("- GIS intigration for calculation of catchment and site level attributes"),
                                      helpText("- Tools for better data exploration"),
                                      helpText("- Trend analysis using Generalized Linear Mixed-Effects models"),
                                      helpText("- Users can save and load datasets though the tool")
                                  ),
                                  box(title="Known Bugs",width=12,status="info",collapsible = T, collapsed = T,solidHeader = T,
                                      helpText("- none, but some feature states have not been fully tested"),
                                      helpText("If you encounter any bugs or crashes, email: ecopulseanalytics@gmail.com")
                                      
                                  )#,
                                  #actionButton("savestate","Save state"),
                                  #actionButton("loadstate","Load state")
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
                                             fluidPage(
                                               DT::dataTableOutput("rawDataView"),
                                               br(),
                                               br()
                                             ),
                                             fluidRow(
                                               conditionalPanel("output.show_rawdatabox",
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
                                                                                                   conditionalPanel("input.rawFormat == 'Long'",actionButton("raw.abund.cols", "Abundances")),
                                                                                                   br(),
                                                                                                   actionButton("raw.habitat.cols", "Habitat Descriptors (Optional)")
                                                                                            ),
                                                                                            column(width=4,
                                                                                                   actionButton("raw.siteID.cols.rem", "Undo"),
                                                                                                   br(),
                                                                                                   actionButton("raw.taxa.cols.rem", "Undo"),
                                                                                                   br(),
                                                                                                   conditionalPanel("input.rawFormat == 'Long'",actionButton("raw.abund.cols.rem", "Undo")),
                                                                                                   br(),
                                                                                                   actionButton("raw.habitat.cols.rem", "Undo")
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
                                             )
                                             
                                    ),
                                    tabPanel("Taxa",
                                             useShinyjs(),
                                             conditionalPanel(condition="input.finalize_raw>0 && output.show_mets == true",
                                                              conditionalPanel("input.metdata==false",
                                                                               box(title="Taxa",width=12,status="primary",collapsible = T,solidHeader = T,collapsed = F,
                                                                                   DT::dataTableOutput("view.taxa")
                                                                               )
                                                              ),
                                                              box(title="Summary Metrics",width=12,status="primary",collapsible = T,solidHeader = T,collapsed = F,
                                                                  DT::dataTableOutput("view.metrics.raw")
                                                              ),
                                                              br(),
                                                              fluidRow(
                                                                column(width=3,
                                                                       conditionalPanel(condition="input.metdata == false",
                                                                                        box(title="Download Data",width=12,status="primary",collapsible = T,solidHeader = T,collapsed = F,
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
                           DT::dataTableOutput("out_usermatch.refsites")
          ),
          
          ##################################################
          # Mapping
          ##################################################
          conditionalPanel("input.sidebarmenu === 'map_options'",
                           leafletOutput("mymap",height = 800)
                           #leafletOutput("mymap",height = 800),
                           
                           #absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                          #               draggable = F, top = 80, left = "auto", right = 40, bottom = "auto",
                          #               width = 250, height = "auto",
                          #               box(title=NULL,width=12,background= "olive",
                          #                   h3("Controls"),
                          #                   radioButtons("basemap_input",label="Basemap",choices=c("Street","Satellite")),
                          #                   checkboxInput("map_admin",label="Administrative Boundaries"),
                          #                   hr(),
                          #                   selectInput("map_pointtype", "Map symbols",choices=c("Points"="Points","Pie Chart"="pie", "Bar Chart"="bar")),
                          #                   hr(),
                          #                   conditionalPanel("input.map_pointtype == 'pie' || input.map_pointtype == 'bar'",
                          #                                    sliderInput("map_chart_site","Chart Size", min=1,max=100,value=45),
                          #                                    uiOutput("out.map_chart_variables"),
                          #                                    hr()
                          #                                    
                          #                   ),
                          #                   conditionalPanel("input.map_pointtype == 'Points'",
                          #                                    selectInput("map_pointcolgroup", "Colour",choices=c("None","Habitat","Taxa","Metrics","Impairment")),
                          #                                    uiOutput("map_pointcolselect_out")
                          #                   )
                          #               )
                          # )
                           
          ),
          ##################################################
          # Data summaries
          ##################################################
          conditionalPanel("input.sidebarmenu === 'explorationSetup'",
                           fluidRow(
                             tabBox(width=9,
                                    tabPanel(h4("Table"),
                                             fluidPage(
                                               column(width=4,
                                                      uiOutput("datsum_tabresponse"),
                                                      uiOutput("datsum_tabgrpfct1"),
                                                      uiOutput("datsum_tabgrpfct2"),
                                                      uiOutput("datsum_tabgrpfct3"),
                                                      selectInput("datasum_fun","Function", list(General=c("Sum","Mean"),
                                                                                                 Percentiles=c("5th","25th","50th","75th","95th")),
                                                                  multiple=F,selected="Sum"),
                                                      checkboxInput("datsum_tab_usetrans","Use transformed data"),
                                                      downloadButton("download_datasum_table","Download")
                                               ),
                                               column(width=6,
                                                      dataTableOutput("datsumtable"))
                                             )
                                    ),
                                    tabPanel(h4("Correlations")
                                    ),
                                    tabPanel(h4("Scatter Plots")
                                    ),
                                    tabPanel(h4("Box Plots")
                                    ),
                                    tabPanel(h4("Pie Charts")
                                    ),
                                    tabPanel(h4("Exploratory models")
                                    )
                             )
                           )
          ),
          
          ##################################################
          # RCA - Single
          ##################################################
          conditionalPanel("input.sidebarmenu === 'RCA_sub'",
                           fluidRow(
                             tabBox("RCA",width=12,
                                    tabPanel(h4("Nearest-Neighbour Site Matching"),
                                             fluidRow(
                                               column(width=8,
                                                      fluidRow(
                                                        conditionalPanel("input.nn_method!='User Selected'",
                                                                         column(width=8,
                                                                                plotOutput("nn.ord", dblclick = "nn.ord_dblclick",
                                                                                           brush = brushOpts(
                                                                                             id = "nn.ord_brush",
                                                                                             resetOnNew = TRUE
                                                                                           )),
                                                                                downloadButton("nn.ord.plot.download","Download plot"),
                                                                                helpText("Drag a box and double-click to zoom to that area. Double-click again to zoom out"),
                                                                                actionLink("NN_results_modal","Nearest-Neighbour Model Results") #Need to add this
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
                                                        ),
                                                        conditionalPanel("input.nn_method=='User Selected'",
                                                                         DT::dataTableOutput("out_usermatch.refsites1")
                                                                         )
                                                      )
                                               ),
                                               box(title="Metric Selection",width=4,
                                                   column(width=12,
                                                          conditionalPanel("input.nn_method!='RDA-ANNA'",
                                                          checkboxInput("useMD","Maximal-Distance Metric Selection", value=T)
                                                          )
                                                          ,
                                                          uiOutput("out_metric.select")
                                                   )
                                                   
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               box(title="Nearest-Neighbour Options",width=5,
                                                      column(width=6,
                                                             #uiOutput("out_nn_method"),
                                                             selectInput("nn_method","NN Method", multiple=F,selectize=F,
                                                                         choices=c("ANNA","RDA-ANNA","User Selected")),
                                                             conditionalPanel("input.nn_method!='User Selected'",
                                                                              numericInput("nn.k","Number of Reference Sites", value = 0,min=0,step=1),
                                                                              checkboxInput("nn.scale","Scale Ordination",value=T)
                                                                              )
                                                      ),
                                                      column(width=6,
                                                             conditionalPanel("input.nn_method!='User Selected'",
                                                                              box(title="Distance-Decay Site Selection",width=12,
                                                                                  checkboxInput("nn_useDD","Use Distance-Decay Site Selection", value=T),
                                                                                  numericInput("nn.factor","Distance Decay factor", value = 2),
                                                                                  numericInput("nn.constant","Distance Decay constant", value = 1)
                                                                              )
                                                             )
                                                      )
                                                      
                                               ),
                                               conditionalPanel("input.nn_method!='User Selected'",
                                                                box(title="Habitat Distances",width=4,
                                                                    plotOutput("nn.dist")
                                                                )
                                                                
                                               ),
                                               box(title="TSA Options", width=3,
                                                   conditionalPanel("input.nn_method!='User Selected'",
                                                                    checkboxInput("tsa_weighted","Weight Reference sites by habitat distance?",value=F)
                                                   ),
                                                   checkboxInput("tsa_outlier_rem","Remove potential outlier reference sites?",value=F),
                                                   conditionalPanel("input.tsa_outlier_rem==true",
                                                                    sliderInput("tsa_outbound", "Outlier Coefficient", min=0.01,max=0.99,value=0.1)
                                                   )
                                               )
                                             )
                                    ),
                                    tabPanel(h4("Test Site Analysis"),
                                             fluidPage(
                                               fluidRow(
                                                 column(width=5,
                                                          uiOutput("tsa.result.printed"),
                                                          actionLink("TSA_results_modal","TSA Model Results") #Need to add this

                                                 ),
                                                 column(width=4,
                                                        tabsetPanel(
                                                          tabPanel(title=("Circle Plot"),plotOutput("tsa.circle.plot"),downloadButton("tsa.circle.plot.download","Download plot")),
                                                          tabPanel(title=("TSA Distance"),plotOutput("tsa.distance.plot"),downloadButton("tsa.distance.plot.download","Download plot") ),
                                                          tabPanel(title=("Correspondence Analysis"),plotOutput("tsa.ca.plot"),downloadButton("tsa.ca.plot.download","Download plot") ),
                                                          tabPanel(title=("PCoA"),plotOutput("tsa.pcoa.plot"),downloadButton("tsa.pcoa.download","Download plot") )
                                                        )
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width=10,plotOutput("tsa.metric.plot"),downloadButton("tsa.metric.plot.download","Download plot"))
                                               )
                                             )
                                    )
                                    
                             )
                             
                           )
          ),
          ##################################################
          # RCA - Batch
          ##################################################
          
          conditionalPanel("input.sidebarmenu === 'RCA_sub_batch'",
                           fluidRow(
                             h3("Integrity Assessment by Reference Condition Approach"),
                             tabBox("RCA",width=12,
                                    tabPanel(h4("Batch Setup"),
                                             fluidRow(
                                               box(title="Nearest-Neighbour Options",width=4,
                                                   column(width=6,
                                                          selectInput("nn_method_b","NN Method", multiple=F,selectize=F,
                                                                      choices=c("ANNA","RDA-ANNA","User Selected")),
                                                          conditionalPanel("input.nn_method_b!='User Selected'",
                                                                           numericInput("nn.k_b","Number of Reference Sites", value = 0,min=0,step=1),
                                                                           checkboxInput("nn.scale_b","Scale Ordination",value=T)
                                                          )
                                                   ),
                                                   column(width=6,
                                                          conditionalPanel("input.nn_method_b!='User Selected'",
                                                                           box(title="Distance-Decay Site Selection",width=12,
                                                                               checkboxInput("nn_useDD_b","Use Distance-Decay Site Selection", value=T),
                                                                               numericInput("nn.factor_b","Distance Decay factor", value = 2),
                                                                               numericInput("nn.constant_b","Distance Decay constant", value = 1)
                                                                           )
                                                          )
                                                   )
                                               ),
                                               box(title="Metric Selection",width=4,
                                                   column(width=12,
                                                          conditionalPanel("input.nn_method_b!='RDA-ANNA'",
                                                          checkboxInput("useMD_b","Maximal-Distance Metric Selection", value=T)
                                                          ),
                                                          uiOutput("out_metric.select_b")
                                                   )
                                                   
                                               ),
                                               box(title="TSA Options", width=3,
                                                   checkboxInput("tsa_weighted_b","Weight Reference sites by habitat distance?",value=F),
                                                   checkboxInput("tsa_outlier_rem_b","Remove potential outlier reference sites?",value=F),
                                                   conditionalPanel("input.tsa_outlier_rem_b==true",
                                                                    sliderInput("tsa_outbound_b", "Outlier Coefficient", min=0.01,max=0.99,value=0.1)
                                                   )
                                               )
                                             ),
                                             hr(),
                                             actionButton("tsa_batch_go", "Start"),
                                             helpText("This operation may take a long time. See Progress bar in lower right...")
                                    ),
                                    tabPanel(h4("Results"),
                                             fluidRow(
                                               box(title="Summary",
                                                   uiOutput("out_tsa_bulk_table_sumby"),
                                                   dataTableOutput("tsa_bulk_table"),
                                                   downloadButton("download_tsa_batch","Download all results")
                                               )
                                             ),
                                             fluidRow(
                                               box(title="Test Site Selection", width=2,
                                                   uiOutput("out_batch_test_result_select")),
                                               box(title="Overview",width=10,#uiOutput("batch_summary_report")
                                                   fluidRow(
                                                     fluidRow(
                                                       column(width=5,
                                                              uiOutput("tsa.result.printed_b"),
                                                              actionLink("TSA_results_modal_b","TSA Model Results") #Need to add this
                                                              
                                                       ),
                                                       column(width=4,
                                                              plotOutput("tsa.circle.plot_b")
                                                       )
                                                     ),
                                                     column(width=10,plotOutput("tsa.metric.plot_b"))
                                                     
                                                   ),
                                                   fluidRow(
                                                     column(width=6,
                                                            plotOutput("nn.ord_b"),
                                                            actionLink("NN_results_modal_b","Nearest-Neighbour Model Results") #Need to add this
                                                            
                                                     ),
                                                     column(width=4,
                                                            plotOutput("nn.dist_b")
                                                     )
                                                   )
                                               )
                                             )
                                    )
                             )
                           )
          ),
          
          ##################################################
          #Generate Reports
          ##################################################
          conditionalPanel("output.adminloggedin", 
                           conditionalPanel("input.sidebarmenu === 'ReportsSetup'",
                                            fluidRow(
                                              h3("Generate Summary Reports")
                                            ),
                                            fluidRow(
                                              box(title="",width=6,
                                                  radioButtons("report_orientation","Page Orientation", choices=list(Portrait="Portrait",Landscape="Landscape"),inline=T,width=600),
                                                  uiOutput("report_test"),
                                                  actionButton("apply_report_item","Apply")
                                                  #verbatimTextOutput("plot_hoverinfo")
                                              ),
                                              conditionalPanel(condition="output.populate_report_element",
                                                               box(title="Report Element", width=6,
                                                                   uiOutput("out.report_element_number"),
                                                                   uiOutput("out.report_element_type")
                                                               )
                                              )
                                            )
                           )
          )
          
          ##################################################
          # End of Body
          ##################################################
          #h5("User Matched Reference Sites"),
          #helpText("This feature is still in Development"),
          #verbatimTextOutput("testout1"),
          #verbatimTextOutput("testout3"),
          #DT::dataTableOutput("testout2")
          
  )
)

dashboardPage(
  header,
  sidebar,
  body
)



