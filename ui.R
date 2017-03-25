##################################################
# Header
##################################################
header <- dashboardHeader(
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
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
    menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")
    ),
    menuItem("Data Input", icon = icon("th"), tabName = "datainput",
             menuItem("Raw Data upload",tabName = "rawdatainput",icon=NULL),
            conditionalPanel("input.sidebarmenu === 'rawdatainput'",
                             fileInput(inputId="inrawbioFile",multiple = FALSE, label = h5("Choose CSV file"),accept=c(".csv",".txt")),
                             checkboxInput("metdata",label=h5("Input data are metrics"),value=F),
                             radioButtons(inputId="rawFormat", label = h5("Input File Format"),choices = list("Long" ,"Wide" ),inline=T, selected = "")
                             
            ),
            conditionalPanel("output.finalizeRaw==true && input.sidebarmenu === 'rawdatainput'",
                             actionButton("finalize.raw","Finalize")
                             )
    )
  )
)

##################################################
# Body
##################################################
body <- dashboardBody(
  tabItems(
    ##################################################
    # Intruduction Page
    ##################################################
    
    tabItem(tabName = "introduction",
            h2("Introduction"),
            fluidRow(
              helpText("Hold")
            )
    ),
    ##################################################
    # Raw Data Input
    ##################################################
    
    tabItem(tabName = "rawdatainput",
            h2("Raw Data Input"),
            fluidRow(
              tabBox("Data",width=12,
                tabPanel("Raw",status="warning",collapsible = T,solidHeader = T,
                         DT::dataTableOutput("rawDataView")
                         ),
                tabPanel("Taxa",
                         DT::dataTableOutput("view.taxa")
                         ),
                tabPanel("Habitat")
              )
            ),
            fluidRow(
              column(width=6,
                box(title="Columns",width=12,status="primary",collapsible = T,solidHeader = T,
                    conditionalPanel(condition="input.rawFormat == 'Wide'",
                                     numericInput("rawData_taxarows", label = h5("Rows of column Identifiers"), min=1,value=1)
                    ),
                    conditionalPanel("input.rawFormat == 'Wide' && input.rawData_taxarows == 1 ",
                                     textInput("text", label = h5("Character that separates taxa names"), value = ";")
                    ),
                    uiOutput("wideTaxaCols1")
                )
                
              ),
              column(width=6,
                box(title="Assign Columns", width=NULL,status="success",collapsible = T,solidHeader = T,
                    conditionalPanel("input.rawFormat == 'Wide'||input.rawFormat == 'Long'",
                                     column(width=6,
                                            actionButton("raw.siteID.cols", "Site/Sampling Events"),
                                            br(),
                                            actionButton("raw.taxa.cols", "Taxa or Metrics"),
                                            br(),
                                            actionButton("raw.habitat.cols", "Habitat Descriptors"),
                                            br(),
                                            conditionalPanel("input.rawFormat == 'Long'",actionButton("raw.abund.cols", "Values"))
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
                                 box(title="Test vs. Reference",width=NULL,status="success",collapsible = T,solidHeader = T,collapsed = T,
                                     uiOutput("test.vs.ref"),
                                     actionButton("raw.testref.cols", "Test and Reference Sites"),
                                     actionButton("raw.testref.cols.rem", "Undo")
                                     ),
                                 box(title="Coordinates",width=NULL,status="success",collapsible = T,solidHeader = T,collapsed = T,
                                     uiOutput("eastingCols"),
                                     uiOutput("northingCols"),
                                     uiOutput("ESPGCols"),
                                     actionButton("raw.coord.cols", "Coordinates"),
                                     actionButton("raw.coord.cols.rem", "Undo")
                                     )
                )
              )
              )
            )
    )
  )





dashboardPage(
  header,
  sidebar,
  body
)



