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
              box(width=12,
                  DT::dataTableOutput("rawDataView")
              )
            ),
            fluidRow(
              box(title="Taxa/Metrics",width=3,
                  conditionalPanel(condition="input.rawFormat == 'Wide'",
                                   numericInput("rawData_taxarows", label = h5("Rows of Taxa Identifiers"), min=1,value=1),
                                   conditionalPanel("input.rawData_taxarows >1 ",
                                                    uiOutput("wideTaxaCols1")
                                                    ),
                                   conditionalPanel("input.rawData_taxarows == 1 ",
                                                    uiOutput("wideTaxaCols2"),
                                                    textInput("text", label = h5("Character that separates taxa names"), value = ";")
                                                    )
                                   )
                  ),
              conditionalPanel(condition="input.rawFormat == 'Long'",
                               box(width=3,
                                   h2("Taxa/Metric Columns - wide"),
                                   h2("Taxa/Metric Rows - wide"))
              ),
              
              box(title="Sites/Sampling Events",width=3,
                  uiOutput("wideSiteIDCols")
                  ),
              box(title="Habitat Descriptors",width=3,
                  uiOutput("habitatCols")
                  ),
              box(title="Coordinates (optional)",width=3,
                  uiOutput("eastingCols"),
                  uiOutput("northingCols"),
                  uiOutput("EPSGCols")
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



