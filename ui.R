
shinyUI(fluidPage(
  navbarPage("Benthic Analysis",
             navbarMenu("Data Input",
                        #########################################################
                        #Input biological Data
                        ########################################################
                        tabPanel("Biological Data",
                                 sidebarLayout(
                                   sidebarPanel(
                                     h3("Biological Data"),
                                     helpText("Select file containing taxa matrix for calculating summary metrics, or metrics calculated by the user.", 
                                              "Taxa identifiers must to be split into 2 rows."),
                                     fileInput(inputId="inrawbioFile",multiple = FALSE, label = h4("Choose CSV file"),
                                               accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                                     checkboxInput("metdata",label="Input data are metrics",value=F),
                                     radioButtons(inputId="rawFormat", label = h4("Input File Format"),
                                                  choices = list("Long" ,"Wide" ),inline=T),
                                     conditionalPanel("input.rawFormat=='Long'",
                                                      uiOutput("longformatoptions")),
                                     
                                     uiOutput("rawformat")
                                     #verbatimTextOutput("rawformatout")
                                     

                                     #fileInput("inbioFile", label = h4("File input - Taxa")),
                                     #checkboxInput("metdata",label="Input data are metrics",value=F),
                                     #numericInput("taxa.names", 
                                    #              label = h4("Number of rows used for taxa identifiers"), 
                                    #              value = 2),
                                    # 
                                    # numericInput("site.names", 
                                    #              label = h4("Number of columns used for site identifiers"), 
                                    #              value = 2),
                                    # br(),
                                    # "-------------------------------------",
                                    # br(),
                                    # conditionalPanel("input.metdata==false",actionButton('downloadmetricData', 'Export Metrc Data')),
                                    # actionButton('downloadtransmetricData', 'Export Transformed Metrc Data')
                                   ),
                                   mainPanel(
                                     tabsetPanel(type="tabs",
                                                 tabPanel("Raw Data", dataTableOutput("rawDataView"))
                                  #               tabPanel("Raw Data", dataTableOutput("bio.data.view")),
                                  #               tabPanel("Taxa Data", dataTableOutput("bio.data.view")),
                                  #               tabPanel("Metric Data", dataTableOutput("metric.data.view")),
                                  #               tabPanel("Metric Summary", verbatimTextOutput("metric.summary.view")),
                                  #               navbarMenu("Transformations",
                                  #                          tabPanel("Transformations",sidebarLayout(
                                  #                            sidebarPanel(
                                  #                              uiOutput("sel.met.for.trans"),
                                  #                              radioButtons("trans", label = h3("Transformation"),
                                  #                                           choices = list("None" = "None", "Log10" = "Log10", "Log10+1" = "Log10+1", "Square Root" = "Square Root", "Inverse" = "Inverse", "Arcsine Sqare Root"= "Arcsine Sqare Root", "Logit" = "Logit", "Delete"="Delete"), 
                                  #                                           selected = "None"),
                                  #                              actionButton("apply.trans",label="Apply Selection"),
                                  #                              tableOutput("met.trans.table")
                                  #                              
                                  #                            ),
                                  #                            mainPanel(
                                  #                              plotOutput("met.trans.plot1"),
                                  #                              plotOutput("met.trans.plot2"),
                                  #                              verbatimTextOutput("trans.summary.stats")
                                  #                            )
                                  #                          )),
                                  #                          tabPanel("Transformed Data",dataTableOutput("transformed.data")))
                                     )
                                   )
                                 ))
                        
    
  )
)))
