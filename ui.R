
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

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
                                     helpText("Select file containing raw taxa data for calculating summary metrics, or metrics calculated by the user.", 
                                              "Taxa identifiers must to be split into 2 rows."),
                                     
                                     fileInput("inbioFile", label = h4("File input - Taxa")),
                                     checkboxInput("metdata",label="Input data are metrics",value=F),
                                     numericInput("taxa.names", 
                                                  label = h4("Number of rows used for taxa identifiers"), 
                                                  value = 2),
                                     
                                     numericInput("site.names", 
                                                  label = h4("Number of columns used for site identifiers"), 
                                                  value = 2),
                                     br(),
                                     "-------------------------------------",
                                     br(),
                                     conditionalPanel("input.metdata==false",actionButton('downloadmetricData', 'Export Metrc Data')),
                                     actionButton('downloadtransmetricData', 'Export Transformed Metrc Data')
                                   ),
                                   mainPanel(
                                     tabsetPanel(type="tabs",
                                                 tabPanel("Taxa Data", dataTableOutput("bio.data.view")),
                                                 tabPanel("Metric Data", dataTableOutput("metric.data.view")),
                                                 tabPanel("Metric Summary", verbatimTextOutput("metric.summary.view")),
                                                 navbarMenu("Transformations",
                                                            tabPanel("Transformations",sidebarLayout(
                                                              sidebarPanel(
                                                                uiOutput("sel.met.for.trans"),
                                                                radioButtons("trans", label = h3("Transformation"),
                                                                             choices = list("None" = "None", "Log10" = "Log10", "Log10+1" = "Log10+1", "Square Root" = "Square Root", "Inverse" = "Inverse", "Arcsine Sqare Root"= "Arcsine Sqare Root", "Logit" = "Logit", "Delete"="Delete"), 
                                                                             selected = "None"),
                                                                actionButton("apply.trans",label="Apply Selection"),
                                                                tableOutput("met.trans.table")
                                                                
                                                              ),
                                                              mainPanel(
                                                                plotOutput("met.trans.plot1"),
                                                                plotOutput("met.trans.plot2"),
                                                                verbatimTextOutput("trans.summary.stats")
                                                              )
                                                            )),
                                                            tabPanel("Transformed Data",dataTableOutput("transformed.data")))
                                     )
                                   )
                                 ))
                        
    
  )
)))
