library(shiny)
library(BenthicAnalysistesting)
library(shinydashboard)

shinyServer(function(input, output) {

  #########################################################
  #DATA INPUT
  ########################################################
  
  #########################################################
  #Raw Data Manipulation
  ########################################################

  raw.bio.data<- reactive({
    validate(
      need(input$inrawbioFile != "", "Please upload a data file")
    )
    output<-read.csv(input$inrawbioFile$datapath,strip.white=TRUE, header=if(input$metdata==T) T else F)
    output
  })
  
  output$longformatoptions = renderUI({
    if(input$metdata==F){
      selectInput('longformatColname_taxa', label="Column of Taxon Identifiers", choices=colnames(raw.bio.data()),selected="", multiple=FALSE, selectize=TRUE)
    }
    if(input$metdata==T){
      selectInput('longformatColname_taxa', label="Column of Taxon Identifiers", choices=colnames(raw.bio.data()),selected="", multiple=TRUE, selectize=TRUE)
    }
    
  })
  
  output$rawDataView<-renderDataTable({
    raw.bio.data()
  })
  

  #########################################################
  #Input biological Data
  ########################################################
  
  #bio.data<-reactiveValues()
  #bio.data<- reactive({
  #  validate(
  #    need(input$inbioFile != "", "Please select a data set")
  #  )
  #  if (input$metdata==T){
  #    
  #  }
  #  d<-BenthicAnalysistesting::benth.met(x=read.csv(input$inbioFile$datapath, header=F,strip.white=TRUE), tax.fields=input$taxa.names, site.fields=input$site.names, HBI = NULL)
  #  d
  #})

  #output$bio.data.view <- renderDataTable({
  #  bio.data()$Raw.Data
  #})
  
  #output$metric.data.view <- renderDataTable({
  #  bio.data()$Summary.Metrics
  #})
  
  #output$metric.summary.view <- renderPrint({
  #  summary(bio.data()$Summary.Metrics)
  #})

  
})
