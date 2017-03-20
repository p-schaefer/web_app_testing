
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#> library(rsconnect)
#> rsconnect::deployApp('C:/Users/Patrick/Documents/BA_webapp/web_app')

#library(shiny)
#library(rpanel)
#library(MASS)
#library(RColorBrewer)
#library(robustbase)
#library(vegan)
#library(biotools)
#library(caret)
#library(gplots)
#library(mvoutlier)
#library(testit,car)
#library(BenthicAnalysis)

shinyServer(function(input, output) {
  library(BenthicAnalysistesting)

  #########################################################
  #DATA INPUT
  ########################################################
  
  #########################################################
  #Input biological Data
  ########################################################
  
  #bio.data<-reactiveValues()
  bio.data<- reactive({
    validate(
      need(input$inbioFile != "", "Please select a data set")
    )
    d<-BenthicAnalysistesting::benth.met(x=read.csv(input$inbioFile$datapath, header=F,strip.white=TRUE), tax.fields=input$taxa.names, site.fields=input$site.names, HBI = NULL)
    d
  })

  output$bio.data.view <- renderDataTable({
    bio.data()$Raw.Data
  })
  
  output$metric.data.view <- renderDataTable({
    bio.data()$Summary.Metrics
  })
  
  output$metric.summary.view <- renderPrint({
    summary(bio.data()$Summary.Metrics)
  })

  
})
