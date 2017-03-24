library(shiny)
library(BenthicAnalysistesting)
library(shinydashboard)
library(DT)

shinyServer(function(input, output) {

  #########################################################
  #DATA INPUT
  ########################################################
  
  #########################################################
  #Raw Data Manipulation
  ########################################################

  raw.bio.data<- reactive({#Raw data input
    validate(
      need(input$inrawbioFile != "", "Please upload a data file")
    )
    output<-read.csv(input$inrawbioFile$datapath,strip.white=TRUE, header=if(input$metdata==T) T else F)
    output
  })
  
  output$rawDataView<-renderDataTable({#Renders raw data table
    DT::datatable(raw.bio.data(),
                  options(pageLength = 5))
  })
  
  raw.data.rows<-reactive({
    validate(
      need(input$rawFormat!="","")
    )
    if (input$rawFormat=="Wide") {
      as.numeric(input$rawData_taxarows)
    } else {
      1
    }
  })
  
  raw.colnames<-reactive({
    validate(
      need(input$rawFormat!="","")
    )
    
    if (input$rawFormat=="Wide") {
      paste0(raw.bio.data()[1:raw.data.rows(),],sep=";")
    }
  })

  output$wideTaxaCols1 = renderUI({#taxa/metric ID when 2 or more rows used for identifiers - wide format
    selectInput(inputId="widetaxacols1", label=h5('Columns of taxa or metrics'), multiple = TRUE,selectize=FALSE,
                choices=raw.colnames()[!raw.colnames()%in%input$rawsitecols&
                                         !raw.colnames()%in%input$rawhabitatcols&
                                         !raw.colnames()%in%input$raweastingcols&
                                         !raw.colnames()%in%input$rawnorthingcols&
                                         !raw.colnames()%in%input$rawepsgcols])    
  })
  output$wideTaxaCols2 = renderUI({#taxa/metric ID when 1 row is used for identifiers - wide format
    selectInput(inputId="widetaxacols2", label=h5('Columns of taxa or metrics'), multiple = TRUE,selectize=FALSE,
                choices=raw.colnames()[!raw.colnames()%in%input$rawsitecols&
                                         !raw.colnames()%in%input$rawhabitatcols&
                                         !raw.colnames()%in%input$raweastingcols&
                                         !raw.colnames()%in%input$rawnorthingcols&
                                         !raw.colnames()%in%input$rawepsgcols])    
  })

  output$wideSiteIDCols = renderUI({#number of rows used for site identifiers
    selectInput(inputId="rawsitecols", label=h5('Columns of Sites/Sampling events'), multiple = TRUE,selectize=FALSE,
                choices=raw.colnames()[!raw.colnames()%in%input$widetaxacols1&
                                         !raw.colnames()%in%input$widetaxacols2&
                                         !raw.colnames()%in%input$rawhabitatcols&
                                         !raw.colnames()%in%input$raweastingcols&
                                         !raw.colnames()%in%input$rawnorthingcols&
                                         !raw.colnames()%in%input$rawepsgcols])    
  })
  
  output$habitatCols = renderUI({#number of rows used for habitat desciptors
    selectInput(inputId="rawhabitatcols", label=h5('Columns of Habitat Descriptors'), multiple = TRUE,selectize=FALSE,
                choices=raw.colnames()[!raw.colnames()%in%input$widetaxacols1&
                                         !raw.colnames()%in%input$widetaxacols2&
                                         !raw.colnames()%in%input$rawsitecols&
                                         !raw.colnames()%in%input$raweastingcols&
                                         !raw.colnames()%in%input$rawnorthingcols&
                                         !raw.colnames()%in%input$rawepsgcols])    
  })
  
  output$eastingCols = renderUI({#number of rows used for Easting/Latitude
    selectInput(inputId="raweastingcols", label=h5('Columns of Eastings or Longitude'),selectize=F,selected="",
                choices=c("",raw.colnames()[!raw.colnames()%in%input$widetaxacols1&
                                         !raw.colnames()%in%input$widetaxacols2&
                                         !raw.colnames()%in%input$rawsitecols&
                                         !raw.colnames()%in%input$rawhabitatcols&
                                         !raw.colnames()%in%input$rawnorthingcols&
                                         !raw.colnames()%in%input$rawepsgcols]))    
  })
  output$northingCols = renderUI({#number of rows used for Easting/Latitude
    selectInput(inputId="rawnorthingcols", label=h5('Columns of Northings or Latitude'),selectize=F,selected="",
                choices=c("",raw.colnames()[!raw.colnames()%in%input$widetaxacols1&
                                         !raw.colnames()%in%input$widetaxacols2&
                                         !raw.colnames()%in%input$rawsitecols&
                                         !raw.colnames()%in%input$rawhabitatcols&
                                         !raw.colnames()%in%input$raweastingcols&
                                         !raw.colnames()%in%input$rawepsgcols]))    
  })
  output$EPSGCols = renderUI({#number of rows used for Easting/Latitude
    selectInput(inputId="rawepsgcols", label=h5('Columns of EPSG codes'),selectize=F,selected="",
                choices=c("",raw.colnames()[!raw.colnames()%in%input$widetaxacols1&
                                         !raw.colnames()%in%input$widetaxacols2&
                                         !raw.colnames()%in%input$rawsitecols&
                                         !raw.colnames()%in%input$rawhabitatcols&
                                         !raw.colnames()%in%input$raweastingcols&
                                         !raw.colnames()%in%input$rawnorthingcols]))    
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
