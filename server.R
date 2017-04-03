library(BenthicAnalysistesting)
library(shinyjs)
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(sp)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

shinyServer(function(input, output, session) {

  #########################################################
  #DATA INPUT
  ########################################################
  
  #########################################################
  #Raw Data Manipulation
  ########################################################

  raw.bio.data<- reactiveValues(data=NULL)
  
  observeEvent(input$inrawbioFile,{
    #req(input$inrawbioFile)
    raw.bio.data$data<-read.csv(input$inrawbioFile$datapath,strip.white=TRUE, header=F)
    raw.bio.data$data<-data.frame(apply(raw.bio.data$data,2,as.character))
  })
  
  observeEvent(input$clear_all,{
    shinyjs::reset('inrawbioFile')
    shinyjs::reset('rawFormat')
    
    raw.bio.data$data<-NULL
    taxa.by.site$data<-NULL
    bio.data$data<-NULL
    habitat.by.site$data<-NULL
    site.ID.cols$data<-NULL
    taxa.ID.cols$data<-NULL
    abund.ID.cols$data<-NULL
    habitat.ID.cols$data<-NULL
    reftest.ID.cols$data<-NULL
    coord.ID.cols$east<-NULL
    coord.ID.cols$north<-NULL
    coord.ID.cols$espg<-NULL
  })
  
  output$rawDataView<-renderDataTable({#Renders raw data table
    DT::datatable(raw.bio.data$data, options=list(pageLength = 5,scrollX=T))
  })
  
  raw.data.rows<-reactive({
    validate(
      need(input$rawFormat!=0,"")
    )
    if (input$rawFormat=="Wide") {
      as.numeric(input$rawData_taxarows)
    } else {
      1
    }
  })
  
  raw.colnames<-reactive({
    validate(
      need(input$rawFormat!=0,"")
    )
    data<-raw.bio.data$data
    output<-as.vector(sapply(data[1:max(raw.data.rows(),1),],paste0,collapse="",sep=";"))
    substr(output,start=1,stop=(nchar(output)-1))
  })

  output$wideTaxaCols1 = renderUI({#taxa/metric ID when 2 or more rows used for identifiers - wide format
    validate(
      need(input$rawFormat!=0, "Must select Long or Wide data format")
    )
    selectInput(inputId="widetaxacols1", label=h5('Columns of taxa or metrics'), multiple = TRUE,selectize=FALSE,size=10,
                choices=raw.colnames()[!raw.colnames()%in%site.ID.cols$data&
                                         !raw.colnames()%in%taxa.ID.cols$data&
                                         !raw.colnames()%in%habitat.ID.cols$data&
                                         !raw.colnames()%in%abund.ID.cols$data&
                                         !raw.colnames()%in%coord.ID.cols$east&
                                         !raw.colnames()%in%coord.ID.cols$north&
                                         !raw.colnames()%in%coord.ID.cols$espg&
                                         !raw.colnames()%in%reftest.ID.cols$data
                                         ])    
  })
  output$test.vs.ref = renderUI({#taxa/metric ID when 2 or more rows used for identifiers - wide format
    selectInput(inputId="raw.testrefcols", label=h5('TEST or REF Site'), multiple = F,selectize=T,selected = "",
                choices=raw.colnames()[!raw.colnames()%in%taxa.ID.cols$data&
                                         !raw.colnames()%in%habitat.ID.cols$data&
                                         !raw.colnames()%in%abund.ID.cols$data&
                                         !raw.colnames()%in%coord.ID.cols$east&
                                         !raw.colnames()%in%coord.ID.cols$north&
                                         !raw.colnames()%in%coord.ID.cols$espg
                                       ])    
  })
  output$eastingCols = renderUI({#taxa/metric ID when 1 row is used for identifiers - wide format
    selectInput(inputId="raw.east", label=h5('Easting or Longitude'), multiple = F,selectize=T,selected = "",
                choices=raw.colnames()[!raw.colnames()%in%taxa.ID.cols$data&
                                         !raw.colnames()%in%abund.ID.cols$data&
                                         !raw.colnames()%in%coord.ID.cols$north&
                                         !raw.colnames()%in%coord.ID.cols$espg&
                                         !raw.colnames()%in%reftest.ID.cols$data
                                       ])    
  })
  output$northingCols = renderUI({#taxa/metric ID when 1 row is used for identifiers - wide format
    selectInput(inputId="raw.north", label=h5('Northing or Latitude'), multiple = F,selectize=T,selected = "",
                choices=raw.colnames()[!raw.colnames()%in%taxa.ID.cols$data&
                                         !raw.colnames()%in%abund.ID.cols$data&
                                         !raw.colnames()%in%coord.ID.cols$east&
                                         !raw.colnames()%in%coord.ID.cols$espg&
                                         !raw.colnames()%in%reftest.ID.cols$data
                                       ])    
  })
  output$ESPGCols = renderUI({#taxa/metric ID when 1 row is used for identifiers - wide format
    selectInput(inputId="raw.espg", label=h5('ESPG'), multiple = F,selectize=T,selected = "",
                choices=raw.colnames()[!raw.colnames()%in%taxa.ID.cols$data&
                                         !raw.colnames()%in%abund.ID.cols$data&
                                         !raw.colnames()%in%coord.ID.cols$east&
                                         !raw.colnames()%in%coord.ID.cols$north&
                                         !raw.colnames()%in%reftest.ID.cols$data
                                       ])    
  })
  
  site.ID.cols<-reactiveValues(data=NULL) #Set site ID columns
  observeEvent(input$raw.siteID.cols,{
    site.ID.cols$data<-input$widetaxacols1
  })
  observeEvent(input$raw.siteID.cols.rem,{
    site.ID.cols$data<-NULL
  })
  taxa.ID.cols<-reactiveValues(data=NULL) #Set Taxa/Metrics columns
  observeEvent(input$raw.taxa.cols,{
    taxa.ID.cols$data<-input$widetaxacols1
  })
  observeEvent(input$raw.taxa.cols.rem,{
    taxa.ID.cols$data<-NULL
  })
  habitat.ID.cols<-reactiveValues(data=NULL) #Set Habitat columns
  observeEvent(input$raw.habitat.cols,{
    habitat.ID.cols$data<-input$widetaxacols1
  })
  observeEvent(input$raw.habitat.cols.rem,{
    habitat.ID.cols$data<-NULL
  })
  abund.ID.cols<-reactiveValues(data=NULL) #Set Habitat columns
  observeEvent(input$raw.abund.cols,{
    abund.ID.cols$data<-input$widetaxacols1
  })
  observeEvent(input$raw.abund.cols.rem,{
    abund.ID.cols$data<-NULL
  })
  
  reftest.ID.cols<-reactiveValues(data=NULL) #Set Habitat columns
  observeEvent(input$raw.testref.cols,{
    reftest.ID.cols$data<-input$raw.testrefcols
  })
  observeEvent(input$raw.testref.cols.rem,{
    reftest.ID.cols$data<-NULL
  })
  

  coord.ID.cols<-reactiveValues(east=NULL,north=NULL,espg=NULL) #Set Coordinate columns
  observeEvent(input$raw.coord.cols,{
    coord.ID.cols$east<-input$raw.east
    coord.ID.cols$north<-input$raw.north
    coord.ID.cols$espg<-input$raw.espg
  })
  observeEvent(input$raw.coord.cols.rem,{
    coord.ID.cols$east<-NULL
    coord.ID.cols$north<-NULL
    coord.ID.cols$espg<-NULL
  })
  
  output$finalizeRaw<-reactive({ #when site ID and taxa ID columns are entered, the option to finalize options becomes available
    validate(
      need(input$rawFormat != 0, "")
    )
    if (!is.null(site.ID.cols$data)&!is.null(taxa.ID.cols$data)&input$rawFormat=="Wide"){
      TRUE
    } else {
      if (!is.null(site.ID.cols$data)&!is.null(taxa.ID.cols$data)&input$rawFormat=="Long"&!is.null(abund.ID.cols$data)){
      TRUE 
      } else {
      FALSE
      }
    }
  }) #Detects when enough data have been specified to finalize
  outputOptions(output, 'finalizeRaw', suspendWhenHidden=FALSE)
  
  output$show_mets<-reactive({
    validate(
      need(input$rawFormat != 0, "")
    )
    if(!is.null(taxa.by.site$data)) {
      TRUE
    } else {
      FALSE
    }
  })  #Detects when enough data have been specified to allow calculation of benthic metrics
  outputOptions(output, 'show_mets', suspendWhenHidden=FALSE)
  
  output$show_mets2<-reactive({
    validate(
      need(input$rawFormat != 0, "")
    )
    if(!is.null(bio.data$data)) {
      TRUE
    } else {
      FALSE
    }
  }) #Detects when enough data have been specified to allow download of benthic metrics
  outputOptions(output, 'show_mets2', suspendWhenHidden=FALSE)
  
  #########################################################
  #When Raw Data are finalized
  ########################################################
  
  taxa.by.site<-reactiveValues(data=NULL) #calculate taxa by site table
  observeEvent(input$finalize_raw,{
    isolate(
      if (input$rawFormat=="Wide"){
        site.names<-apply(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
        site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
        output<-raw.bio.data$data[max(raw.data.rows()+1,2):nrow(raw.bio.data$data),raw.colnames()%in%taxa.ID.cols$data]
        output<-data.frame(apply(output,2,as.numeric))
        rownames(output)<-site.names
        colnames(output)<-taxa.ID.cols$data
      }
    )
    isolate(
      if (input$rawFormat=="Long") {
        site.names<-apply(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
        site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
        #site.names<-unique(site.names)
        
        taxa.names<-apply(raw.bio.data$data[-c(1),raw.colnames()%in%taxa.ID.cols$data],1,paste0,collapse="",sep=";")
        taxa.names<-substr(taxa.names,start=1,stop=(nchar(taxa.names)-1))
        #taxa.names<-unique(taxa.names)
        
        input<-data.frame(sites=site.names,taxa=taxa.names, abund=as.numeric(raw.bio.data$data[-c(1),raw.colnames()%in%abund.ID.cols$data]))
        int.output<-aggregate(abund~sites+taxa, data=input,sum)
        output<-as.data.frame.matrix(xtabs(abund~sites+taxa,data=int.output))
      }
    )
    taxa.by.site$data<-output
  })
  output$view.taxa<-renderDataTable({#Renders raw data table
    DT::datatable(taxa.by.site$data,
                  options=list(pageLength = 5,scrollX=T))
  })
  
  habitat.by.site<-reactiveValues(data=NULL) #calculate habitat by site table
  observeEvent(input$finalize_raw,{
    if (!is.null(habitat.ID.cols$data)){
      isolate(
        if (input$rawFormat=="Wide"){
          site.names<-apply(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
          site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
          output<-raw.bio.data$data[max(raw.data.rows()+1,2):nrow(raw.bio.data$data),raw.colnames()%in%habitat.ID.cols$data]
          output<-data.frame(apply(output,2,as.numeric))
          rownames(output)<-site.names
          colnames(output)<-habitat.ID.cols$data
        }
      )
      isolate(
        if (input$rawFormat=="Long") {
          site.names<-apply(raw.bio.data$data[,raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
          site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
          #site.names<-unique(site.names)
          
          output<-data.frame(raw.bio.data$data[,raw.colnames()%in%habitat.ID.cols$data])
          output<-output[!duplicated(site.names),]
          rownames(output)<-site.names[!duplicated(site.names)]
          colnames(output)<-habitat.ID.cols$data
          output<-output[-c(1),]
        }
      )
      habitat.by.site$data<-output
    }
  })
  output$view.habitat<-renderDataTable({#Renders raw data table
    DT::datatable(habitat.by.site$data,
                  options=list(pageLength = 5,scrollX=T))
  })
 
  #########################################################
  #Calculate Summary Metrics
  ########################################################
  
  bio.data<-reactiveValues(data=NULL)
  
  observeEvent(input$calculate_metrics,{
    if(input$metdata==F){
      bio.data$data<-BenthicAnalysistesting::benth.metUI(x=taxa.by.site$data, taxa.sep = input$taxa_sep, HBI=NULL)
    }
  })
  output$view.metrics.raw<-renderDataTable(
    DT::datatable(bio.data$data$Summary.Metrics, options=list(pageLength = 5,scrollX=T))
  )
  output$download_raw_mets<-downloadHandler(filename = function() { paste("Metrics-",input$inrawbioFile, sep='') },
                  content = function(file) {write.csv(bio.data$data$Summary.Metrics,file)})
  
})
