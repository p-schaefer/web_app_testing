library(BenthicAnalysistesting)
library(shinyjs)
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(sp)
library(rgdal)
library(leaflet.minicharts)
library(colorRamps)
library(plyr)
library(dplyr)
library(ggplot2)
library(leaflet.minicharts)
library(vegan)
library(reshape2)

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  
  #########################################################
  #Login
  #########################################################
  loggedin<-F
    
  login.modal<-function(failed=F){
    modalDialog(
      size="s",
      textInput("username","User Name"),
      passwordInput("password","Password"),
      footer = actionButton("login","Login"),
      easyClose = F,
      if (failed){
        div(tags$b("Invalid user name or password", style = "color: red;"))
      }
    )
  }
  
  if (loggedin==F){
    showModal(login.modal())
  }
  
  observeEvent(input$login, {
    # Check that data object exists and is data frame.
    if (input$username=="Admin" & input$password=="Admin1867") {
      removeModal()
    } else {
      showModal(login.modal(failed = TRUE))
      loggedin<-T
    }
  })
  
  #########################################################
  #DATA INPUT
  #########################################################
  #########################################################
  #    Raw Data Manipulation
  #########################################################
  
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
    output<-substr(output,start=1,stop=(nchar(output)-1))
    while(any(substr(output,1,1)==";")){
      output[substr(output,1,1)==";"]<-substr(output,2,nchar(output))[substr(output,1,1)==";"]
    }
    output
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
    selectInput(inputId="raw.testrefcols", label=h5('Test(1) or Reference(0) Site'), multiple = F,selectize=T,selected = "",
                choices=c("None",raw.colnames()[!raw.colnames()%in%taxa.ID.cols$data&
                                         !raw.colnames()%in%habitat.ID.cols$data&
                                         !raw.colnames()%in%abund.ID.cols$data&
                                         !raw.colnames()%in%coord.ID.cols$east&
                                         !raw.colnames()%in%coord.ID.cols$north&
                                         !raw.colnames()%in%coord.ID.cols$espg
                                       ]))    
  })
  output$eastingCols = renderUI({#taxa/metric ID when 1 row is used for identifiers - wide format
    selectInput(inputId="raw.east", label=h5('Easting or Longitude'), multiple = F,selectize=T,selected = "",
                choices=c("None",raw.colnames()[!raw.colnames()%in%taxa.ID.cols$data&
                                         !raw.colnames()%in%abund.ID.cols$data&
                                         !raw.colnames()%in%coord.ID.cols$north&
                                         !raw.colnames()%in%coord.ID.cols$espg&
                                         !raw.colnames()%in%reftest.ID.cols$data
                                       ]))    
  })
  output$northingCols = renderUI({#taxa/metric ID when 1 row is used for identifiers - wide format
    selectInput(inputId="raw.north", label=h5('Northing or Latitude'), multiple = F,selectize=T,selected = "",
                choices=c("None",raw.colnames()[!raw.colnames()%in%taxa.ID.cols$data&
                                         !raw.colnames()%in%abund.ID.cols$data&
                                         !raw.colnames()%in%coord.ID.cols$east&
                                         !raw.colnames()%in%coord.ID.cols$espg&
                                         !raw.colnames()%in%reftest.ID.cols$data
                                       ]))    
  })
  output$ESPGCols = renderUI({#taxa/metric ID when 1 row is used for identifiers - wide format
    selectInput(inputId="raw.espg", label=h5('ESPG'), multiple = F,selectize=T,selected = "",
                choices=c("None",raw.colnames()[!raw.colnames()%in%taxa.ID.cols$data&
                                         !raw.colnames()%in%abund.ID.cols$data&
                                         !raw.colnames()%in%coord.ID.cols$east&
                                         !raw.colnames()%in%coord.ID.cols$north&
                                         !raw.colnames()%in%reftest.ID.cols$data
                                       ]))    
  })
  
  output$time_ID<-renderUI({
    validate(
      need(!is.null(site.ID.cols$data),"")
    )
    selectInput(inputId = "time.ID", label="", multiple=F,selectize=T,
                choices=c("",raw.colnames()[raw.colnames()%in%site.ID.cols$data]))
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
  abund.ID.cols<-reactiveValues(data=NULL) #Set Abundance columns
  observeEvent(input$raw.abund.cols,{
    abund.ID.cols$data<-input$widetaxacols1
  })
  observeEvent(input$raw.abund.cols.rem,{
    abund.ID.cols$data<-NULL
  })
  
  reftest.ID.cols<-reactiveValues(data=NULL) #Set Ref v.s test site columns
  observeEvent(input$finalize_raw,{
    reftest.ID.cols$data<-input$raw.testrefcols
  })
  #observeEvent(input$raw.testref.cols.rem,{
  #  reftest.ID.cols$data<-NULL
  #})
  
  coord.ID.cols<-reactiveValues(east=NULL,north=NULL,espg=NULL) #Set Coordinate columns
  observeEvent(input$finalize_raw,{
    coord.ID.cols$east<-input$raw.east
    coord.ID.cols$north<-input$raw.north
    coord.ID.cols$espg<-input$raw.espg
  })
  #observeEvent(input$raw.coord.cols.rem,{
  #  coord.ID.cols$east<-NULL
  #  coord.ID.cols$north<-NULL
  #  coord.ID.cols$espg<-NULL
  #})
  
  #output$view.coord.cols<-renderPrint({
  #  paste0(coord.ID.cols$east," | ",
  #  coord.ID.cols$north," | ",
  #  coord.ID.cols$espg)
  #})
  
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
    } else if (input$metdata==T){
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
  #    When Raw Data are finalized
  #########################################################
  
  taxa.by.site<-reactiveValues(data=NULL,data.alt.colnames=NULL) #calculate taxa by site table
  observeEvent(input$finalize_raw,{
    if (T){
      isolate(
        if (input$rawFormat=="Wide"){
          if (length(site.ID.cols$data)==1){
            site.names<-as.vector(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data])
          } else {
            site.names<-apply(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
            site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
          }
          output<-raw.bio.data$data[max(raw.data.rows()+1,2):nrow(raw.bio.data$data),raw.colnames()%in%taxa.ID.cols$data]
          output<-data.frame(apply(output,2,as.numeric))
          rownames(output)<-site.names
          colnames(output)<-taxa.ID.cols$data
          output<-output[site.names[!duplicated(site.names)],]
        }
      )
      isolate(
        if (input$rawFormat=="Long") {
          if (length(site.ID.cols$data)==1){
            site.names<-as.vector(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data])
          } else {
            site.names<-apply(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
            site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
          }
          
          if (length(taxa.ID.cols$data)==1){
            taxa.names<-as.vector(raw.bio.data$data[-c(1),raw.colnames()%in%taxa.ID.cols$data])
          } else {
            taxa.names<-apply(raw.bio.data$data[-c(1),raw.colnames()%in%taxa.ID.cols$data],1,paste0,collapse="",sep=";")
            taxa.names<-substr(taxa.names,start=1,stop=(nchar(taxa.names)-1))
            
          }
          
          input<-data.frame(sites=site.names,taxa=taxa.names, abund=as.numeric(as.character(raw.bio.data$data[-c(1),raw.colnames()%in%abund.ID.cols$data])))
          int.output<-aggregate(abund~taxa+sites, data=input,sum)
          output<-as.data.frame.matrix(xtabs(abund~sites+taxa,data=int.output))
          output<-output[site.names[!duplicated(site.names)],]
        }
      )
      output<-do.call(data.frame,lapply(output, function(x) type.convert(as.character(x))))
      rownames(output)<-site.names[!duplicated(site.names)]
      colnames(output)<-gsub(".",";",colnames(output),fixed=T)
      taxa.by.site$data<-output
      
      taxa.by.site$data.alt.colnames<-output
      colnames(taxa.by.site$data.alt.colnames)<-gsub(";",".",colnames(output),fixed=T)
    }
  })
  output$view.taxa<-renderDataTable({#Renders raw data table
    DT::datatable(taxa.by.site$data,
                  options=list(pageLength = 5,scrollX=T))
  })
  
  missing.sampling.events<-reactiveValues(full.data=NULL,rnames=NULL) #if a time field is specified, find missing sampling events
  observeEvent(input$finalize_raw,{
    validate(
      need(input$time.ID!="","")
    )
    isolate(
      if (input$time.ID!="") {
        orig.ID<-rownames(taxa.by.site$data)
        orig.ID<-data.frame(do.call(rbind,strsplit(as.character(orig.ID),";")))
        colnames(orig.ID)<-site.ID.cols$data
        orig.ID[,input$time.ID]<-as.numeric(as.character(orig.ID[,input$time.ID]))
        
        non.time.ID<-as.factor(orig.ID[,!colnames(orig.ID)%in%input$time.ID])
        if (length(which(!colnames(orig.ID)%in%input$time.ID))>1){
          non.time.ID<-apply(non.time.ID,1,paste0,collapse="",sep=";")
          non.time.ID<-as.factor(substr(non.time.ID,start=1,stop=(nchar(non.time.ID)-1)))
        }
        
        time.range<-min(orig.ID[,input$time.ID]):max(orig.ID[,input$time.ID])
        
        expanded.ID<-expand.grid(X1=levels(non.time.ID),X2=time.range)
        
        expanded.ID1<-data.frame(cbind(do.call(rbind,strsplit(as.character(expanded.ID$X1),";")),expanded.ID[,-c(1)]))
        colnames(expanded.ID1)<-c(site.ID.cols$data[!site.ID.cols$data%in%input$time.ID],input$time.ID)
        expanded.ID1<-expanded.ID1[,site.ID.cols$data]
        
        orig.ID$missing<-T
        
        missing.samples<-merge(orig.ID,expanded.ID1,all=T)
        missing.samples<-missing.samples[is.na(missing.samples$missing),!colnames(missing.samples)%in%"missing"]
        
        rnames<-apply(missing.samples,1,paste0,collapse="",sep=";")
        rnames<-substr(rnames,start=1,stop=(nchar(rnames)-1))
        
        missing.sampling.events$full.data<-missing.samples
        missing.sampling.events$rnames<-rnames
      }
    )
  })

  habitat.by.site<-reactiveValues(data=NULL) #calculate habitat by site table
  observeEvent(input$finalize_raw,{
    if (!is.null(habitat.ID.cols$data)){
      isolate(
        if (input$rawFormat=="Wide"){
          if (length(site.ID.cols$data)==1){
            site.names<-as.vector(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data])
          } else {
            site.names<-apply(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
            site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
          }
          output<-raw.bio.data$data[max(raw.data.rows()+1,2):nrow(raw.bio.data$data),raw.colnames()%in%habitat.ID.cols$data]
          rownames(output)<-site.names
          colnames(output)<-habitat.ID.cols$data
        }
      )
      isolate(
        if (input$rawFormat=="Long") {
          if (length(site.ID.cols$data)==1){
            site.names<-as.vector(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data])
          } else {
            site.names<-apply(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
            site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
          }
          
          output<-data.frame(raw.bio.data$data[-c(1),raw.colnames()%in%habitat.ID.cols$data])
          output<-output[!duplicated(site.names),]
          rownames(output)<-site.names[!duplicated(site.names)]
          colnames(output)<-habitat.ID.cols$data
        }
      )
      output<-do.call(data.frame,lapply(output, function(x) type.convert(as.character(x))))
      rownames(output)<-site.names[!duplicated(site.names)]
      habitat.by.site$data<-output
    }
  })
  
  output$view.habitat<-renderDataTable({#Renders raw data table
    DT::datatable(habitat.by.site$data,
                  options=list(pageLength = 5,scrollX=T))
  })
  
  coordinates.by.site<-reactiveValues(data.all=NULL,data.unique=NULL,gis.site.id=NULL) #coordinate by site table
  observeEvent(input$finalize_raw,{
    if (!is.null(coord.ID.cols$east)&!is.null(coord.ID.cols$north)&!is.null(coord.ID.cols$espg)){
      if (coord.ID.cols$east!="None" & coord.ID.cols$north!="None" & coord.ID.cols$espg!="None"){
        isolate(
          if (input$rawFormat=="Wide"){
            if (length(site.ID.cols$data)==1){
              site.names<-as.vector(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data])
            } else {
              site.names<-apply(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
              site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
            }
            output<-raw.bio.data$data[max(raw.data.rows()+1,2):nrow(raw.bio.data$data),
                                      c(
                                        which(raw.colnames()%in%coord.ID.cols$east),
                                        which(raw.colnames()%in%coord.ID.cols$north),
                                        which(raw.colnames()%in%coord.ID.cols$espg)
                                      )]
            rownames(output)<-site.names
            output<-data.frame(apply(output,2,as.numeric))
            output<-cbind(do.call(rbind,strsplit(site.names,";")),output)
            colnames(output)<-c(site.ID.cols$data,"east","north","epsg")
          }
        )
        isolate(
          if (input$rawFormat=="Long") {
            if (length(site.ID.cols$data)==1){
              site.names<-as.vector(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data])
            } else {
              site.names<-apply(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
              site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
            }
            
            output<-data.frame(raw.bio.data$data[-c(1),
                                                 c(
                                                   which(raw.colnames()%in%coord.ID.cols$east),
                                                   which(raw.colnames()%in%coord.ID.cols$north),
                                                   which(raw.colnames()%in%coord.ID.cols$espg)
                                                 )])
            output<-output[!duplicated(site.names),]
            rownames(output)<-site.names[!duplicated(site.names)]
            colnames(output)<-c("east","north","epsg")
            output<-data.frame(apply(output,2,as.numeric))
            output<-cbind(do.call(rbind,strsplit(site.names[!duplicated(site.names)],";")),output)
            colnames(output)<-c(site.ID.cols$data,"east","north","epsg")
          }
        )
        validate(
          need(!any(is.na(output)),"NAs detected")
        )
        for (i in unique(output$epsg)){ #Convert coordinates to EPSG 4326
          output.coords<-output[output$epsg==i,]
          sp::coordinates(output.coords)<- ~ east+north
          try1<-try(sp::proj4string(output.coords) <- sp::CRS(paste0("+init=epsg:",i)),silent=T)
          validate(need(class(try1)!="try-error", "GIS Projection Error"))
          sp::proj4string(output.coords) <- sp::CRS(paste0("+init=epsg:",i))
          output.coords <- sp::spTransform(output.coords, CRS("+init=epsg:4326"))
          output.coords<-as.data.frame(output.coords)
          output[output$epsg==i,]<-output.coords[,colnames(output)]
        }
        
        if (length(site.ID.cols$data)==1){#Identify site ID column that corresponds to individual sites
          gis.site.id<-site.names
        } else {
          if (input$rawFormat=="Wide"){
            temp1<-data.frame(!apply(raw.bio.data$data[max(raw.data.rows()+1,2):nrow(raw.bio.data$data),raw.colnames()%in%site.ID.cols$data],2,duplicated))
            temp2<-raw.bio.data$data[max(raw.data.rows()+1,2):nrow(raw.bio.data$data),
                                     c(
                                       which(raw.colnames()%in%coord.ID.cols$east),
                                       which(raw.colnames()%in%coord.ID.cols$north),
                                       which(raw.colnames()%in%coord.ID.cols$espg)
                                     )]
          }
          if (input$rawFormat=="Long") {
            temp1<-data.frame(!apply(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data],2,duplicated))
            temp2<-data.frame(raw.bio.data$data[-c(1),
                                                c(
                                                  which(raw.colnames()%in%coord.ID.cols$east),
                                                  which(raw.colnames()%in%coord.ID.cols$north),
                                                  which(raw.colnames()%in%coord.ID.cols$espg)
                                                )])
          }
          temp2<-!duplicated(temp2)
          temp3<-NA
          for (n in 1:ncol(temp1)){
            temp3<-apply(cbind(temp1[,n],temp2), 1, function(x)(all(x)))
            if (identical(temp3,temp2)){
              if (input$rawFormat=="Wide"){
                gis.site.id<-as.character(raw.bio.data$data[max(raw.data.rows()+1,2):nrow(raw.bio.data$data),raw.colnames()%in%site.ID.cols$data[n]])
              }
              if (input$rawFormat=="Long"){
                gis.site.id<-as.character(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data[n]])
              }
              break()
            } else {
              gis.site.id<-site.names
            }
          }
        }
        
        coordinates.by.site$data.unique<-unique(output[,c("east","north","epsg")])
        rownames(coordinates.by.site$data.unique)<-unique(gis.site.id)
        coordinates.by.site$gis.site.id<-gis.site.id
        coordinates.by.site$data.all<-output
      }
    }
  })
  
  output$view.coords<-renderDataTable({#Renders raw data table
    DT::datatable(coordinates.by.site$data.unique,
                  options=list(pageLength = 5,scrollX=T))
  })
  
  reftest.by.site<-reactiveValues(data=NULL) #calculate habitat by site table
  observeEvent(input$finalize_raw,{
    if (!is.null(reftest.ID.cols$data)){
      if (reftest.ID.cols$data!="None"){
        isolate(
          if (input$rawFormat=="Wide"){
            if (length(site.ID.cols$data)==1){
              site.names<-as.vector(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data])
            } else {
              site.names<-apply(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
              site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
            }
            output<-data.frame(as.numeric(as.character(raw.bio.data$data[max(raw.data.rows()+1,2):nrow(raw.bio.data$data),raw.colnames()%in%reftest.ID.cols$data])))
            rownames(output)<-site.names
            colnames(output)<-reftest.ID.cols$data
          }
        )
        isolate(
          if (input$rawFormat=="Long") {
            if (length(site.ID.cols$data)==1){
              site.names<-as.vector(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data])
            } else {
              site.names<-apply(raw.bio.data$data[-c(1),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
              site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
            }
            
            output<-data.frame(as.numeric(as.character(raw.bio.data$data[-c(1),raw.colnames()%in%reftest.ID.cols$data])))
            output<-output[!duplicated(site.names),]
            rownames(output)<-site.names[!duplicated(site.names)]
            colnames(output)<-reftest.ID.cols$data
          }
        )
        output<-do.call(data.frame,lapply(output, function(x) type.convert(as.character(x))))
        rownames(output)<-site.names[!duplicated(site.names)]
        reftest.by.site$data<-output
      }
    }
  })
  
  #########################################################
  #    Calculate Summary Metrics
  #########################################################
  
  bio.data<-reactiveValues(data=NULL)
  
  feeding.data<-reactiveValues(data=NULL,data.reduced=NULL)
  habitat.data<-reactiveValues(data=NULL)
  
  observeEvent(input$finalize_raw,{
    isolate(
      if(input$metdata==F){
        bio.data$data<-BenthicAnalysistesting::benth.metUI(x=taxa.by.site$data, taxa.sep = input$taxa_sep, HBI=NULL)
        bio.data$data$untransformed.metrics<-bio.data$data$Summary.Metrics
        bio.data$data$transformations<-data.frame("Metric"=colnames(bio.data$data$Summary.Metrics), "Transformation"=rep("None",ncol(bio.data$data$Summary.Metrics)))
        bio.data$data$transformations$Transformation<-as.character(bio.data$data$transformations$Transformation)
      }
    )
  })
  observeEvent(input$finalize_raw, {
    isolate(
      if (input$metdata==T){
        bio.data$data$Summary.Metrics<-taxa.by.site$data
        bio.data$data$Raw.Data<-NULL
        bio.data$data$Taxa.List<-NULL
        bio.data$data$Site.List<-rownames(taxa.by.site$data)
        bio.data$data$untransformed.metrics<-bio.data$data$Summary.Metrics
        bio.data$data$transformations<-data.frame("Metric"=colnames(bio.data$data$Summary.Metrics), "Transformation"=rep("None",ncol(bio.data$data$Summary.Metrics)))
        bio.data$data$transformations$Transformation<-as.character(bio.data$data$transformations$Transformation)
      }
    )
  })
  
  observeEvent({input$finalize_raw},{
    isolate(
      if(input$metdata==F){
        feeding.data$data<-aggregate(t(taxa.by.site$data),by=list(bio.data$data$Attributes$Feeding),FUN=sum)
        row.names(feeding.data$data)<-feeding.data$data[,"Group.1"]
        feeding.data$data<-subset(feeding.data$data,select=-c(Group.1))
        feeding.data$data<-t(feeding.data$data)
        colnames(feeding.data$data)<-gsub("COLLECTOR-","",colnames(feeding.data$data))
        
        feeding.data$data.reduced<-data.frame("FILTERER"=rowSums(subset(feeding.data$data,select=grepl("FILTERER",strsplit(colnames(feeding.data$data),"-"))),na.rm=T),
                                              "GATHERER"=rowSums(subset(feeding.data$data,select=grepl("GATHERER",strsplit(colnames(feeding.data$data),"-"))),na.rm=T),
                                              "PREDATOR"=rowSums(subset(feeding.data$data,select=grepl("PREDATOR",strsplit(colnames(feeding.data$data),"-"))),na.rm=T),
                                              "SCRAPER/GRAZER"=rowSums(subset(feeding.data$data,select=grepl("SCRAPER/GRAZER",strsplit(colnames(feeding.data$data),"-"))),na.rm=T),
                                              "PARASITE"=rowSums(subset(feeding.data$data,select=grepl("PARASITE",strsplit(colnames(feeding.data$data),"-"))),na.rm=T),
                                              "PIERCER HERBIVORE"=rowSums(subset(feeding.data$data,select=grepl("PIERCER HERBIVORE",strsplit(colnames(feeding.data$data),"-"))),na.rm=T),
                                              "SHREDDER"=rowSums(subset(feeding.data$data,select=grepl("SHREDDER",strsplit(colnames(feeding.data$data),"-"))),na.rm=T))
        row.names(feeding.data$data.reduced)<-row.names(feeding.data$data)

        habitat.data$data<-aggregate(t(taxa.by.site$data),by=list(bio.data$data$Attributes$Habitat),FUN=sum)
        row.names(habitat.data$data)<-habitat.data$data[,"Group.1"]
        habitat.data$data<-subset(habitat.data$data,select=-c(Group.1))
        habitat.data$data<-t(habitat.data$data)
      }
    )
  })
  
  
  
  
  output$view.metrics.raw<-renderDataTable(
    DT::datatable(bio.data$data$Summary.Metrics, options=list(pageLength = 5,scrollX=T))
  )
  output$download_raw_mets<-downloadHandler(filename = function() { paste("Metrics-",input$inrawbioFile, sep='') },
                                            content = function(file) {write.csv(bio.data$data$Summary.Metrics,file,row.names = T)})
  output$download_raw_taxa<-downloadHandler(filename = function() { paste("Taxa-",input$inrawbioFile, sep='') },
                                            content = function(file) {write.csv(taxa.by.site$data,file,row.names = T)})
  output$download_taxa_atts<-downloadHandler(filename = function() { paste("Attributes-",input$inrawbioFile, sep='') },
                                             content = function(file) {write.csv(bio.data$data$Attributes,file,row.names = T)})
  
  
  #########################################################
  #    Metric Transformations
  #########################################################
  output$out_trans_selected<-renderUI({
    radioButtons("trans", label = "",
                 choices = list("None" = "None", "Log10" = "Log10", "Log10+1" = "Log10+1", "Square Root" = "Square Root", "Inverse" = "Inverse", "Arcsine Sqare Root"= "Arcsine Sqare Root", "Logit" = "Logit", "Delete"="Delete"), 
                 selected = bio.data$data$transformations$Transformation[bio.data$data$transformations$Metric==input$mets_for_trans_in])
  })
  
  output$mets_for_trans_out<-renderUI({
    selectInput("mets_for_trans_in", label=h4("Metrics"), choices=colnames(bio.data$data$untransformed.metrics))
  })
  
  output$applied_transformations<-renderTable({
    data.frame(bio.data$data$transformations)
  })
  
  #trans.metric<-reactiveValues(data=NULL)
  trans.metric<-reactive({
    validate(
      need(if (input$trans=="Log10" & any(bio.data$data$untransformed.metrics[,input$mets_for_trans_in]==0)) {FALSE} else {TRUE}, "Metric contains 0's, try log10(x+1)"),
      need(if (input$trans=="Inverse" & any(bio.data$data$untransformed.metrics[,input$mets_for_trans_in]==0)) {FALSE} else {TRUE}, "Metric contains 0's"),
      need(if (input$trans=="Arcsine Sqare Root" & (bio.data$data$untransformed.metrics[,input$mets_for_trans_in]<0 || (bio.data$data$untransformed.metrics[,input$mets_for_trans_in]>1))) {FALSE} else {TRUE}, "Transofmration only available for values between 0-1"),
      need(input$trans!="Delete","")
    )
    if (input$trans=="None"){
      t.metric<-try(bio.data$data$untransformed.metrics[,input$mets_for_trans_in],silent = T)
    }
    if (input$trans=="Log10"){
      t.metric<-try(log(bio.data$data$untransformed.metrics[,input$mets_for_trans_in]),silent = T)
    }
    if (input$trans=="Log10+1" ){
      t.metric<-try(log(bio.data$data$untransformed.metrics[,input$mets_for_trans_in]+1),silent = T)
    }
    if (input$trans=="Square Root" ){
      t.metric<-try(sqrt(bio.data$data$untransformed.metrics[,input$mets_for_trans_in]),silent = T)
    }
    if (input$trans=="Inverse" ){
      t.metric<-try(1/(bio.data$data$untransformed.metrics[,input$mets_for_trans_in]),silent = T)
    }
    if (input$trans=="Arcsine Sqare Root"){
      t.metric<-try(asin(sqrt(bio.data$data$untransformed.metrics[,input$mets_for_trans_in])),silent = T)
    }
    if (input$trans=="Logit"){
      t.metric<-try(car::logit(bio.data$data$untransformed.metrics[,input$mets_for_trans_in]),silent = T)
    }
    validate(
      need(!(is(t.metric,"try-error")),"")
    )
    if (!is(t.metric,"try-error")) {
      t.metric
    } else {
      NULL
    }
  })
  
  output$met.trans.plot1<-renderPlot({
    validate(
      need(input$mets_for_trans_in != "", "Please select a metric"),
      need(input$trans!="Delete", "Metric to be deleted")#,
    )
    hist(as.numeric(trans.metric()),prob=F,col="grey", main=paste0(input$trans," ",input$mets_for_trans_in), xlab="")
  })
  output$met.trans.plot2<-renderPlot({
    validate(
      need(input$mets_for_trans_in != "", "Please select a metric"),
      need(input$trans!="Delete", "Metric to be deleted")#,
    )
    qqnorm(as.numeric(trans.metric()), main=paste0(input$trans," ",input$mets_for_trans_in), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
    qqline(as.numeric(trans.metric()), datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
  })
  
  observeEvent(input$apply.trans,{
    isolate(
      if (input$trans=="Delete") {
        bio.data$data$Summary.Metrics<-bio.data$data$untransformed.metrics[,!colnames(bio.data$data$untransformed.metrics)%in%input$mets_for_trans_in]
        bio.data$data$transformations$Transformation[bio.data$data$transformations$Metric%in%input$mets_for_trans_in]<-"Deleted"
      } else {
        bio.data$data$Summary.Metrics[,colnames(bio.data$data$untransformed.metrics)%in%input$mets_for_trans_in]<-as.numeric(trans.metric())
        bio.data$data$transformations$Transformation[bio.data$data$transformations$Metric%in%input$mets_for_trans_in]<-as.character(input$trans)
      }
    )
  })
  
  #Batch Transformations
  
  output$batch_met_sel<-renderPrint({
    input<-input$trans_keyword
    input<-gsub(",","|",input)
    input<-gsub(" ","",input)
    colnames(bio.data$data$untransformed.metrics)[grep(input,colnames(bio.data$data$untransformed.metrics))]
  })
  
  batch.met.trans<-reactive({
    input<-input$trans_keyword
    input<-gsub(",","|",input)
    input<-gsub(" ","",input)
    as.vector(colnames(bio.data$data$untransformed.metrics)[grep(input,colnames(bio.data$data$untransformed.metrics))])
  })
  
  observeEvent(input$apply.trans.batch,{
    for (i in batch.met.trans()){
      t.metric<-NA
      if (input$trans.batch=="Delete") {
        bio.data$data$Summary.Metrics<-bio.data$data$untransformed.metrics[,!colnames(bio.data$data$untransformed.metrics)%in%batch.met.trans()]
        bio.data$data$transformations$Transformation[bio.data$data$transformations$Metric%in%batch.met.trans()]<-"Deleted"
      }
      if (input$trans.batch=="None"){
        t.metric<-try(bio.data$data$untransformed.metrics[,i],silent = T)
      }
      if (input$trans.batch=="Log10"){
        if (any(bio.data$data$untransformed.metrics[,i]==0)){
          bio.data$data$transformations$Transformation[bio.data$data$transformations$Metric%in%i]<-"ERROR"
          next
        } else {
          t.metric<-try(log(bio.data$data$untransformed.metrics[,i]),silent = T)
        }
      }
      if (input$trans.batch=="Log10+1" ){
        t.metric<-try(log(bio.data$data$untransformed.metrics[,i]+1),silent = T)
      }
      if (input$trans.batch=="Square Root" ){
        t.metric<-try(sqrt(bio.data$data$untransformed.metrics[,i]),silent = T)
      }
      if (input$trans.batch=="Inverse" ){
        if (any(bio.data$data$untransformed.metrics[,i]==0)){
          bio.data$data$transformations$Transformation[bio.data$data$transformations$Metric%in%i]<-"ERROR"
          next
        } else {
          t.metric<-try(1/(bio.data$data$untransformed.metrics[,i]),silent = T)
        }
      }
      if (input$trans.batch=="Arcsine Sqare Root"){
        if (bio.data$data$untransformed.metrics[,i]<0 || (bio.data$data$untransformed.metrics[,i]>1)){
          bio.data$data$transformations$Transformation[bio.data$data$transformations$Metric%in%i]<-"ERROR"
          next
        } else {
          t.metric<-try(asin(sqrt(bio.data$data$untransformed.metrics[,i])),silent = T)
        }
      }
      if (input$trans.batch=="Logit"){
        t.metric<-try(car::logit(bio.data$data$untransformed.metrics[,i]),silent = T)
      }
      if (is(t.metric,"try-error")) {
        next
      } else {
        bio.data$data$Summary.Metrics[,colnames(bio.data$data$untransformed.metrics)%in%i]<-t.metric
        bio.data$data$transformations$Transformation[bio.data$data$transformations$Metric%in%i]<-as.character(input$trans.batch)
      }
      
    }
  })
  
  output$applied_transformations.batch<-renderTable({
    data.frame(bio.data$data$transformations)
  })
  
  output$view.transformed.metrics<-renderDataTable(
    DT::datatable(bio.data$data$Summary.Metrics, options=list(pageLength = 10,scrollX=T))
  )
  
  #########################################################
  #    Habitat Transformations
  #########################################################
  
  output$dispaly_habitat_factors<-renderUI({
    validate(
      need(any(sapply(habitat.by.site$data,is.factor)),"No variables recognized as factors")
    )
    selectInput(inputId="habitat.factors.selected", label=h5('Select variables to change'), multiple = F,size=10,selectize=F,selected = "",
                choices=colnames(habitat.by.site$data)[sapply(habitat.by.site$data,is.factor)])
    
  })
  
  output$dispaly_habitat_numeric<-renderUI({
    validate(
      need(any(sapply(habitat.by.site$data,function(x)is.numeric(x)|is.factor(x))),"No variables recognized as numeric")
    )
    selectInput(inputId="habitat.numeric.selected", label=h5('Select variables to change'), multiple = F,size=10,selectize=F,selected = "",
                choices=colnames(habitat.by.site$data)[sapply(habitat.by.site$data,is.numeric)|sapply(habitat.by.site$data,is.integer)])
    
  })
  
  observeEvent(input$habitat_convert_fact_to_numb,{
    t1<-try(sapply(habitat.by.site$data[,input$habitat.factors.selected],as.numeric),silent=T)
    if (class(t1)=="try-error") {
      showModal(
        modalDialog(
          size="s",
          helpText("Unable to convert selected variables"),
          hr(),
          footer = modalButton("Dismiss"),
          easyClose = TRUE
        )
      )
    } else {
      habitat.by.site$data[,input$habitat.factors.selected]<-sapply(habitat.by.site$data[,input$habitat.factors.selected],as.numeric)
    } 
  })
  
  observeEvent(input$habitat_convert_numb_to_fact,{
    t1<-try(sapply(habitat.by.site$data[,input$habitat.numeric.selected],as.factor),silent=T)
    if (class(t1)=="try-error") {
      showModal(
        modalDialog(
          size="l",
          helpText("Unable to convert selected variables"),
          hr(),
          footer = modalButton("Dismiss"),
          easyClose = TRUE
        )
      )
    } else {
      habitat.by.site$data[,input$habitat.numeric.selected]<-sapply(habitat.by.site$data[,input$habitat.numeric.selected],as.factor)
    }
  })
  
  #########################################################
  #    Combine all available data into 1 dataset
  #########################################################
  
  all.data<-reactiveValues(data=NULL)
  
  observeEvent(
    c(input$finalize_raw,
      input$calculate_metrics,
      input$in_test_site_select,
      input$nn.k,
      input$nn_useDD,
      input$nn.factor,
      input$nn.constant,
      input$nn_method,
      input$in_metric.select,
      input$tsa_outlier_rem,
      input$tsa_outbound,
      input$useMD,
      input$tsa_weighted
    )
  ,{
    all.data$data<-taxa.by.site$data.alt.colnames
    
    if(input$metdata==F & !is.null(bio.data$data$Summary.Metrics)){
      all.data$data<-data.frame(cbind(all.data$data,bio.data$data$Summary.Metrics,feeding.data$data.reduced,habitat.data$data))
    }
    
    if(!is.null(habitat.by.site$data)){
      all.data$data<-data.frame(cbind(all.data$data,habitat.by.site$data))
    }
    
    if(!is.null(coordinates.by.site$data.all)){
      all.data$data <- data.frame(cbind(all.data$data,coordinates.by.site$data.all))
    }
    
    if (!is.null(reftest.by.site$data)){
      all.data$data <- data.frame(cbind(all.data$data,reftest.by.site$data))
    }
    
    if (!is.null(nn.sites$data)){
      temp<-data.frame(nn.sites$data$TF.matrix)
      temp<-temp[rownames(all.data$data),]
      all.data$data<-data.frame(cbind(all.data$data,temp))
    }
    
    if (!is.null(missing.sampling.events$full.data)&!is.null(coordinates.by.site$data.all)){
      all.data$data <- data.frame(merge(all.data$data,missing.sampling.events$full.data,all=T))
      missing.sites<-as.character(all.data$data[is.na(all.data$data$east),(colnames(all.data$data)%in%site.ID.cols$data & !colnames(all.data$data)%in%input$time.ID)])
      all.data$data$east[is.na(all.data$data$east)]<-coordinates.by.site$data.unique$east[match(missing.sites,rownames(coordinates.by.site$data.unique))]
      all.data$data$north[is.na(all.data$data$north)]<-coordinates.by.site$data.unique$north[match(missing.sites,rownames(coordinates.by.site$data.unique))]
      rownames(all.data$data)<-apply(all.data$data[,site.ID.cols$data], 1 , function(x) paste(x,collapse=";",sep=";"))
    }
    
    if (!is.null(tsa.results$data)){
      all.data$data<-data.frame(cbind(all.data$data,tsa.results$data))
    }
    
    
  })

  #########################################################
  #NN Site Matching + Metric Selection
  #########################################################
  output$out_test.site.select<-renderUI({
    validate(need(!is.null(reftest.ID.cols$data),""))
    selectInput("in_test_site_select","",selected="None",
                choices=c("None",rownames(habitat.by.site$data)[reftest.by.site$data==0])
                )
  })
  
  output$out_metric.select<-renderUI({
    validate(need(!is.null(reftest.ID.cols$data) & !is.null(bio.data$data),""))
    
    if(input$nn_method=="ANNA" & input$metdata==F){
      selectInput("in_metric.select","", multiple = T,selectize = F, size=15,
                  choices=c(colnames(bio.data$data$Summary.Metrics),
                            "O:E","Bray-Curtis","CA1","CA2"), 
                  selected=c(colnames(bio.data$data$Summary.Metrics),
                             "O:E","Bray-Curtis","CA1","CA2")
      )
    } else {
      selectInput("in_metric.select","", multiple = T,selectize = F, size=15,
                  choices=colnames(bio.data$data$Summary.Metrics)
      )
    }
  })

  nn.sites<-reactiveValues(data=NULL)
  observeEvent(c(
    input$in_test_site_select,
    input$nn.k,
    input$nn_useDD,
    input$nn.factor,
    input$nn.constant,
    input$nn_method,
    input$in_metric.select,
    input$nn.scale
  ),{
    nn.sites$data<-NULL
    validate(need(!is.null(habitat.by.site$data) & !is.null(reftest.ID.cols$data),"Missing Habitat data or Reference Sites"))
    validate(need(reftest.ID.cols$data!="None","Missing Reference Sites"))
    validate(need(input$nn_method=="RDA-ANNA" | input$nn_method=="ANNA",""))
    validate(need(!any(is.na(habitat.by.site$data)),"NAs not allowed in habitat data"))
    validate(need(input$nn.k>=3|input$nn_useDD,"Need k>=3 or use Distance-Decay site selection"))
    if (input$nn_method=="RDA-ANNA"){
      validate(need(length(input$in_metric.select)>=3,"Select Metricsat least 3 metrics"))
      validate(need(length(input$in_metric.select)<=(0.5*ncol(habitat.by.site$data)),"Too many metrics for number of habitat variables"))
      validate(need(!any(is.na(bio.data$data$Summary.Metrics[reftest.by.site$data==1,input$in_metric.select])),"NAs not allowed in biological data"))
    }
    
    nn.sites$data<-BenthicAnalysistesting::site.matchUI(Test=habitat.by.site$data[reftest.by.site$data==0,],
                                                        Reference=habitat.by.site$data[reftest.by.site$data==1,],
                                                        k=if (is.numeric(input$nn.k)){input$nn.k} else {NULL},
                                                        distance.decay=input$nn_useDD,
                                                        dd.factor=input$nn.factor,
                                                        dd.constant=input$nn.constant,
                                                        RDA.reference= if (input$nn_method=="RDA-ANNA") {bio.data$data$Summary.Metrics[reftest.by.site$data==1,input$in_metric.select]} else {NULL},
                                                        scale=input$nn.scale)
  })
  
  output$out_nn.axis1<-renderUI({
    selectInput("in_nn.axis1","X Axis", choices=colnames(nn.sites$data$env.ordination.scores),
                selected=colnames(nn.sites$data$env.ordination.scores)[1])
  })
  output$out_nn.axis2<-renderUI({
    selectInput("in_nn.axis2","Y Axis", choices=colnames(nn.sites$data$env.ordination.scores),
                selected=colnames(nn.sites$data$env.ordination.scores)[2])
  })
  
  nn.ord.ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$nn.ord_dblclick, {
    brush <- input$nn.ord_brush
    if (!is.null(brush)) {
      nn.ord.ranges$x <- c(brush$xmin, brush$xmax)
      nn.ord.ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      nn.ord.ranges$x <- NULL
      nn.ord.ranges$y <- NULL
    }
  })
  output$nn.ord<-renderPlot({
    if(is.null(input$in_nn.axis1) | is.null(input$in_nn.axis2)){return(NULL)}
    
    validate(need(!is.null(habitat.by.site$data) & !is.null(reftest.ID.cols$data),"Missing Habitat data or Reference Sites"))
    validate(need(!is.null(nn.sites$data),"Insufficient Information"))
    validate(need(!is.null(input$in_nn.axis1) & !is.null(input$in_nn.axis2),""))
    validate(need(input$nn_method!="User Selected",""))
    
    if (input$nn_method=="RDA-ANNA"){
      validate(need(!is.null(input$in_metric.select),"Select indicator metrics first"))
      validate(need(length(input$in_metric.select)>=3,"Select more than 3 indicator metrics"))
    }
    
    
    if (input$in_test_site_select!="None"){
      hull<-nn.sites$data$ordination.scores[nn.sites$data$ordination.scores$Class=="Reference",]
      hull$Ref<-data.frame(t(nn.sites$data$TF.matrix[rownames(nn.sites$data$TF.matrix)%in%input$in_test_site_select,]))
      hull<-hull[hull$Ref==T,]
      hull<-hull[chull(hull[,c(input$in_nn.axis1,input$in_nn.axis2)]),]
      
      test.site<-nn.sites$data$ordination.scores[rownames(nn.sites$data$ordination.scores)%in%input$in_test_site_select,]
      
      reference.sites<-nn.sites$data$ordination.scores[nn.sites$data$ordination.scores$Class=="Reference",]
      reference.sites$Ref<-data.frame(t(nn.sites$data$TF.matrix[rownames(nn.sites$data$TF.matrix)%in%input$in_test_site_select,]))
      reference.sites<-reference.sites[reference.sites$Ref==T,]
    }

    p1 <- ggplot(data=nn.sites$data$ordination.scores,aes_string(x=input$in_nn.axis1, y=input$in_nn.axis2)) + 
      geom_vline(xintercept = 0, color="darkgrey") + geom_hline(yintercept = 0, color="darkgrey") +
      geom_point(aes(color=Class))  + theme_bw() + 
      labs(title=paste0("Nearest-neighbour Ordination by ",input$nn_method),
           subtitle=if(input$in_test_site_select!="None"){paste0(input$in_test_site_select)} else {NULL}
           ) +
      xlab(paste0(input$in_nn.axis1)) + 
      ylab(paste0(input$in_nn.axis2)) +
      coord_cartesian(xlim = nn.ord.ranges$x, ylim = nn.ord.ranges$y, expand = TRUE)


    if (input$nnplot.hab.points){
      p1<- p1 + geom_point(data=nn.sites$data$env.ordination.scores[,c(input$in_nn.axis1,input$in_nn.axis2)], size=1,shape=8,color="darkred") 
    }
    
    if (input$nnplot.hab.names){
      p1<- p1 + geom_text(data=nn.sites$data$env.ordination.scores[,c(input$in_nn.axis1,input$in_nn.axis2)], label=rownames(nn.sites$data$env.ordination.scores))
      
    }
    
    if (input$in_test_site_select!="None"){
      if (input$nnplot.hull){
        p1<- p1 + geom_polygon(data=hull[,c(input$in_nn.axis1,input$in_nn.axis2)],alpha=0.5)
      }
      if (input$nnplot.refnames){
        p1<- p1 + geom_text(data=reference.sites[,c(input$in_nn.axis1,input$in_nn.axis2)], label=rownames(reference.sites))
      }
      if (input$nnplot.testsite){
        p1<- p1 + geom_point(data=test.site[,c(input$in_nn.axis1,input$in_nn.axis2)],size=3)
      }
    }
    p1
  })
  
  output$nn.dist<-renderPlot({
    validate(need(input$in_test_site_select!="None",""))
    validate(need(input$nn_method!="User Selected",""))
    validate(need(!is.null(nn.sites$data),"Insufficient Information"))
    if (input$in_test_site_select=="None"){return()}
    if (input$in_test_site_select!="None"){
      distances<-nn.sites$data$distance.matrix[rownames(nn.sites$data$distance.matrix)%in%input$in_test_site_select,]
      distances<-distances[order(distances)]
      distances<-data.frame(distances)
      nn.distances<-as.numeric(nn.sites$data$dd.number$dd.number[nn.sites$data$dd.number$sites%in%input$in_test_site_select])
      distances$Selected<-NA
      distances$Selected[1:nn.distances]<-"Yes"
      distances$Selected[(nn.distances+1):nrow(distances)]<-"No"
      colnames(distances)[1]<-"Distance"
      
      p2<-ggplot()+geom_bar(data=distances,aes(y=Distance, x=1:nrow(distances), fill=Selected, color=Selected), stat="identity")+theme_bw()+
        labs(title=paste0("Nearest-neighbour Distances by ",input$nn_method),
             subtitle=paste0(input$in_test_site_select)) +
        xlab("Distance") + 
        ylab("")
      
      p2
      
    }
  })
  


  #########################################################
  #TSA Calculations
  #########################################################
  
  #Calculate Additional metrics if input$nn_method=="ANNA" & input$metdata==F
  additional.metrics<-reactiveValues(data=NULL) #create a list of tables of additional metrics at reference sites
  observeEvent(c(
    input$in_test_site_select,
    input$tsa_outbound,
    input$tsa_outlier_rem,
    input$tsa_weighted,
    input$nn.k,
    input$nn_useDD,
    input$nn.factor,
    input$nn.constant,
    input$nn_method,
    input$in_metric.select,
    input$nn.scale
  ), {
    validate(need(input$in_test_site_select!="None",""))
    temp<-all.data$data
    colnames(temp)<-gsub(".",";",colnames(temp),fixed = T)
    
    Test<-temp[input$in_test_site_select,colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
    ref.set<-nn.sites$data$TF.matrix[rownames(Test),]
    Reference<-temp[names(ref.set)[ref.set==T],colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
    temp2<-BenthicAnalysistesting::add.met(Test=Test,Reference = Reference,original=F)
    rownames(temp2)[nrow(temp2)]<-rownames(Test)
    eval(parse(text=paste0("additional.metrics$data$'",rownames(Test),"'<-temp2")))
    
    
    #for (i in which(all.data$data[,reftest.ID.cols$data]==0)){
    #  Test<-temp[i,colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
    #  ref.set<-nn.sites$data$TF.matrix[rownames(Test),]
    #  Reference<-temp[names(ref.set)[ref.set==T],colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
    #  temp2<-BenthicAnalysistesting::add.met(Test=Test,Reference = Reference,original=F)
    #  rownames(temp2)[nrow(temp2)]<-rownames(Test)
    #  eval(parse(text=paste0("additional.metrics$data$'",rownames(Test),"'<-temp2")))
    #}
  })
  
  tsa.results<-reactiveValues(data=NULL,output.list=NULL) #create a list of tsa.test objects
  observeEvent(c(
    input$in_test_site_select,
    input$tsa_outbound,
    input$tsa_outlier_rem,
    input$tsa_weighted,
    input$nn.k,
    input$nn_useDD,
    input$nn.factor,
    input$nn.constant,
    input$nn_method,
    input$in_metric.select,
    input$nn.scale
    ), {
    validate(need(input$in_test_site_select!="None","Select a test site from the sidebar"))
    validate(need(!is.null(nn.sites$data),""))
    validate(need(input$nn_method!="User Selected","User selected reference sites not yet implimented")) # Fix this once user selected ref sites implimented
    validate(need(!is.null(input$in_metric.select),"Select Metrics"))
    temp<-all.data$data
    output2<-all.data$data
    output2[,c("TSA Impairment",
               "Interval Test",
               "Equivalence Test",
               "Randomization p value",
               "Test Site D2",
               "Lower Critical",
               "Upper Critical",
               "TSA Lambda",
               "TSA F Value"
    )]<-NA
    output2<-output2[-c(1:ncol(all.data$data))]
    
    test.site<-input$in_test_site_select
    if (input$nn_method=="ANNA"){
      validate(need(!is.null(additional.metrics$data),""))
      Test<-temp[test.site,colnames(temp)%in%colnames(bio.data$data$Summary.Metrics)]
      
      add.met.test<-data.frame(additional.metrics$data[which(names(additional.metrics$data)%in%test.site)])
      colnames(add.met.test)<-c("O:E","Bray-Curtis","CA1","CA2")
      
      Test<-cbind(Test,add.met.test[nrow(add.met.test),])
      
      ref.set<-nn.sites$data$TF.matrix[rownames(Test),]
      Reference<-temp[names(ref.set)[ref.set==T],colnames(temp)%in%colnames(bio.data$data$Summary.Metrics)]
      Reference<-cbind(Reference,add.met.test[1:(nrow(add.met.test)-1),])
    }
    if (input$nn_method=="RDA-ANNA"){
      validate(need(!is.null(additional.metrics$data),""))
      Test<-temp[test.site,input$in_metric.select]
      ref.set<-nn.sites$data$TF.matrix[rownames(Test),]
      Reference<-temp[names(ref.set)[ref.set==T],input$in_metric.select]
    }
    
    if (input$tsa_weighted){
      distance<-nn.sites$data$distance.matrix[rownames(Test),]
      distance<-distance[rownames(Reference)]
    } else {
      distance<-NULL
    }
    
    output1<-try(BenthicAnalysistesting::tsa.test.UI(Test=Test,
                                                     Reference=Reference,
                                                     outlier.rem=input$tsa_outlier_rem,
                                                     outbound=input$tsa_outbound,
                                                     m.select=input$useMD,
                                                     distance=distance
    ), silent=T)
    
    validate(need(class(output1)!="try-error",output1[1]))
    
    output2<-t(output1$tsa.results)
    
    eval(parse(text=paste0("tsa.results$output.list$'",rownames(Test),"'<-output1")))

    #for (i in which(all.data$data[,reftest.ID.cols$data]==0)){
    #  test.site<-rownames(all.data$data)[i]
    #  if (input$nn_method=="ANNA"){
    #    validate(need(!is.null(additional.metrics$data),""))
    #    Test<-temp[i,colnames(temp)%in%colnames(bio.data$data$Summary.Metrics)]
    #    
    #    add.met.test<-data.frame(additional.metrics$data[which(names(additional.metrics$data)%in%test.site)])
    #    colnames(add.met.test)<-c("O:E","Bray-Curtis","CA1","CA2")
    #    
    #    Test<-cbind(Test,add.met.test[nrow(add.met.test),])
    #    
    #    ref.set<-nn.sites$data$TF.matrix[rownames(Test),]
    #    Reference<-temp[names(ref.set)[ref.set==T],colnames(temp)%in%colnames(bio.data$data$Summary.Metrics)]
    #    Reference<-cbind(Reference,add.met.test[1:(nrow(add.met.test)-1),])
    #  }
    #  if (input$nn_method=="RDA-ANNA"){
    #    validate(need(!is.null(additional.metrics$data),""))
    #    Test<-temp[i,input$in_metric.select]
    #    ref.set<-nn.sites$data$TF.matrix[rownames(Test),]
    #    Reference<-temp[names(ref.set)[ref.set==T],input$in_metric.select]
    #  }
    #  
    #  if (input$tsa_weighted){
    #    distance<-nn.sites$data$distance.matrix[rownames(Test),]
    #    distance<-distance[rownames(Reference)]
    #  } else {
    #    distance<-NULL
    #  }
    #  
    #  output1<-try(BenthicAnalysistesting::tsa.test.UI(Test=Test,
    #                                               Reference=Reference,
    #                                               outlier.rem=input$tsa_outlier_rem,
    #                                               outbound=input$tsa_outbound,
    #                                               m.select=input$useMD,
    #                                               distance=distance
    #  ), silent=T)
    #  
    #  validate(need(class(output1)!="try-error",output1[1]))
    #  
    #  output2[i,]<-t(output1$tsa.results)
    #  
    #  eval(parse(text=paste0("tsa.results$output.list$'",rownames(Test),"'<-output1")))
    #}
    tsa.results$data<-output2
  })
  
  #########################################################
  #TSA Plots
  #########################################################
  
  output$tsa.result.printed<-renderUI({
    validate(need(!is.null(tsa.results$data),""))
    
    if (input$in_test_site_select!="None"){
      tsa.object<-tsa.results$output.list[which(names(tsa.results$output.list)%in%input$in_test_site_select)]
      tsa.object<-tsa.object[[1]]
      
      if (tsa.object$tsa.results["TSA Impairment",]=="Not Impaired"){
        status="primary"
        stat.col<-"blue"
      }
      if (tsa.object$tsa.results["TSA Impairment",]=="Possibly Impaired"){
        status="warning"
        stat.col<-"orange"
      }
      if (tsa.object$tsa.results["TSA Impairment",]=="Impaired"){
        status="danger"
        stat.col<-"red"
      }
      
      box(width=10,title=h2(tsa.object$test.site),status=status,
          infoBox(value=tsa.object$tsa.results["TSA Impairment",],title="Status",width=10, color=stat.col,icon=icon("heartbeat",lib="font-awesome"),fill=T),
          fluidRow(valueBox(tsa.object$tsa.results["Test Site D2",],"Test site D2",width=4, color="light-blue",icon=icon("line-chart",lib="font-awesome")),
                   valueBox(tsa.object$jacknife["Jacknife Consistency",],"Jacknife Consistency",width=4, color="light-blue",icon=icon("refresh",lib="font-awesome")),
                   valueBox(tsa.object$general.results["Number of Metrics",],"Number of Metrics",width=4, color="light-blue",icon=icon("hashtag",lib="font-awesome"))
          ),
          fluidRow(valueBox(tsa.object$general.results["Number of Reference Sites",],"Reference samples",width=4, color="light-blue",icon=icon("hashtag",lib="font-awesome")),
                   valueBox(tsa.object$tsa.results["Interval Test",],"Interval Test",width=4, color="light-blue",icon=icon("hashtag",lib="font-awesome")),
                   valueBox(tsa.object$tsa.results["Equivalence Test",],"Equivalence Test",width=4, color="light-blue",icon=icon("hashtag",lib="font-awesome"))
          )
      )
    }
    
  })
  
  output$tsa.distance.plot<-renderPlot({
    validate(need(!is.null(tsa.results$data),""))

    if (input$in_test_site_select!="None"){
      tsa.object<-tsa.results$output.list[which(names(tsa.results$output.list)%in%input$in_test_site_select)]
      tsa.object<-tsa.object[[1]]
      
      tsa.dist<-tsa.object$mahalanobis.distance
      nInd<-as.numeric(tsa.object$general.results["Number of Metrics",])
      nRef<-as.numeric(tsa.object$general.results["Number of Reference Sites",])
      tsa.lambda<-as.numeric(tsa.object$tsa.results["TSA Lambda",])
      test.site<-tsa.object$general.results["Test Site",]
      
      d1<-density(tsa.dist[1:(length(tsa.dist)-1)])
      d2<-density(((nInd*(nRef-1))*rf(1000000, df1=nInd, df2=(nRef-nInd), ncp=tsa.lambda))/((nRef-nInd)*nRef))
      
      plot(d1,main=paste0(test.site),yaxt="n",xlab="Mahalanobis Distance",ylab="",xlim=c(-1,(max(tsa.dist)+3)))
      polygon(d1,col="grey80")
      lines(d2,lty=2,cex=2,col="grey70")
      abline(v=((nInd*(nRef-1))*qf(0.95, df1=nInd, df2=(nRef-nInd), ncp=tsa.lambda, log=FALSE)/((nRef-nInd)*nRef)), lty=2, col='red')
      abline(v=((nInd*(nRef-1))*qf(0.05, df1=nInd, df2=(nRef-nInd), ncp=tsa.lambda, log=FALSE)/((nRef-nInd)*nRef)), lty=2, col='orange')
      points(tsa.dist[length(tsa.dist)],0, pch="*",col='black',cex=2,lwd=2)
      if (any(names(tsa.object)=="jacknife")) {
        segments(x0=tsa.object$jacknife[2,],y0=0.05,x1=tsa.object$jacknife[3,],y1=0.05,col="black",lwd=2)
        text(tsa.object$jacknife[2,],0.05,labels=paste0("Jacknife Consistency ",substr(tsa.object$jacknife[1,],1,3),"%"),pos=3, offset=0.5,cex=0.70,col='black')
      }
      
      text(tsa.dist[length(tsa.dist)],0, labels="test-site",pos=3, offset=0.5,cex=1,col='black')
      
    }
  })
  
  output$tsa.metric.plot<-renderPlot({
    validate(need(!is.null(tsa.results$data),""))
    
    if (input$in_test_site_select!="None"){
      tsa.object<-tsa.results$output.list[which(names(tsa.results$output.list)%in%input$in_test_site_select)]
      tsa.object<-tsa.object[[1]]
      
      tsa.stand<-tsa.object$z.scores
      nInd<-ncol(tsa.stand)
      nRef<-nrow(tsa.stand)-1
      
      part.tsa<-if (!is.null(tsa.object$partial.tsa)) {tsa.object$partial.tsa} else {NULL}
      all.met<-colnames(tsa.stand)
      sel.met<-unlist(strsplit(substr(tsa.object$general.results["Selected Indicator Metrics",],1,(nchar(tsa.object$general.results["Selected Indicator Metrics",])-2)),split=", "))
      
      melt.ref<-reshape2::melt(tsa.stand[1:nRef,])
      p1<-ggplot(data=melt.ref) + theme_bw() +
        geom_boxplot(data=melt.ref, aes(x=Var2,y=value, fill=Var2), width=0.5) +
        labs(title=paste0(input$in_test_site_select," Indicator metrics")) +
        theme(axis.text.x = element_text(angle = 65, hjust = 1), legend.position="none", strip.text.x = element_blank()) +
        ylim(min(tsa.stand)*1.3, max(tsa.stand)*1.1) +
        xlab("") + 
        ylab("") +
        geom_point(data=data.frame(x=levels(melt.ref$Var2)[which(colnames(tsa.stand)%in%sel.met)],y=tsa.stand[(nRef+1),sel.met]), aes(x=x,y=y), shape=1,fill="transparent",size=4) +
        geom_point(data=data.frame(x=levels(melt.ref$Var2),y=tsa.stand[(nRef+1),]), aes(x=x,y=y), col="red")
        
        
      if (any(part.tsa$p<0.05)) {
        p1 <- p1 +geom_point(data=data.frame(x=levels(melt.ref$Var2)[which(colnames(tsa.stand)%in%rownames(part.tsa)[part.tsa$p<0.05])],y=rep(min(tsa.stand)*1.2,length(rownames(part.tsa)[part.tsa$p<0.05]))), aes(x=x,y=y), shape=8,size=2)
      }
       p1
    }
  })
  
  output$tsa.pcoa.plot<-renderPlot({
    validate(need(!is.null(tsa.results$data),""))
    
    if (input$in_test_site_select!="None"){
      tsa.object<-tsa.results$output.list[which(names(tsa.results$output.list)%in%input$in_test_site_select)]
      tsa.object<-tsa.object[[1]]
      
      vectors=T
      mets<-tsa.object$z.scores[,colnames(tsa.object$raw.data)]
      nInd<-ncol(mets)
      nRef<-nrow(mets)-1
      refsites<-c(rep(1,nRef),0)
      
      plot1<-vegan::capscale(BenthicAnalysistesting::D2.dist(mets,(cov(mets[1:nRef,])),inverted=F)~1,add=F,sqrt.dist=F)
      fig<-vegan::ordiplot(plot1,type="n",main=paste(rownames(mets[max(nrow(mets)),])," PCoA Plot",sep=""),
                           xlab=paste("MDS ",substr((eigenvals(plot1)[1]/sum(eigenvals(plot1)))*100,1,4),"%"),
                           ylab=paste("MDS ",substr((eigenvals(plot1)[2]/sum(eigenvals(plot1)))*100,1,4),"%"))
      points(fig,what="sites",cex=0.8,select=refsites==1,col="black",pch=19)
      points(fig,what="sites",cex=0.8,select=refsites==0,col="red",pch=19)
      suppressWarnings(vegan::ordiellipse(plot1,refsites,kind="sd",conf=0.95,draw="line",col="grey20",lty=5,show.groups=1))
      text(fig,what="sites",select=refsites==1,col="black",cex=0.8,pos=3)
      text(fig,what="sites",select=refsites==0,col="red",cex=0.9,pos=3)
      if (vectors==T) {
        plot(vegan::envfit(plot1,mets,display="sites",na.rm=F,permutations=0),cex=0.8,col="orange")
      }

    }
  })
  
  output$tsa.ca.plot<-renderPlot({
    validate(need(!is.null(tsa.results$data),""))
    
    if (input$in_test_site_select!="None"){
      temp<-all.data$data
      colnames(temp)<-gsub(".",";",colnames(temp),fixed = T)
      
      Test<-temp[input$in_test_site_select,colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
      ref.set<-nn.sites$data$TF.matrix[rownames(Test),]
      Reference<-temp[names(ref.set)[ref.set==T],colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
      nRef<-nrow(Reference)
      raw.data<-rbind(Reference,Test)
      pRef<-colSums(decostand(Reference,"pa"))/nrow(Reference)
      
      ca.ord<-vegan::cca(log(raw.data[,names(which(pRef>=0.1))]+1))
      ca1<-ca.ord$CA$u[,1]
      ca2<-ca.ord$CA$u[,2]
      
      plot(ca.ord,type="n",main=paste(rownames(ca1)[(nRef+1)]," CA Plot",sep=""),
           xlab=paste("CA1 ",substr((eigenvals(ca.ord)[1]/sum(eigenvals(ca.ord)))*100,1,4),"%"),
           ylab=paste("CA2 ",substr((eigenvals(ca.ord)[2]/sum(eigenvals(ca.ord)))*100,1,4),"%"))
      text(x=ca.ord$CA$v[,1],y=ca.ord$CA$v[,2],labels=rownames(ca.ord$CA$v),col="grey50",cex=0.7)
      points(x=ca1[1:nRef],y=ca2[1:nRef],cex=0.8,col="black",pch=19)
      points(x=ca1[(nRef+1)],y=ca2[(nRef+1)],cex=0.8,col="red",pch=19)
      text(x=ca1[1:nRef],y=ca2[1:nRef],labels=names(ca1)[1:nRef],col="black",cex=0.8,pos=3)
      text(x=ca1[(nRef+1)],y=ca2[(nRef+1)],labels=names(ca1)[(nRef+1)],col="red",cex=0.9,pos=3)
      suppressWarnings(vegan::ordiellipse(ca.ord,c(rep(1,nrow(Reference)),0),kind="sd",conf=0.95,draw="line",col="grey20",lty=5,show.groups=1))
      }
  })
  
  
  
  #########################################################
  #Mapping
  #########################################################

  output$map_pointcolselect_out<-renderUI({
    validate(
      need(input$map_pointcolgroup!="None","")
    )
    vars<-NULL
    if (input$map_pointcolgroup=="Habitat"){
      validate(
        need(!is.null(habitat.by.site$data),"")
      )
      vars<-colnames(habitat.by.site$data)
    }
    if (input$map_pointcolgroup=="Taxa"){
      validate(
        need(!is.null(taxa.by.site$data),"")
      )
      vars<-colnames(taxa.by.site$data)
    }
    if (input$map_pointcolgroup=="Metrics"){
      validate(
        need(!is.null(bio.data$data$Summary.Metrics),"")
      )
      vars<-colnames(bio.data$data$Summary.Metrics)
    }
    selectInput("map_pointcolselect_in",label="Attribute",choices=vars)
  })
  
  output$out.map_chart_variables<-renderUI({
    #validate(
    #  need(!is.null(all.data$data),"")
    #)
    selectInput("in.map_chart_variables",label="Chart Variables",choices=list(
      Taxa=colnames(all.data$data)[colnames(all.data$data)%in%colnames(taxa.by.site$data.alt.colnames)],
      Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
      Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
      Feeding_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(feeding.data$data.reduced)],
      Habitat_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.data$data)]
    ),multiple = TRUE)
  })
  
  map_icons<-reactiveValues(data=NULL)
  
  observe({
    map_icons$data<-NULL
  })
  
  
  output$mymap <- renderLeaflet({
    validate(
      need(!is.null(coordinates.by.site$data.unique),"")
    )
    m <- leaflet()
    if (input$basemap_input=="Street"){
      #m <- addTiles(m)
      m<-addProviderTiles(map=m,
                          provider=providers$Esri.WorldTopoMap)
    }
    if (input$basemap_input=="Satellite"){
      m<-addProviderTiles(map=m,
                          provider=providers$Esri.WorldImagery)
      
      #m <- addTiles(m,urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google')
    }
    if (input$map_admin==T){
      m<-addProviderTiles(map=m,
                          provider=providers$OpenMapSurfer.AdminBounds)
    }
    
    if (input$map_pointtype=="pie"|input$map_pointtype=="bar"){
      m<-addMinicharts(
        map = m,
        lng=all.data$data$east,
        lat=all.data$data$north,
        layerId=all.data$data[,colnames(all.data$data)%in%site.ID.cols$data & !colnames(all.data$data)%in%input$time.ID],
        width = input$map_chart_site,
        height = input$map_chart_site,
        #maxValues<-aggregate(feeding.data$data.reduced,by=list(coordinates.by.site$data.all[,input$time.ID]),max),
        type=input$map_pointtype,
        chartdata=all.data$data[,colnames(all.data$data)%in%input$in.map_chart_variables],
        time=all.data$data[,input$time.ID],
        showLabels = T,
        legendPosition = "bottomleft"#,
        #colorPalette=colorRamps::primary.colors(ncol(data[colnames(data)%in%colnames(feeding.data$data.reduced)]))
      )
    }
    
    if (input$map_pointtype=="Points"){
      m <- addMarkers(map=m,
                      lng=all.data$data$east,
                      lat=all.data$data$north,
                      label=rownames(coordinates.by.site$data.unique)
      )
    }
    
    m
    })
  
  #########################################################
  #    Map markers
  #########################################################
  
  #observe({
  #  if (input$map_pointtype=="pie"|input$map_pointtype=="bar") {
  #    data <- data.frame(cbind(coordinates.by.site$data.all,feeding.data$data.reduced))
  #    data <- data.frame(merge(data,missing.sampling.events$full.data,all=T))
  ##    
  #    leafletProxy("mymap", session) %>%
  #      updateMinicharts(
  #        layerId=data[,(colnames(coordinates.by.site$data.all)%in%site.ID.cols$data & !colnames(coordinates.by.site$data.all)%in%input$time.ID)],
  #        chartdata = data[,colnames(data)%in%colnames(feeding.data$data.reduced)],
  #        #maxValues = maxValue,
  #        time = data[,colnames(data)%in%input$time.ID],
  ##        type = "pie",
  #        showLabels = T,
  #        legendPosition = "bottomleft"
  #      )
      
  #  } 
    
    #maxValue <- max(as.matrix(data))
    
  #})
  #pie.charts <- pie(, data = meuse@data)
  #p <- mget(rep("p", length(meuse)))
  
  #pie.charts <- lapply(1:length(p), function(i) {
  #  clr[i] <- "red"
  #  update(p[[i]], col = clr)
  #})
  
  
  #########################################################
  #Hold
  #########################################################
  output$testout1<-renderPrint({
    list(
      Taxa=colnames(all.data$data)[colnames(all.data$data)%in%colnames(taxa.by.site$data.alt.colnames)],
      Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
      Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
      Feeding_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(feeding.data$data.reduced)],
      Habitat_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.data$data)]#,
      #Test=data.frame(t(nn.sites$data$TF.matrix[rownames(nn.sites$data$TF.matrix)%in%input$in_test.site.select,]))
    )
    #all.data$data
  })
  
  output$testout3<-renderPrint({
    colnames(taxa.by.site$data.alt.colnames)
  })
  output$testout4<-renderPrint({
    colnames(all.data$data)
  })
  
  
  output$testout2<-renderDataTable({
    DT::datatable(all.data$data[,], options=list(pageLength = 10,scrollX=T))
  })
  
  #########################################################
  #Help Texts - raw data input
  #########################################################
  
  observeEvent(input$Date_field.help,{
    showModal(modalDialog(
      size="s",
      helpText("One column assigned to 'Site/Sampling Event' may be used to indicate the temporal sequence of sampling events. This field should be numeric."),
      footer = modalButton("Dismiss"),
      easyClose = TRUE
    )
    )
  })
  
  #Raw Data input
  observeEvent(input$raw.help, {
    showModal(modalDialog(
      size="l",
      title = "Upload and define Raw Data",
      hr(),
      helpText("A single input file is used for all data. Below are examples of different input data structures."),
      hr(),
      fluidRow(
        box(title=h4("Examples"),width=12, collapsible = T, collapsed = T, status="success",solidHeader = T,
            tabBox(width = 12,
                   tabPanel("Long Format",
                            downloadLink(outputId="download_ex_long",label="Download"),
                            hr(),
                            renderDataTable({DT::datatable(read.csv("Long_example.csv",header=F),options=list(pageLength = 5,scrollX=T,searching = FALSE))})
                            ),
                   tabPanel("Wide Format (1 row)",
                            downloadLink(outputId="download_ex_wide1",label="Download"),
                            hr(),
                            renderDataTable({DT::datatable(read.csv("Wide_example1.csv",header=F),options=list(pageLength = 5,scrollX=T,searching = FALSE))})
                            ),
                   tabPanel("Wide Format (multi row)",
                            downloadLink(outputId="download_ex_wide2",label="Download"),
                            hr(),
                            renderDataTable({DT::datatable(read.csv("Wide_example2.csv",header=F),options=list(pageLength = 5,scrollX=T,searching = FALSE))})
                   )
            )
        )
      ),
      hr(),
      fluidRow(
        h4("Select your input file here"),
        br(),
        renderImage({
          return(list(
            src = "Rawdata_help1.png",
            filetype = "image/png",
            height = 400,
            width = 500
          ))
        }, deleteFile = FALSE),
        br()
      ),
      hr(),
      fluidRow(
        h4("Select the format of your input data"),
        br(),
        renderImage({
          return(list(
            src = "Rawdata_help2.png",
            filetype = "image/png",
            height = 400,
            width = 500
          ))
        }, deleteFile = FALSE),
        br()
      ),
      hr(),
      fluidRow(
        h4("Highlight column(s) (multiple while holding shift) and assign them to the appropriate attributes"),
        br(),
        renderImage({
          return(list(
            src = "Rawdata_help3.png",
            filetype = "image/png",
            height = 400,
            width = 600
          ))
        }, deleteFile = FALSE),
        renderImage({
          return(list(
            src = "Rawdata_help4.png",
            filetype = "image/png",
            height = 400,
            width = 600
          ))
        }, deleteFile = FALSE),
        br()
      ),
      hr(),
      fluidRow(
        h4("Some attributes (i.e. Coordinates) are selected from dropdown menus (1)"),
        h4("Once all fields have been selected, press the Finalize Coordinates button (2)"),
        br(),
        renderImage({
          return(list(
            src = "Rawdata_help5.png",
            filetype = "image/png",
            height = 400,
            width = 500
          ))
        }, deleteFile = FALSE),
        br()
      ),
      hr(),
      fluidRow(
        h4("Once the minimum necessary columns have been assigned, the Finalize button will appear. Pressing it will lock in the column assignments (1)."),
        h4("The data can then be viewed though the tabs at the top (2)"),
        br(),
        renderImage({
          return(list(
            src = "Rawdata_help6.png",
            filetype = "image/png",
            height = 400,
            width = 500
          ))
        }, deleteFile = FALSE),
        br()
      ),
      hr(),
      fluidRow(
        h4("To calculate indicator metrics, press the Taxa tab (1)."),
        h4("Review the input taxa data and press 'Calculate Summary Metrics' (2)"),
        h4("Calculate may take a few minutes with large datasets. Calculated summary metrics can be viewed in the table below (3)"),
        br(),
        renderImage({
          return(list(
            src = "Rawdata_help7.png",
            filetype = "image/png",
            height = 400,
            width = 500
          ))
        }, deleteFile = FALSE),
        br()
      ),
      hr(),
      
      footer = modalButton("Dismiss"),
      easyClose = TRUE
    ))
  })
  
  output$download_ex_long<-downloadHandler(filename = function() { paste("Long_example.csv") },
                                            content = function(file) {write.csv(read.csv("Long_example.csv",header=T),file,row.names = F)})
  output$download_ex_wide1<-downloadHandler(filename = function() { paste("Wide_example1.csv") },
                                           content = function(file) {write.csv(read.csv("Wide_example1.csv",header=T),file,row.names = F)})
  output$download_ex_wide2<-downloadHandler(filename = function() { paste("Wide_example2.csv") },
                                           content = function(file) {write.csv(read.csv("Wide_example2.csv",header=T),file,row.names = F)})
  
  
  #Raw Data input
  
  
  #########################################################
  #Server End
  ########################################################
  
})
