#Note to future self. If you change HBI or taxa traits, need to update the data files here as well
#

library(BenthicAnalysistesting)
library(shinyjs)
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(sp)
library(rgdal)
library(leaflet.minicharts)
library(mapview)
library(colorRamps)
library(plyr)
library(dplyr)
library(ggplot2)
library(vegan)
library(reshape2)
#library(purrr)    # map functions (like lapply)
#library(lazyeval) # interp function
#library(tidyr)
library(RColorBrewer)
library(tibble)
library(fmsb)
library(sf)
library(googlesheets)
library(googledrive)
library(gsheet)
library(classInt)
library(corrplot)
#library(shinyDND)


options(shiny.maxRequestSize=30*1024^2)

#gap<-gs_title("AEAtool_IDs")
#data.frame(gs_read(gap))
#gs_ws_ls()
#(GAP_URL <- gs_gap_url())
#userIDs<-as.data.frame(gs_read(gs_url("https://docs.google.com/spreadsheets/d/e/2PACX-1vR1yZS2cHiV6Pcy9Ea8MXBUo-tIw3jUGf4241TyUh3URo4L1osSv6WfVH7Z_Xou1-rpwa9WWTOgNZYC/pub?gid=142358852&single=true&output=csv")))
#userIDs<-as.data.frame(gs_read(gs_url("https://docs.google.com/spreadsheets/d/1pJvhMR02pdqhRnitFHXUx8HYRr9jWiBuiTwDJ_VIP5M/pub?output=csv")))

#gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1sE5ykRJe2hiUxKnX2NLszil4GsWkBH3pwZEuj8-Ge60/edit?usp=sharing')


shinyServer(function(input, output, session) {
  
  #########################################################
  #Login
  #########################################################
  loggedin<-F
  
  loggedin1<-reactiveValues(data=FALSE)
    
  login.modal<-function(failed=F){
    modalDialog(
      size="s",
      textInput("username","Email Address"),
      passwordInput("password","Login ID"),
      helpText(a("Create Account",target="_blank", href="https://docs.google.com/forms/d/e/1FAIpQLSd4V_kuS-AiyOPcqA8hjVtOlV2VoSXq4Izb0Il4rjE2osq3FA/viewform?usp=sf_link")),
      #helpText(a("Create Account",target="_blank", href="https://docs.google.com/forms/d/e/1FAIpQLSezlBS8yoJCzQONq422c4uys9OdURXzUW1Q2dl3k7_Fwz4uDA/viewform?usp=sf_link")),
      helpText("For help email: ecopulseanalytics@gmail.com"),
      footer = actionButton("login","Login"),
      easyClose = F,
      if (failed){
        div(tags$b("Invalid Email Address or Login ID", style = "color: red;"))
      }
    )
  }
  
  if (loggedin==F){
    showModal(login.modal())
  }
  

  observeEvent(input$login, {
    userIDs<-as.data.frame(gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1sE5ykRJe2hiUxKnX2NLszil4GsWkBH3pwZEuj8-Ge60/edit?usp=sharing'))
    
    #userIDs<-as.data.frame(gs_read(gs_url("https://docs.google.com/spreadsheets/d/1pJvhMR02pdqhRnitFHXUx8HYRr9jWiBuiTwDJ_VIP5M/pub?output=csv")))
    
    # Check that data object exists and is data frame.
    userIDs1<-userIDs[userIDs$`Email Address`==input$username,]
    if ((input$username==userIDs1$`Email Address` && input$password==userIDs1$`Login ID`) && nrow(userIDs1)!=0) {
      removeModal()
      output$loggedin1<-reactive({TRUE})
      outputOptions(output, "loggedin1", suspendWhenHidden = FALSE)
      if (input$username=="ecopulseanalytics@gmail.com") {
        output$adminloggedin<-reactive({TRUE})
        outputOptions(output, "adminloggedin", suspendWhenHidden = FALSE)
      }
      loggedin<-TRUE
      loggedin1$data<-TRUE
    } else {
      showModal(login.modal(failed = TRUE))
      #loggedin<-TRUE
      #loggedin1$data<-TRUE
    }
  })
  

  #########################################################
  #Saving and loading states
  #########################################################
  
  #observeEvent(input$savestate, {
  #  saveRDS( reactiveValuesToList(input) , file = 'inputs.RDS')
  #  #save.image(file="savetest.RData")
  #})
  
  #observeEvent(input$loadstate, {
  #  if(!file.exists('inputs.RDS')) {return(NULL)}
  #  
  #  savedInputs <- readRDS('inputs.RDS')
  #  
  #  inputIDs      <- names(savedInputs) 
  #  inputvalues   <- unlist(savedInputs) 
  #  for (i in 1:length(savedInputs)) { 
  #    session$sendInputMessage(inputIDs[i],  list(value=inputvalues[[i]]) )
  #  }
  #  #load("savetest.RData",envir = .GlobalEnv)
  #})
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
    taxa.by.site$data.alt.colnames<-NULL
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
    all.data$data<-NULL
    additional.metrics_b$data=NULL
    additional.metrics_b$output.list=NULL
    tsa.results_b$data=NULL
    tsa.results_b$output.list=NULL
    additional.metrics$data=NULL
    additional.metrics$output.list=NULL
    tsa.results$data=NULL
    tsa.results$output.list=NULL
    tsa.batch.outout$data=NULL
    nn.sites$data=NULL
    missing.sampling.events$full.data=NULL
    missing.sampling.events$rnames=NULL
    reftest.by.site$data=NULL
    coordinates.by.site$data.all=NULL
    coordinates.by.site$data.unique=NULL
    coordinates.by.site$gis.site.id=NULL
    feeding.data$data.reduced<-NULL
    habitat.data$data<-NULL
  })
  
  output$rawDataView<-renderDataTable({#Renders raw data table
    validate(need(!is.null(raw.bio.data$data),"Select an input file from the sidebar"))
    DT::datatable(raw.bio.data$data, options=list(pageLength = 5,scrollX=T))
  })
  
  output$show_rawdatabox<-reactive({
    if(!is.null(raw.bio.data$data)){
      TRUE
    } else {
      FALSE
    }
  })
  outputOptions(output, 'show_rawdatabox', suspendWhenHidden=FALSE)
  
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
    selectInput(inputId="raw.testrefcols", label=h5('Test(0) or Reference(1) Site'), multiple = F,selectize=T,selected = "",
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
  #    User Matched Reference Sites
  #########################################################
  
  userMatchRefSites<-reactiveValues(raw.data=NULL,TFmatrix=NULL)
  
  observeEvent(input$inUserMatchRefFile,{
    userMatchRefSites$raw.data<-read.csv(input$inUserMatchRefFile$datapath,strip.white=TRUE, header=T)
    userMatchRefSites$raw.data<-data.frame(apply(userMatchRefSites$raw.data,2,as.character),stringsAsFactors=FALSE)
    
    if (is.null(reftest.by.site$data)) {
      shinyjs::reset('inUserMatchRefFile')
      validate(need(F,"Must specify Reference and Test samples in input file"))
    }

    temp.tfMatrix<-data.frame(matrix(nrow=sum(reftest.by.site$data[,1]==0),ncol=sum(reftest.by.site$data[,1]==1)))
    rownames(temp.tfMatrix)<-rownames(reftest.by.site$data)[reftest.by.site$data[,1]==0]
    colnames(temp.tfMatrix)<-rownames(reftest.by.site$data)[reftest.by.site$data[,1]==1]
    
    raw.site.names<-rownames(reftest.by.site$data)
    user.site.names<-unlist(userMatchRefSites$raw.data)
    user.site.names<-unique(user.site.names)
    
    if (!any(raw.site.names%in%user.site.names)|!any(user.site.names%in%raw.site.names)){
      shinyjs::reset('inUserMatchRefFile')
      validate(need(F,"Sample name mismatch between Raw Data User Site Match"))
    }
    
    for (i in rownames(temp.tfMatrix)){
      temp<-paste0(userMatchRefSites$raw.data[userMatchRefSites$raw.data[,1]%in%i,-c(1)])
      temp<-temp[temp!=""]
      
      temp.tfMatrix[i,colnames(temp.tfMatrix)%in%temp]<-TRUE
      temp.tfMatrix[i,!colnames(temp.tfMatrix)%in%temp]<-FALSE
      rm(temp)
    }
    userMatchRefSites$TFmatrix<-temp.tfMatrix
  })
  output$out_usermatch.refsites<-renderDataTable({
    validate(need(!is.null(reftest.by.site$data),"Must specify Reference and Test samples in input file"))
    validate(need(!is.null(userMatchRefSites$TFmatrix),"Select a file to upload from the sidebar"))
    DT::datatable(userMatchRefSites$TFmatrix,options=list(pageLength = 10,scrollX=T))
  })
  output$out_usermatch.refsites1<-renderDataTable({
    validate(need(!is.null(reftest.by.site$data),"Must specify Reference and Test samples in input file"))
    validate(need(!is.null(userMatchRefSites$TFmatrix),"No user matched reference sites specified. See Data Input -> User Matched Reference Sites"))
    DT::datatable(userMatchRefSites$TFmatrix,options=list(pageLength = 10,scrollX=T))
  })
  
  
  #########################################################
  #    When Raw Data are finalized
  #########################################################
  
  passed.validation<-reactiveValues(pass=TRUE)
  taxa.by.site<-reactiveValues(data=NULL,data.alt.colnames=NULL) #calculate taxa by site table
  observeEvent(input$finalize_raw,{
    isolate(
      if (input$rawFormat=="Wide"){
        if (length(site.ID.cols$data)==1){
          site.names<-as.vector(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data])
        } else {
          site.names<-apply(raw.bio.data$data[-c(1:max(raw.data.rows(),1)),raw.colnames()%in%site.ID.cols$data],1,paste0,collapse="",sep=";")
          site.names<-substr(site.names,start=1,stop=(nchar(site.names)-1))
        }
        if(any(duplicated(site.names))){
          passed.validation$pass<-FALSE
          showModal(modalDialog(
            title = "Error",
            "Duplicate sampling events are not permitted",
            easyClose = T
          ))
        } else {
          output<-raw.bio.data$data[max(raw.data.rows()+1,2):nrow(raw.bio.data$data),raw.colnames()%in%taxa.ID.cols$data]
          output<-data.frame(apply(output,2,as.numeric))
          rownames(output)<-site.names
          colnames(output)<-taxa.ID.cols$data
          output<-output[site.names[!duplicated(site.names)],]
        }
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
    
    flag1<-any(grepl(" ",colnames(output),fixed = T))
    flag2<-any(grepl(".",colnames(output),fixed = T))
    flag3<-any(grepl(",",colnames(output),fixed = T))
    flag4<-any(grepl("sp.",colnames(output),fixed = T))
    flag5<-any(grepl("gr.",colnames(output),fixed = T))
    flag6<-any(grepl("group",colnames(output),fixed = T))
    flag7<-any(grepl("complex",colnames(output),fixed = T))
    flag8<-any(grepl("(",colnames(output),fixed = T))
    flag9<-any(grepl(")",colnames(output),fixed = T))
    flag10<-any(grepl("/",colnames(output),fixed = T))
    flag11<-any(grepl("spp.",colnames(output),fixed = T))
    flag12<-any(grepl("1|2|3|4|5|6|7|8|9|0",colnames(output),fixed = F))
    
    if (any(flag1,flag2,flag3,flag4,flag5,flag6,flag7,flag8,flag9,flag10,flag11,flag12)){
      passed.validation$pass<-FALSE
      showModal(modalDialog(
        title = "Error",
        "Taxa names cannot contain: duplicates, numbers, spaces, punctuation, sp. spp, gr., group, complex, etc.",
        easyClose = T
      ))
    } 
    
    flag13<-any(is.na(output))
    if (any(flag13)){
      passed.validation$pass<-FALSE
      showModal(modalDialog(
        title = "Error",
        "Wide datasets cannot contain blank cells or NA cells",
        easyClose = T
      ))
    } 

    flag14<-any(colSums(output,na.rm=T)==0)
    if (any(flag14)){
      passed.validation$pass<-FALSE
      showModal(modalDialog(
        title = "Error",
        "Dataset cannot contain taxa with no occurances",
        easyClose = T
      ))
    } 
    
    
    validate(
      need(passed.validation$pass,"Data must pass validation")
    )
    
    output<-do.call(data.frame,lapply(output, function(x) type.convert(as.character(x))))
    rownames(output)<-site.names[!duplicated(site.names)]
    colnames(output)<-gsub(".",";",colnames(output),fixed=T)
    taxa.by.site$data<-output
    
    taxa.by.site$data.alt.colnames<-output
    colnames(taxa.by.site$data.alt.colnames)<-gsub(";",".",colnames(output),fixed=T)
  })
  
  output$rawfinalized1<-reactive({
    if(!is.null(taxa.by.site$data)){
      TRUE
    } else {
      FALSE
    }
  })
  outputOptions(output, "rawfinalized1", suspendWhenHidden = FALSE)
  
  output$view.taxa<-renderDataTable({#Renders raw data table
    DT::datatable(taxa.by.site$data,
                  options=list(pageLength = 5,scrollX=T))
  })
  
  missing.sampling.events<-reactiveValues(full.data=NULL,rnames=NULL) #if a time field is specified, find missing sampling events
  observeEvent(input$finalize_raw,{
    validate(
      need(passed.validation$pass,"Data must pass validation")
    )
    validate(
      need(input$time.ID!="","")
    )
    isolate(
      if (input$time.ID!="") {
        orig.ID<-rownames(taxa.by.site$data)
        orig.ID<-data.frame(do.call(rbind,strsplit(as.character(orig.ID),";")))
        colnames(orig.ID)<-site.ID.cols$data
        orig.ID[,input$time.ID]<-as.numeric(as.character(orig.ID[,input$time.ID]))
        
        non.time.ID<-colwise(as.factor)(as.data.frame(orig.ID[,!colnames(orig.ID)%in%input$time.ID]))
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
    validate(
      need(passed.validation$pass,"Data must pass validation")
    )
    
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
    validate(
      need(passed.validation$pass,"Data must pass validation")
    )
    
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
    validate(
      need(passed.validation$pass,"Data must pass validation")
    )
    
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
            output<-data.frame(output[!duplicated(site.names),])
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
    validate(
      need(passed.validation$pass,"Data must pass validation")
    )
    
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
    validate(
      need(passed.validation$pass,"Data must pass validation")
    )
    
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
    validate(
      need(passed.validation$pass,"Data must pass validation")
    )
    
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
  output$download_raw_mets<-downloadHandler(filename = function() { paste("Metrics-",input$inrawbioFile[1], sep='') },
                                            content = function(file) {write.csv(bio.data$data$Summary.Metrics,file,row.names = T)})
  output$download_raw_taxa<-downloadHandler(filename = function() { paste("Taxa-",input$inrawbioFile[1], sep='') },
                                            content = function(file) {write.csv(taxa.by.site$data,file,row.names = T)})
  output$download_taxa_atts<-downloadHandler(filename = function() { paste("Attributes-",input$inrawbioFile[1], sep='') },
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
        bio.data$data$Summary.Metrics<-bio.data$data$Summary.Metrics[,!colnames(bio.data$data$Summary.Metrics)%in%input$mets_for_trans_in]
        bio.data$data$transformations$Transformation[bio.data$data$transformations$Metric%in%input$mets_for_trans_in]<-"Deleted"
      } else {
        bio.data$data$Summary.Metrics[,colnames(bio.data$data$Summary.Metrics)%in%input$mets_for_trans_in]<-as.numeric(trans.metric())
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
        bio.data$data$Summary.Metrics<-bio.data$data$Summary.Metrics[,!colnames(bio.data$data$Summary.Metrics)%in%batch.met.trans()]
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
        bio.data$data$Summary.Metrics[,colnames(bio.data$data$Summary.Metrics)%in%i]<-t.metric
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
  
  all.data<-reactiveValues(data=NULL,untransformed=NULL)
  
  observeEvent(
    c(input$finalize_raw,
      input$calculate_metrics,
      input$apply.trans,
      input$apply.trans.batch,
      input$habitat_convert_fact_to_numb,
      input$habitat_convert_numb_to_fact
      #input$in_test_site_select,
      #input$nn.k,
      #input$nn_useDD,
      #input$nn.factor,
      #input$nn.constant,
      #input$nn_method,
      #input$in_metric.select,
      #input$tsa_outlier_rem,
      #input$tsa_outbound,
      #input$useMD,
      #input$tsa_weighted
    )
  ,{
    validate(
      need(passed.validation$pass,"Data must pass validation")
    )

    all.data$data<-taxa.by.site$data.alt.colnames
    all.data$untransformed<-taxa.by.site$data.alt.colnames

    if (!is.null(site.ID.cols$data)){
      all.data$data<-cbind(do.call(rbind,strsplit(rownames(all.data$data),";")),all.data$data)
      colnames(all.data$data)[1:length(site.ID.cols$data)]<-site.ID.cols$data
      
      all.data$untransformed<-cbind(do.call(rbind,strsplit(rownames(all.data$untransformed),";")),all.data$data)
      colnames(all.data$untransformed)[1:length(site.ID.cols$data)]<-site.ID.cols$data
    }
    
    if(input$metdata==F & !is.null(bio.data$data$Summary.Metrics)){
      all.data$data<-data.frame(cbind(all.data$data,bio.data$data$Summary.Metrics,feeding.data$data.reduced,habitat.data$data))
      
      all.data$untransformed<-data.frame(cbind(all.data$untransformed,bio.data$data$untransformed.metrics,feeding.data$data.reduced,habitat.data$data))
      
    }
    
    if(input$metdata==T & !is.null(bio.data$data$Summary.Metrics)){
      all.data$data<-data.frame(cbind(all.data$data,bio.data$data$Summary.Metrics))
      
      all.data$untransformed<-data.frame(cbind(all.data$untransformed,bio.data$data$Summary.Metrics))
    }
    
    if(!is.null(habitat.by.site$data)){
      all.data$data<-data.frame(cbind(all.data$data,habitat.by.site$data))
      
      all.data$untransformed<-data.frame(cbind(all.data$untransformed,habitat.by.site$data))
    }
    
    if(!is.null(coordinates.by.site$data.all)){
      all.data$data <- data.frame(cbind(all.data$data,coordinates.by.site$data.all))
      
      all.data$untransformed <- data.frame(cbind(all.data$untransformed,coordinates.by.site$data.all))
    }
    
    if (!is.null(reftest.by.site$data)){
      all.data$data <- data.frame(cbind(all.data$data,reftest.by.site$data))
      
      all.data$untransformed <- data.frame(cbind(all.data$untransformed,reftest.by.site$data))
    }
    
    #if (!is.null(missing.sampling.events$full.data)&!is.null(coordinates.by.site$data.all)){
    #  all.data$data <- data.frame(merge(all.data$data,missing.sampling.events$full.data,all=T))
    #  missing.sites<-as.character(all.data$data[is.na(all.data$data$east),(colnames(all.data$data)%in%site.ID.cols$data & !colnames(all.data$data)%in%input$time.ID)])
    #  all.data$data$east[is.na(all.data$data$east)]<-coordinates.by.site$data.unique$east[match(missing.sites,rownames(coordinates.by.site$data.unique))]
    #  all.data$data$north[is.na(all.data$data$north)]<-coordinates.by.site$data.unique$north[match(missing.sites,rownames(coordinates.by.site$data.unique))]
    #  rownames(all.data$data)<-apply(all.data$data[,site.ID.cols$data], 1 , function(x) paste(x,collapse=";",sep=";"))
    #}
    
    #if (!is.null(tsa.results$data) & class(tsa.results$data)!="try-error"){
    #  all.data$data<-data.frame(cbind(all.data$data,tsa.results$data))
    #}
  })

  
  #########################################################
  #NN Site Matching + Metric Selection
  #########################################################

  output$out_test.site.select<-renderUI({
    validate(need(!is.null(reftest.ID.cols$data),""))
    selectInput("in_test_site_select","",selected="None",
                choices=c("None",rownames(reftest.by.site$data)[reftest.by.site$data==0])
                )
  })
  
  output$out_metric.select<-renderUI({
    validate(need(!is.null(reftest.ID.cols$data) & !is.null(bio.data$data),""))
    
    if((input$nn_method=="ANNA"|input$nn_method=="User Selected") & input$metdata==F){
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
    if (input$nn_method!="User Selected"){
      nn.sites$data<-NULL
      validate(need(!is.null(habitat.by.site$data) & !is.null(reftest.ID.cols$data),"Missing Habitat data or Reference Sites"))
      validate(need(reftest.ID.cols$data!="None","Missing Reference Sites"))
      validate(need(input$nn_method=="RDA-ANNA" | input$nn_method=="ANNA","User matched reference sites not yet implimented"))
      validate(need(!any(is.na(habitat.by.site$data)),"NAs not allowed in habitat data"))
      validate(need(input$nn.k>=3|input$nn_useDD,"Need k>=3 or use Distance-Decay site selection"))
      if (input$nn_method=="RDA-ANNA"){
        validate(need(length(input$in_metric.select)>=3,"Select Metricsat least 3 metrics"))
        validate(need(length(input$in_metric.select)<=(0.5*ncol(habitat.by.site$data)),"Too many metrics for number of habitat variables"))
        validate(need(!any(input$in_metric.select%in%c("O:E","Bray-Curtis","CA1","CA2")),"The following metrics are only available with ANNA: Observed:Expected, Bray-Curtis, CA1, CA2"))
        validate(need(!any(is.na(bio.data$data$Summary.Metrics[,input$in_metric.select])),"NAs not allowed in biological data"))
      }
      
      nn.sites$data<-BenthicAnalysistesting::site.matchUI(Test=habitat.by.site$data[reftest.by.site$data==0,],
                                                          Reference=habitat.by.site$data[reftest.by.site$data==1,],
                                                          k=if (is.numeric(input$nn.k)){input$nn.k} else {NULL},
                                                          distance.decay=input$nn_useDD,
                                                          dd.factor=input$nn.factor,
                                                          dd.constant=input$nn.constant,
                                                          RDA.reference= if (input$nn_method=="RDA-ANNA") {bio.data$data$Summary.Metrics[reftest.by.site$data==1,input$in_metric.select]} else {NULL},
                                                          scale=T) #scale=input$nn.scale crashes it)
    }
    if (input$nn_method=="User Selected"){
      nn.sites$data$TF.matrix<-userMatchRefSites$TFmatrix
    }
  })
  
  output$out_nn.axis1<-renderUI({
    validate(need(input$nn_method!="User Selected",""))
    selectInput("in_nn.axis1","X Axis", choices=colnames(nn.sites$data$env.ordination.scores),
                selected=colnames(nn.sites$data$env.ordination.scores)[1])
  })
  output$out_nn.axis2<-renderUI({
    validate(need(input$nn_method!="User Selected",""))
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
  
  output$nn.ord <- renderPlot({
    print(nn.ord.raw())
  })
  
  output$nn.ord.plot.download<-downloadHandler(
    filename = function() {
      paste0("NNOrd.pdf", sep="")
    },
    content = function(file) {
      pdf(file, height=4,width=5.33334)
      print(nn.ord.raw())
      dev.off()
      }
  )
  
  nn.ord.raw<-reactive({
    validate(need(input$nn_method!="User Selected",""))
    validate(need(!is.null(habitat.by.site$data) & !is.null(reftest.ID.cols$data),"Missing Habitat data or Reference Sites"))
    
    if (!is.null(input$in_test_site_select) & !is.null(input$in_nn.axis1) & !is.null(input$in_nn.axis2)){
      
      if(is.null(input$in_nn.axis1) | is.null(input$in_nn.axis2)){return(NULL)}
      
      validate(need(!is.null(habitat.by.site$data) & !is.null(reftest.ID.cols$data),"Missing Habitat data or Reference Sites"))
      #validate(need(!is.null(nn.sites$data),"Insufficient Information"))
      validate(need(!is.null(input$in_nn.axis1) & !is.null(input$in_nn.axis2),""))
      validate(need(!is.null(input$in_test_site_select),""))
      validate(need(input$nn_method!="User Selected",""))
      #validate(need(!is.null(input$input$in_test_site_select),""))
      
      
      if (input$nn_method=="RDA-ANNA"){
        validate(need(!is.null(input$in_metric.select),"Select 3 or more indicator metrics"))
        validate(need(length(input$in_metric.select)>=3,"Select 3 or more indicator metrics"))
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
      
      x.ax.lab<-paste0(input$in_nn.axis1," (",strtrim(nn.sites$data$var.explained*100, 4)[colnames(nn.sites$data$env.ordination.scores)==input$in_nn.axis1],"%)")
      y.ax.lab<-paste0(input$in_nn.axis2," (",strtrim(nn.sites$data$var.explained*100, 4)[colnames(nn.sites$data$env.ordination.scores)==input$in_nn.axis2],"%)")
      
      p1 <- ggplot(data=nn.sites$data$ordination.scores,aes_string(x=input$in_nn.axis1, y=input$in_nn.axis2)) + 
        geom_vline(xintercept = 0, color="darkgrey") + geom_hline(yintercept = 0, color="darkgrey") +
        geom_point(aes(color=Class))  + theme_bw() + 
        labs(title=paste0("Nearest-neighbour Ordination by ",input$nn_method)#,
             #subtitle=if(input$in_test_site_select!="None"){paste0(input$in_test_site_select)} else {NULL}
        ) +
        xlab(paste0(x.ax.lab)) + 
        ylab(paste0(y.ax.lab)) +
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
    }
  })
  
  output$nn.dist<-renderPlot({
    validate(need(input$in_test_site_select!="None","Select a test site from the sidebar"))
    validate(need(input$nn_method!="User Selected","User matched reference site selection not yet implimented"))
    validate(need(!is.null(nn.sites$data),""))
    
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
  #TSA Calculations - single
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
    if (input$metdata==F){
      if (input$in_test_site_select=="None"||is.null(input$in_test_site_select)){
        tsa.results$output.list<-NULL
        tsa.results$data<-NULL
        validate(need(F, ""))
      }
      if (is.null(nn.sites$data)){
        tsa.results$output.list<-NULL
        tsa.results$data<-NULL
        validate(need(F, ""))
      }
      temp<-all.data$data
      colnames(temp)<-gsub(".",";",colnames(temp),fixed = T)
      
      Test<-temp[input$in_test_site_select,colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
      ref.set<-nn.sites$data$TF.matrix[rownames(Test),]
      Reference<-temp[names(ref.set)[ref.set==T],colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
      temp2<-BenthicAnalysistesting::add.met(Test=Test,Reference = Reference,original=F)
      rownames(temp2)[nrow(temp2)]<-rownames(Test)
      eval(parse(text=paste0("additional.metrics$data$'",rownames(Test),"'<-temp2")))
    }
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
    input$nn.scale,
    input$useMD
    ), {
      if (input$in_test_site_select=="None"||is.null(input$in_test_site_select)){
        tsa.results$output.list<-NULL
        tsa.results$data<-NULL
        validate(need(F, ""))
      }
      if (is.null(nn.sites$data)){
        tsa.results$output.list<-NULL
        tsa.results$data<-NULL
        validate(need(F, ""))
      }
      #if (input$nn_method=="User Selected") {
      #  tsa.results$output.list<-NULL
      #  tsa.results$data<-NULL
      #  validate(need(F, ""))
      #}
      if (is.null(input$in_metric.select)){
        tsa.results$output.list<-NULL
        tsa.results$data<-NULL
        validate(need(F, ""))
      }
      
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
    if (input$nn_method=="ANNA"|input$nn_method=="User Selected"){
      if (is.null(additional.metrics$data)){
        tsa.results$output.list<-NULL
        tsa.results$data<-NULL
        validate(need(F,""))
      }
      
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
    if (input$tsa_weighted & input$nn_method!="User Selected"){
      distance<-nn.sites$data$distance.matrix[rownames(Test),]
      distance<-distance[rownames(Reference)]
    } else {
      distance<-NULL
    }
    
    output1<-try(BenthicAnalysistesting::tsa.test.UI(Test=Test[,input$in_metric.select],
                                                     Reference=Reference[,input$in_metric.select],
                                                     outlier.rem=input$tsa_outlier_rem,
                                                     outbound=input$tsa_outbound,
                                                     m.select=input$useMD,
                                                     distance=distance
    ), silent=T)
    
    if(class(output1)=="try-error"){
      tsa.results$output.list<-NULL
      tsa.results$data<-output1
      validate(need(F, ""))
    }
    
    output2<-t(output1$tsa.results)
    tsa.results$output.list<-output1
    tsa.results$data<-output2
  })
  
  observeEvent(input$TSA_results_modal,{
    tsa.object<-tsa.results$output.list
    showModal(modalDialog(
      title="TSA Results",
      renderPrint(tsa.object),
      hr(),
      h4("Selected Metrics"),
      renderPrint(tsa.object$selected.metrics),
      h4("Significant Metrics"),
      renderPrint(tsa.object$general.results[4,]),
      size="l",
      easyClose = T
    ))
  })
  
  observeEvent(input$NN_results_modal,{
    tsa.object<-tsa.results$output.list
    
    showModal(modalDialog(
      title="Nearest-Neighbour Model Results",
      renderPrint(nn.sites$data),
      hr(),
      h4("Reference Set"),
      renderPrint(tsa.object$general.results[2,]),
      hr(),
      h4("Ordination Results"),
      renderPrint(nn.sites$data$ordination),
      hr(),
      h4("Distances"),
      renderPrint(nn.sites$data$distance.matrix[rownames(nn.sites$data$distance.matrix)%in%input$in_test_site_select,]),
      hr(),
      h4("Inclusions"),
      renderPrint(nn.sites$data$TF.matrix[rownames(nn.sites$data$TF.matrix)%in%input$in_test_site_select,]),
      hr(),
      box(title="Ordination Results (detailed)", width=12, collapsible = T, collapsed = T,
          renderPrint(summary(nn.sites$data$ordination))
      ),
      size="l",
      easyClose = T
    ))
  })
  
  #########################################################
  #TSA- batch
  ########################################################
  
  output$out_metric.select_b<-renderUI({
    validate(need(!is.null(reftest.ID.cols$data) & !is.null(bio.data$data),""))
    
    if((input$nn_method_b=="ANNA"|input$nn_method_b=="User Selected") & input$metdata==F){
      selectInput("in_metric.select_b","", multiple = T,selectize = F, size=15,
                  choices=c(colnames(bio.data$data$Summary.Metrics),
                            "O:E","Bray-Curtis","CA1","CA2"), 
                  selected=c(colnames(bio.data$data$Summary.Metrics),
                             "O:E","Bray-Curtis","CA1","CA2")
      )
    } else {
      selectInput("in_metric.select_b","", multiple = T,selectize = F, size=15,
                  choices=colnames(bio.data$data$Summary.Metrics)
      )
    }
  })
  
  additional.metrics_b<-reactiveValues(data=NULL,output.list=NULL) #create a list of tables of additional metrics at reference sites
  tsa.results_b<-reactiveValues(data=NULL,output.list=NULL) #create a list of tsa.test objects
  tsa.batch.outout<-reactiveValues(data=NULL) #dataset for downloading
  
  observeEvent(c(
    input$tsa_batch_go
  ), {
    if(input$tsa_batch_go>0){
      nn.sites$data<-NULL
    }
    withProgress(message="Working", value=0, { # Will this work?
      #if (input$nn_method_b=="User Selected" & input$tsa_batch_go>1) {
      #  tsa.results_b$output.list<-NULL
      #  tsa.results_b$data<-NULL
      #  additional.metrics_b<-NULL
      #  showModal(modalDialog(title="Error",
      #                        helpText("User Matched Reference Sites not yet implimented"),
      #                        easyClose = T,
      #                        size="s"
      #                        ))
      #  validate(need(F, "User matched reference sites not yet implimented"))
      #}
      if (is.null(input$in_metric.select_b) & input$tsa_batch_go>0){
        tsa.results_b$output.list<-NULL
        tsa.results_b$data<-NULL
        additional.metrics_b<-NULL
        showModal(modalDialog(title="Error",
                              helpText("Must select indicator metrics"),
                              easyClose = T,
                              size="s"
        ))
        validate(need(F, ""))
      }
      
      if (length(input$in_metric.select_b)<2 & input$tsa_batch_go>0){
        tsa.results_b$output.list<-NULL
        tsa.results_b$data<-NULL
        additional.metrics_b<-NULL
        showModal(modalDialog(title="Error",
                              helpText("Must select more than 2 indicator metrics"),
                              easyClose = T,
                              size="s"
        ))
        validate(need(F, ""))
      }
      if (input$nn_method_b!="User Selected" & is.null(habitat.by.site$data) & input$tsa_batch_go>0){
        tsa.results_b$output.list<-NULL
        tsa.results_b$data<-NULL
        additional.metrics_b<-NULL
        showModal(modalDialog(title="Error",
                              helpText("Habitat data not available"),
                              easyClose = T,
                              size="s"
        ))
        validate(need(F, ""))
      }
      if (input$nn_method_b=="User Selected" & is.null(userMatchRefSites$TFmatrix) & input$tsa_batch_go>0){
        tsa.results_b$output.list<-NULL
        tsa.results_b$data<-NULL
        additional.metrics_b<-NULL
        showModal(modalDialog(title="Error",
                              helpText("User Matched Reference Sites not available"),
                              easyClose = T,
                              size="s"
        ))
        validate(need(F, ""))
      }
      
      
      if (input$nn_method_b!="User Selected"){
        validate(need(!is.null(habitat.by.site$data) & !is.null(reftest.ID.cols$data),"Missing Habitat data or Reference Sites"))
        validate(need(reftest.ID.cols$data!="None","Missing Reference Sites"))
        validate(need(!any(is.na(habitat.by.site$data)),"NAs not allowed in habitat data"))
        validate(need(input$nn.k_b>2|input$nn_useDD_b,"Need k>=3 or use Distance-Decay site selection"))
        if (input$nn_method_b=="RDA-ANNA"){
          validate(need(length(input$in_metric.select_b)>=3,"Select at least 3 metrics"))
          validate(need(length(input$in_metric.select_b)<=(0.5*ncol(habitat.by.site$data)),"Too many metrics for number of habitat variables"))
          validate(need(!any(input$in_metric.select_b%in%c("O:E","Bray-Curtis","CA1","CA2")),"The following metrics are only available with ANNA: Observed:Expected, Bray-Curtis, CA1, CA2"))
          validate(need(!any(is.na(bio.data$data$Summary.Metrics[reftest.by.site$data==1,input$in_metric.select_b])),"NAs not allowed in biological data"))
        }
        nn.sites$data<-BenthicAnalysistesting::site.matchUI(Test=habitat.by.site$data[reftest.by.site$data==0,],
                                                            Reference=habitat.by.site$data[reftest.by.site$data==1,],
                                                            k=if (is.numeric(input$nn.k_b)){input$nn.k_b} else {NULL},
                                                            distance.decay=input$nn_useDD_b,
                                                            dd.factor=input$nn.factor_b,
                                                            dd.constant=input$nn.constant_b,
                                                            RDA.reference= if (input$nn_method_b=="RDA-ANNA") {bio.data$data$Summary.Metrics[reftest.by.site$data==1,input$in_metric.select_b]} else {NULL},
                                                            scale=T) #scale=input$nn.scale crashes it)
        if (!is.null(nn.sites$data)){
          temp<-data.frame(nn.sites$data$ordination.scores)
          temp<-temp[rownames(all.data$data),]
          all.data$data<-data.frame(cbind(all.data$data,temp))
        }
        ref.set<-nn.sites$data$TF.matrix[rownames(reftest.by.site$data)[reftest.by.site$data==0],]
        ref.set2<-apply(as.matrix(ref.set),1, function(m) list(colnames(ref.set)[m]))
        ref.set2<-unlist(ref.set2,recursive = F)
      }
      if (input$nn_method_b=="User Selected"){
        ref.set<-userMatchRefSites$TFmatrix[rownames(reftest.by.site$data)[reftest.by.site$data==0],]
        ref.set2<-apply(as.matrix(ref.set),1, function(m) colnames(ref.set)[m])
      }
      
      temp.all.data<-all.data$data
      colnames(temp.all.data)<-gsub(".",";",colnames(temp.all.data),fixed = T)
      temp.all.data<-temp.all.data[rownames(taxa.by.site$data),]
      
      if (input$metdata==F){
        if (input$nn_method_b=="ANNA"|input$nn_method_b=="User Selected"){
          additional.metrics_b$data<-data.frame(matrix(nrow=length(which(reftest.by.site$data==0)),ncol=4))
          rownames(additional.metrics_b$data)<-rownames(reftest.by.site$data)[reftest.by.site$data==0]
          colnames(additional.metrics_b$data)<-c("O:E","Bray-Curtis","CA1","CA2")
        }
      }

      tsa.results_b$data<-data.frame(matrix(nrow=length(which(reftest.by.site$data==0)),ncol=5))
      rownames(tsa.results_b$data)<-rownames(reftest.by.site$data)[reftest.by.site$data==0]
      colnames(tsa.results_b$data)<-c("TSA Impairment","Interval Test","Equivalence Test","Randomization p value","Test Site D2")
      
      for (i in 1:length(ref.set2)){
        incProgress(1/length(ref.set2), detail = paste("In progress: ", names(ref.set2)[i]))
        ref.set3<-ref.set2[i]
        ref.set3[[1]][length(ref.set3[[1]])+1]<-names(ref.set3)
        ref.set3<-ref.set3[[1]]
        if (input$nn_method_b!="RDA-ANNA"){
          add.mets<-BenthicAnalysistesting::add.met(Test=temp.all.data[names(ref.set2[i]),colnames(temp.all.data)%in%colnames(bio.data$data$Raw.Data)],
                                                    Reference = temp.all.data[ref.set2[[i]],colnames(temp.all.data)%in%colnames(bio.data$data$Raw.Data)],
                                                    original=F)
          
          i.mets<-cbind(temp.all.data[ref.set3,],add.mets)
          colnames(i.mets)<-gsub(";",".",colnames(i.mets),fixed = T)
          i.mets<-i.mets[,input$in_metric.select_b]
          
          additional.metrics_b$data[names(ref.set2)[i],]<-add.mets[nrow(add.mets),]
          eval(parse(text=paste0("additional.metrics$output.list$'",names(ref.set2)[i],"'<-add.mets")))
        } else {
          i.mets<-temp.all.data[ref.set3,]
          colnames(i.mets)<-gsub(";",".",colnames(i.mets),fixed = T)
          i.mets<-i.mets[,input$in_metric.select_b]
        }
        
        if (input$tsa_weighted){
          distance<-nn.sites$data$distance.matrix[names(ref.set2)[i],]
          distance<-distance[ref.set3]
          distance<-distance[1:(length(distance)-1)]
        } else {
          distance<-NULL
        }
        
        output1<-try(BenthicAnalysistesting::tsa.test.UI(Test=i.mets[nrow(i.mets),],
                                                         Reference=i.mets[1:(nrow(i.mets)-1),],
                                                         outlier.rem=input$tsa_outlier_rem_b,
                                                         outbound=input$tsa_outbound_b,
                                                         m.select=input$useMD_b,
                                                         distance=distance
        ), silent=T)
        
        if(class(output1)=="try-error"){
          tsa.results_b$data[names(ref.set2)[i],]<-c("Error","NA","NA","NA","NA")
          eval(parse(text=paste0("tsa.results_b$output.list$'",names(ref.set2)[i],"'<-attr(output1,'condition')$message")))
        } else {
          tsa.results_b$data[names(ref.set2)[i],]<-output1$tsa.results[1:5,]
          eval(parse(text=paste0("tsa.results_b$output.list$'",names(ref.set2)[i],"'<-output1")))
        }
        
      }
      if (input$metdata==F){
        if (input$nn_method_b=="ANNA"|input$nn_method_b=="User Selected"){
          temp<-data.frame(additional.metrics_b$data)
          temp<-temp[rownames(all.data$data),]
          all.data$data<-data.frame(cbind(all.data$data,temp))
          rm(temp)
        }
      }

      tsa.results_b$data$`TSA Impairment`<-as.factor(tsa.results_b$data$`TSA Impairment`)
      tsa.results_b$data$`Interval Test`<-as.numeric(tsa.results_b$data$`Interval Test`)
      tsa.results_b$data$`Equivalence Test`<-as.numeric(tsa.results_b$data$`Equivalence Test`)
      tsa.results_b$data$`Randomization p value`<-as.numeric(tsa.results_b$data$`Randomization p value`)
      tsa.results_b$data$`Test Site D2`<-as.numeric(tsa.results_b$data$`Test Site D2`)
      
      temp<-data.frame(tsa.results_b$data)
      temp<-temp[rownames(all.data$data),]
      all.data$data<-data.frame(cbind(all.data$data,temp))
      #all.data$data$TSA.Impairment<-factor(all.data$data$TSA.Impairment,levels(all.data$data$TSA.Impairment)[c(2,3,1)])
      
      rm(temp)
      
      tsa.batch.outout$data<-data.frame(tsa.results_b$data)
      passed.sites<-rownames(tsa.batch.outout$data)[!is.na(tsa.batch.outout$data$Interval.Test)]
      error.sites<-rownames(tsa.batch.outout$data)[is.na(tsa.batch.outout$data$Interval.Test)]
      
      tsa.batch.outout$data$Selected.Metrics<-NA
      tsa.batch.outout$data$Significant.Metrics<-NA
      tsa.batch.outout$data$Reference.Set<-NA
      tsa.batch.outout$data$Error.details<-NA
      tsa.batch.outout$data$Selected.Metrics[rownames(tsa.batch.outout$data)%in%passed.sites]<-sapply(tsa.results_b$output.list[passed.sites], function(x)x[["general.results"]][3,])
      tsa.batch.outout$data$Significant.Metrics[rownames(tsa.batch.outout$data)%in%passed.sites]<-sapply(tsa.results_b$output.list[passed.sites], function(x)x[["general.results"]][4,])
      tsa.batch.outout$data$Reference.Set[rownames(tsa.batch.outout$data)%in%passed.sites]<-sapply(tsa.results_b$output.list[passed.sites], function(x)x[["general.results"]][2,])
      tsa.batch.outout$data$Reference.Set[rownames(tsa.batch.outout$data)%in%error.sites]<-sapply(tsa.results_b$output.list[error.sites], function(x)x[[1]])
    
      tsa.batch.outout$data$Reference.Set<-as.character(tsa.batch.outout$data$Reference.Set)
      })
  })
  
  

  
  observeEvent(input$TSA_results_modal_b,{
    tsa.object<-tsa.results_b$output.list[which(names(tsa.results_b$output.list)%in%input$in_batch_test_result_select)]
    tsa.object<-tsa.object[[1]]
    showModal(modalDialog(
      title="TSA Results",
      renderPrint(tsa.object),
      hr(),
      h4("Selected Metrics"),
      renderPrint(tsa.object$selected.metrics),
      h4("Significant Metrics"),
      renderPrint(tsa.object$general.results[4,]),
      size="l",
      easyClose = T
    ))
  })
  
  observeEvent(input$NN_results_modal_b,{
    if (input$nn_method_b!="User Selected"){
      tsa.object<-tsa.results_b$output.list[which(names(tsa.results_b$output.list)%in%input$in_batch_test_result_select)]
      tsa.object<-tsa.object[[1]]
      
      showModal(modalDialog(
        title="Nearest-Neighbour Model Results",
        renderPrint(nn.sites$data),
        hr(),
        h4("Reference Set"),
        renderPrint(tsa.object$general.results[2,]),
        hr(),
        h4("Ordination Results"),
        renderPrint(nn.sites$data$ordination),
        hr(),
        h4("Distances"),
        renderPrint(nn.sites$data$distance.matrix[rownames(nn.sites$data$distance.matrix)%in%input$in_batch_test_result_select,]),
        hr(),
        h4("Inclusions"),
        renderPrint(nn.sites$data$TF.matrix[rownames(nn.sites$data$TF.matrix)%in%input$in_batch_test_result_select,]),
        hr(),
        box(title="Ordination Results (detailed)", width=12, collapsible = T, collapsed = T,
            renderPrint(summary(nn.sites$data$ordination))
        ),
        size="l",
        easyClose = T
      ))
    } 
  })
  
  #########################################################
  #TSA- batch outputs
  ########################################################
  output$out_tsa_bulk_table_sumby<-renderUI({
    selectizeInput("in_tsa_bulk_table_sumby","Summarize by:",choices=site.ID.cols$data, multiple=T)
  })
  
  output$tsa_bulk_table<-renderDataTable({
    if (!is.null(input$in_batch_test_result_select)){
      #sites<-data.frame(do.call(rbind,(strsplit(rownames(all.data$data),";"))))
      #apply(sites,2,function(x)length(unique(x)))
      if(is.null(input$in_tsa_bulk_table_sumby)){
        outtable<-plyr::count(all.data$data[reftest.by.site$data==0,],vars=c("TSA.Impairment"))
        colnames(outtable)[(ncol(outtable)-1):ncol(outtable)]<-c("Impairment","Frequency")
      } else {
        outtable<-plyr::count(all.data$data[reftest.by.site$data==0,],vars=c(input$in_tsa_bulk_table_sumby,"TSA.Impairment"))
        colnames(outtable)[(ncol(outtable)-1):ncol(outtable)]<-c("Impairment","Frequency")
      }
      DT::datatable(outtable,rownames = F,filter = 'top')
      
    } else {
      validate(need(F,"Must run Batch Mode First"))
    }
  })
  
  output$download_tsa_batch<-downloadHandler(filename = function() { paste("Batch_Results-",input$inrawbioFile[1], sep='') },
                                             content = function(file) {write.csv(as.data.frame(tsa.batch.outout$data),file,row.names = T)})
  
  
  output$out_batch_test_result_select<-renderUI({
    validate(need(!is.null(tsa.batch.outout$data),""))
    
    selectInput("in_batch_test_result_select","",
                choices=rownames(reftest.by.site$data)[reftest.by.site$data==0]
                  #rownames(all.data$data)[all.data$data$Class=="Test"]#,
                #selected=rownames(all.data$data[all.data$data$Class=="Test",])[1]
                #choices=c("None",rownames(all.data$data[all.data$data$Class=="Test",]))
    )
  })
  
  output$tsa.result.printed_b<-renderUI({
    if (!is.null(input$in_batch_test_result_select)){
      tsa.object<-tsa.results_b$output.list[which(names(tsa.results_b$output.list)%in%input$in_batch_test_result_select)]
      tsa.object<-tsa.object[[1]]
      #tsa.object<-tsa.results$output.list
      
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
          infoBox(value=tsa.object$tsa.results["TSA Impairment",],title="Status",width=12, color=stat.col,icon=icon("heartbeat",lib="font-awesome"),fill=T),
          fluidRow(valueBox(tsa.object$tsa.results["Test Site D2",],"Test site D2",width=4, color="light-blue",icon=icon("line-chart",lib="font-awesome")),
                   valueBox(tsa.object$tsa.results["Interval Test",],"Interval Test",width=4, color="light-blue",icon=icon("hashtag",lib="font-awesome")),
                   valueBox(tsa.object$tsa.results["Equivalence Test",],"Equivalence Test",width=4, color="light-blue",icon=icon("hashtag",lib="font-awesome"))
          )
      )
    }
  })
  
  output$tsa.circle.plot_b<-renderPlot({
    if (!is.null(input$in_batch_test_result_select)){
      tsa.object<-tsa.results_b$output.list[which(names(tsa.results_b$output.list)%in%input$in_batch_test_result_select)]
      tsa.object<-tsa.object[[1]]
      #tsa.object<-tsa.results$output.list
      
      z.scores<-data.frame(tsa.object$z.scores)
      z.scores$ref<-1
      z.scores$ref[nrow(z.scores)]<-0
      z.scores$ref<-as.factor(z.scores$ref)
      z.scores$names<-rownames(z.scores)
      
      nRef<-length(which(z.scores$ref==1))
      
      Community.Structure<-c("O.E","Bray.Curtis","CA1","CA2")
      Biodiversity<-c("Richness","Simpson")
      Sensitivity<-c("HBI","Percent.Intolerants","Intolerants.Richness")
      Hydrology<-c("CEFI")
      Habitat.Guilds<-c("Clinger.Percent","Clinger.Richness","Burrower.Percent","Burrower.Richness","Sprawler.Percent",
                        "Sprawler.Richness")
      Feeding.Guilds<-c("Predator.Percent","Predator.Richness", "ScraperGrazer.Percent","ScraperGrazer.Richness",
                        "Shredder.Percent", "Shredder.Richness", "Filterer.Percent", "Filterer.Richness", "Gatherer.Percent",
                        "Gatherer.Richness")
      
      #plot.data<-data.frame(categories=c("Community.Structure", "Biodiversity", "Sensitivity", "Hydrology", "Physical.Habitat", "Food.Web"), test=NA)
      plot.data<-data.frame(t(data.frame(max=rep(1,6),min=rep(0,6),test=NA)))
      colnames(plot.data)<-c("Community.Structure", "Biodiversity", "Sensitivity", "Hydrology", "Habitat.Guilds", "Feeding.Guilds")
      rownames(plot.data)[3]<-input$in_batch_test_result_select
      
      for (i in as.character(colnames(plot.data))) {
        try1<-try(eval(parse(text=paste0("data1<-z.scores[,colnames(z.scores)%in%",i, "]"))),silent=T)
        if(class(try1)=="try-error"){
          next()
        }
        eval(parse(text=paste0("data1<-z.scores[,colnames(z.scores)%in%",i, "]")))
        if (is.data.frame(data1)){
          are.nas<-unlist(apply(data1,2, function(x) !any(is.na(x)))) & unlist(apply(data1,2, function(x) IQR(x)>0))
          if (sum(are.nas)==0) {
            next()
          }
          if(sum(are.nas)==1){
            data1<-data1[,are.nas]
            tsa.dist<-abs(data1[length(data1)]-mean(data1[1:(length(data1)-1)]))*sqrt(nRef)
            tsa.lambda<-qchisq(0.05,1, ncp = 0, lower.tail = FALSE, log.p = FALSE)*(nRef/2)
            tsa.NCPinterval<-1-pf(tsa.dist, 1, (nRef-1), tsa.lambda, log=FALSE)
            plot.data[3,i]<-tsa.NCPinterval
          } else {
            tsa.dist<-mahalanobis(data1[nrow(data1),are.nas],apply(data1[1:(nrow(data1)-1),are.nas],2,mean),cov(data1[1:(nrow(data1)-1),are.nas]))
            tsa.lambda<-qchisq(0.05,ncol(data1), ncp = 0, lower.tail = FALSE, log.p = FALSE)*(nRef/2)
            tsa.F<-((nRef-ncol(data1))*nRef*tsa.dist)/(ncol(data1)*(nRef-1))
            tsa.NCPinterval<-1-pf(tsa.F, ncol(data1), (nRef-ncol(data1)), tsa.lambda, log=FALSE)
            plot.data[3,i]<-tsa.NCPinterval
          }
        } else {
          are.nas<-!any(is.na(data1))
          if (sum(are.nas)==0) {
            next()
          }
          tsa.dist<-abs(data1[length(data1)]-mean(data1[1:(length(data1)-1)]))*sqrt(nRef)
          tsa.lambda<-qchisq(0.05,1, ncp = 0, lower.tail = FALSE, log.p = FALSE)*(nRef/2)
          tsa.NCPinterval<-1-pf(tsa.dist, 1, (nRef-1), tsa.lambda, log=FALSE)
          plot.data[3,i]<-tsa.NCPinterval
        }
      }
      plot.data<-plot.data[,!is.na(plot.data[3,])]
      validate(need(ncol(plot.data)>2,"Insufficient Metrics selected"))
      fmsb::radarchart(plot.data, axistype=1 , 
                       
                       #custom polygon
                       pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                       
                       #custom the grid
                       cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,length.out=5), cglwd=0.8,
                       
                       #custom labels
                       vlcex=0.8 
      )
      
    }
  })
  
  output$tsa.metric.plot_b<-renderPlot({
    if (!is.null(input$in_batch_test_result_select)){
      #validate(need(!is.null(input$input$in_batch_test_result_select),""))
      
      tsa.object<-tsa.results_b$output.list[which(names(tsa.results_b$output.list)%in%input$in_batch_test_result_select)]
      tsa.object<-tsa.object[[1]]
      #tsa.object<-tsa.results$output.list
      
      tsa.stand<-tsa.object$z.scores
      nInd<-ncol(tsa.stand)
      nRef<-nrow(tsa.stand)-1
      
      part.tsa<-if (!is.null(tsa.object$partial.tsa)) {tsa.object$partial.tsa} else {NULL}
      all.met<-colnames(tsa.stand)
      sel.met<-unlist(strsplit(substr(tsa.object$general.results["Selected Indicator Metrics",],1,(nchar(tsa.object$general.results["Selected Indicator Metrics",])-2)),split=", "))
      
      melt.ref<-reshape2::melt(tsa.stand[1:nRef,])
      melt.ref$Group<-NA
      melt.ref$Group[melt.ref$Var2%in%c("Richness","Simpson","Shannon","Percent.Dominance")]<-"Biodiversity"
      melt.ref$Group[melt.ref$Var2%in%c("Percent.Oligochaeta","Percent.Chironomidae","Percent.Amphipoda","Percent.Isopoda","Percent.Coleoptera",
                                        "Ephem.as.Baetidae","Percent.EPT","Percent.mEPT","Percent.ICHAEBO",
                                        "EPT.Richness","Ephem.Richness","Percent.Ephem","Plec.Richness",
                                        "Percent.Plec","Trich.Richness","Percent.Trich","EPT.per.EPT.and.Chir",
                                        "Percent.Non.Chir.Dip","Trich.as.Hydropsychidae","Coleo.as.Elmidae","Percent.CIGH")]<-"Taxonomic"
      melt.ref$Group[melt.ref$Var2%in%c("Intolerants.Richness" ,"Percent.Intolerants","HBI")]<-"Sensitivity"
      melt.ref$Group[melt.ref$Var2%in%c("CEFI")]<-"Hydrology"
      melt.ref$Group[melt.ref$Var2%in%c("Predator.Percent" ,"Predator.Richness",
                                        "ScraperGrazer.Percent","ScraperGrazer.Richness",
                                        "Shredder.Richness","Shredder.Percent"
                                        ,"Filterer.Percent","Filterer.Richness",
                                        "Gatherer.Percent","Gatherer.Richness",
                                        "ScraperGrazer.to.Shredder.Collector")]<-"Feeding Guilds"
      melt.ref$Group[melt.ref$Var2%in%c("Clinger.Percent","Clinger.Richness","Burrower.Percent",
                                        "Burrower.Richness","Sprawler.Percent","Sprawler.Richness",
                                        "Burrower.to.Sprawler.Clinger")]<-"Habitat Guilds"
      melt.ref$Group[melt.ref$Var2%in%c("O:E" ,"Bray-Curtis","CA1","CA2")]<-"Community Structure"
      
      p1<-ggplot(data=melt.ref) + theme_bw() +
        geom_boxplot(data=melt.ref, aes(x=Var2,y=value, fill=Group), width=0.5) +
        scale_fill_brewer(palette="Dark2") +
        labs(title=paste0(input$in_batch_test_result_select," Indicator metrics")) +
        theme(axis.text.x = element_text(angle = 65, hjust = 1), legend.position="right", strip.text.x = element_blank()) +
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
  
  output$nn.ord_b<-renderPlot({
    if (!is.null(input$in_batch_test_result_select)){
      #validate(need(!is.null(input$input$in_batch_test_result_select),""))
      
      validate(need(input$nn_method_b!="User Selected",""))
      
      if (input$nn_method_b=="RDA-ANNA"){
        validate(need(!is.null(input$in_metric.select_b),"Select 3 or more indicator metrics"))
        validate(need(length(input$in_metric.select_b)>=3,"Select 3 or more indicator metrics"))
      }
      
      if (input$in_batch_test_result_select!="None"){
        hull<-nn.sites$data$ordination.scores[nn.sites$data$ordination.scores$Class=="Reference",]
        hull$Ref<-data.frame(t(nn.sites$data$TF.matrix[rownames(nn.sites$data$TF.matrix)%in%input$in_batch_test_result_select,]))
        hull<-hull[hull$Ref==T,]
        hull<-hull[chull(hull[,c(colnames(nn.sites$data$ordination.scores)[1],colnames(nn.sites$data$ordination.scores)[2])]),]
        
        test.site<-nn.sites$data$ordination.scores[rownames(nn.sites$data$ordination.scores)%in%input$in_batch_test_result_select,]
        
        reference.sites<-nn.sites$data$ordination.scores[nn.sites$data$ordination.scores$Class=="Reference",]
        reference.sites$Ref<-data.frame(t(nn.sites$data$TF.matrix[rownames(nn.sites$data$TF.matrix)%in%input$in_batch_test_result_select,]))
        reference.sites<-reference.sites[reference.sites$Ref==T,]
      }
      
      x.ax.lab<-paste0("PC1 (",strtrim(nn.sites$data$var.explained*100, 4)[1],"%)")
      y.ax.lab<-paste0("PC2 (",strtrim(nn.sites$data$var.explained*100, 4)[2],"%)")

      p1 <- ggplot(data=nn.sites$data$ordination.scores,aes_string(x=colnames(nn.sites$data$ordination.scores)[1], y=colnames(nn.sites$data$ordination.scores)[2])) + 
        geom_vline(xintercept = 0, color="darkgrey") + geom_hline(yintercept = 0, color="darkgrey") +
        geom_point(aes(color=Class))  + theme_bw() + 
        labs(title=paste0("Nearest-neighbour Ordination by ",input$nn_method_b)#,
             #subtitle=if(input$in_batch_test_result_select!="None"){paste0(input$in_batch_test_result_select)} else {NULL}
        )+
        xlab(paste0(x.ax.lab)) + 
        ylab(paste0(y.ax.lab))
      
      p1<- p1 + geom_polygon(data=hull[,c(colnames(nn.sites$data$ordination.scores)[1],colnames(nn.sites$data$ordination.scores)[2])],alpha=0.5)
      p1<- p1 + geom_text(data=reference.sites[,c(colnames(nn.sites$data$ordination.scores)[1],colnames(nn.sites$data$ordination.scores)[2])], label=rownames(reference.sites))
      p1<- p1 + geom_point(data=test.site[,c(colnames(nn.sites$data$ordination.scores)[1],colnames(nn.sites$data$ordination.scores)[2])],size=3)
      
      p1
    }
  })
  
  output$nn.dist_b<-renderPlot({
    validate(need(input$nn_method_b!="User Selected",""))
    if (!is.null(input$in_batch_test_result_select)){
      
      distances<-nn.sites$data$distance.matrix[rownames(nn.sites$data$distance.matrix)%in%input$in_batch_test_result_select,]
      distances<-distances[order(distances)]
      distances<-data.frame(distances)
      nn.distances<-as.numeric(nn.sites$data$dd.number$dd.number[nn.sites$data$dd.number$sites%in%input$in_batch_test_result_select])
      distances$Selected<-NA
      distances$Selected[1:nn.distances]<-"Yes"
      distances$Selected[(nn.distances+1):nrow(distances)]<-"No"
      colnames(distances)[1]<-"Distance"
      
      p2<-ggplot()+geom_bar(data=distances,aes(y=Distance, x=1:nrow(distances), fill=Selected, color=Selected), stat="identity")+theme_bw()+
        labs(title=paste0("Nearest-neighbour Distances by ",input$nn_method_b),
             subtitle=paste0(input$in_batch_test_result_select)) +
        xlab("Distance") + 
        ylab("")
      
      p2
      
    }
  })
  
  
  #########################################################
  #TSA Plots
  #########################################################
  
  output$tsa.result.printed<-renderUI({
    validate(need(input$in_test_site_select!="None","Select a test site from the sidebar"))
    validate(need(!is.null(input$in_metric.select),"Select Metrics"))
    validate(need(class(tsa.results$data)!="try-error", tsa.results$data))
    validate(need(!is.null(tsa.results$data),""))

    #tsa.object<-tsa.results$output.list[which(names(tsa.results$output.list)%in%input$in_test_site_select)]
    #tsa.object<-tsa.object[[1]]
    tsa.object<-tsa.results$output.list
    
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
        infoBox(value=tsa.object$tsa.results["TSA Impairment",],title="Status",width=12, color=stat.col,icon=icon("heartbeat",lib="font-awesome"),fill=T),
        fluidRow(valueBox(tsa.object$tsa.results["Test Site D2",],"Test site D2",width=4, color="light-blue",icon=icon("line-chart",lib="font-awesome")),
                 valueBox(tsa.object$jacknife["Jacknife Consistency",],"Jacknife Consistency",width=4, color="light-blue",icon=icon("refresh",lib="font-awesome")),
                 valueBox(tsa.object$general.results["Number of Metrics",],"Number of Metrics",width=4, color="light-blue",icon=icon("hashtag",lib="font-awesome"))
        ),
        fluidRow(valueBox(tsa.object$general.results["Number of Reference Sites",],"Reference samples",width=4, color="light-blue",icon=icon("hashtag",lib="font-awesome")),
                 valueBox(tsa.object$tsa.results["Interval Test",],"Interval Test",width=4, color="light-blue",icon=icon("hashtag",lib="font-awesome")),
                 valueBox(tsa.object$tsa.results["Equivalence Test",],"Equivalence Test",width=4, color="light-blue",icon=icon("hashtag",lib="font-awesome"))
        ),
        infoBox(value=paste0(tsa.object$selected.metrics, collapse="",sep="; "),title="Selected Metrics",width=12, color="light-blue",fill=T)
    )
    
  })
  
  output$tsa.distance.plot <- renderPlot( {
    print(tsa.distance.plot.raw())
  })
  
  output$tsa.distance.plot.download<-downloadHandler(
    filename = function() {
      paste0("TSAdistance.pdf", sep="")
    },
    content = function(file) {
      pdf(file, height=4,width=5.33334)
      tsa.object<-tsa.results$output.list
      
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
      dev.off()
    }
  )
  
  tsa.distance.plot.raw<-reactive({
    validate(need(input$in_test_site_select!="None",""))
    validate(need(!is.null(input$in_metric.select),""))
    validate(need(class(tsa.results$data)!="try-error", ""))
    validate(need(!is.null(tsa.results$data),""))
    
    #tsa.object<-tsa.results$output.list[which(names(tsa.results$output.list)%in%input$in_test_site_select)]
    #tsa.object<-tsa.object[[1]]
    tsa.object<-tsa.results$output.list
    
    tsa.dist<-tsa.object$mahalanobis.distance
    nInd<-as.numeric(tsa.object$general.results["Number of Metrics",])
    nRef<-as.numeric(tsa.object$general.results["Number of Reference Sites",])
    tsa.lambda<-as.numeric(tsa.object$tsa.results["TSA Lambda",])
    test.site<-tsa.object$general.results["Test Site",]
    
    
    d1<-density(tsa.dist[1:(length(tsa.dist)-1)])
    d2<-density(((nInd*(nRef-1))*rf(1000000, df1=nInd, df2=(nRef-nInd), ncp=tsa.lambda))/((nRef-nInd)*nRef))
    
    p1<-recordPlot()
    
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
    
    replayPlot(p1)
  })
 
  output$tsa.metric.plot <- renderPlot( {
    print(tsa.metric.plot.raw())
  })
  
  output$tsa.metric.plot.download<-downloadHandler(
    filename = function() {
      paste0("TSAmetrics.pdf", sep="")
    },
    content = function(file) {
      pdf(file, height=4,width=10)
      print(tsa.metric.plot.raw())
      dev.off()
    }
  )
  
  tsa.metric.plot.raw<-reactive({
    validate(need(input$in_test_site_select!="None",""))
    validate(need(!is.null(input$in_metric.select),""))
    validate(need(class(tsa.results$data)!="try-error", ""))
    validate(need(!is.null(tsa.results$data),""))
    
    #tsa.object<-tsa.results$output.list[which(names(tsa.results$output.list)%in%input$in_test_site_select)]
    #tsa.object<-tsa.object[[1]]
    tsa.object<-tsa.results$output.list
    
    tsa.stand<-tsa.object$z.scores
    nInd<-ncol(tsa.stand)
    nRef<-nrow(tsa.stand)-1
    
    part.tsa<-if (!is.null(tsa.object$partial.tsa)) {tsa.object$partial.tsa} else {NULL}
    all.met<-colnames(tsa.stand)
    sel.met<-unlist(strsplit(substr(tsa.object$general.results["Selected Indicator Metrics",],1,(nchar(tsa.object$general.results["Selected Indicator Metrics",])-2)),split=", "))
    
    melt.ref<-reshape2::melt(tsa.stand[1:nRef,])
    melt.ref$Group<-NA
    melt.ref$Group[melt.ref$Var2%in%c("Richness","Simpson","Shannon","Percent.Dominance")]<-"Biodiversity"
    melt.ref$Group[melt.ref$Var2%in%c("Percent.Oligochaeta","Percent.Chironomidae","Percent.Amphipoda","Percent.Isopoda","Percent.Coleoptera",
                                      "Ephem.as.Baetidae","Percent.EPT","Percent.mEPT","Percent.ICHAEBO",
                                      "EPT.Richness","Ephem.Richness","Percent.Ephem","Plec.Richness",
                                      "Percent.Plec","Trich.Richness","Percent.Trich","EPT.per.EPT.and.Chir",
                                      "Percent.Non.Chir.Dip","Trich.as.Hydropsychidae","Coleo.as.Elmidae","Percent.CIGH")]<-"Taxonomic"
    melt.ref$Group[melt.ref$Var2%in%c("Intolerants.Richness" ,"Percent.Intolerants","HBI")]<-"Sensitivity"
    melt.ref$Group[melt.ref$Var2%in%c("CEFI")]<-"Hydrology"
    melt.ref$Group[melt.ref$Var2%in%c("Predator.Percent" ,"Predator.Richness",
                                      "ScraperGrazer.Percent","ScraperGrazer.Richness",
                                      "Shredder.Richness","Shredder.Percent"
                                      ,"Filterer.Percent","Filterer.Richness",
                                      "Gatherer.Percent","Gatherer.Richness",
                                      "ScraperGrazer.to.Shredder.Collector")]<-"Feeding Guilds"
    melt.ref$Group[melt.ref$Var2%in%c("Clinger.Percent","Clinger.Richness","Burrower.Percent",
                                      "Burrower.Richness","Sprawler.Percent","Sprawler.Richness",
                                      "Burrower.to.Sprawler.Clinger")]<-"Habitat Guilds"
    melt.ref$Group[melt.ref$Var2%in%c("O:E" ,"Bray-Curtis","CA1","CA2")]<-"Community Structure"

    p1<-ggplot(data=melt.ref) + theme_bw() +
      geom_boxplot(data=melt.ref, aes(x=Var2,y=value, fill=Group), width=0.5) +
      scale_fill_brewer(palette="Dark2") +
      labs(title=paste0(input$in_test_site_select," Indicator metrics")) +
      theme(axis.text.x = element_text(angle = 65, hjust = 1), legend.position="right", strip.text.x = element_blank()) +
      ylim(min(tsa.stand)*1.3, max(tsa.stand)*1.1) +
      xlab("") + 
      ylab("") +
      geom_point(data=data.frame(x=levels(melt.ref$Var2)[which(colnames(tsa.stand)%in%sel.met)],y=tsa.stand[(nRef+1),sel.met]), aes(x=x,y=y), shape=1,fill="transparent",size=4) +
      geom_point(data=data.frame(x=levels(melt.ref$Var2),y=tsa.stand[(nRef+1),]), aes(x=x,y=y), col="red")
    
    
    if (any(part.tsa$p<0.05)) {
      p1 <- p1 +geom_point(data=data.frame(x=levels(melt.ref$Var2)[which(colnames(tsa.stand)%in%rownames(part.tsa)[part.tsa$p<0.05])],y=rep(min(tsa.stand)*1.2,length(rownames(part.tsa)[part.tsa$p<0.05]))), aes(x=x,y=y), shape=8,size=2)
    }
    p1
  })
  
  output$tsa.circle.plot <- renderPlot({
    print(tsa.circle.plot.raw())
  })
  
  output$tsa.circle.plot.download<-downloadHandler(
    filename = function() {
      paste0("TSAcircleplot.pdf", sep="")
    },
    content = function(file) {
      pdf(file, height=4,width=5.33334)
      tsa.object<-tsa.results$output.list
      
      z.scores<-data.frame(tsa.object$z.scores)
      z.scores$ref<-1
      z.scores$ref[nrow(z.scores)]<-0
      z.scores$ref<-as.factor(z.scores$ref)
      z.scores$names<-rownames(z.scores)
      
      nRef<-length(which(z.scores$ref==1))
      
      Community.Structure<-c("O.E","Bray.Curtis","CA1","CA2")
      Biodiversity<-c("Richness","Simpson")
      Sensitivity<-c("HBI","Percent.Intolerants","Intolerants.Richness")
      Hydrology<-c("CEFI")
      Habitat.Guilds<-c("Clinger.Percent","Clinger.Richness","Burrower.Percent","Burrower.Richness","Sprawler.Percent",
                        "Sprawler.Richness")
      Feeding.Guilds<-c("Predator.Percent","Predator.Richness", "ScraperGrazer.Percent","ScraperGrazer.Richness",
                        "Shredder.Percent", "Shredder.Richness", "Filterer.Percent", "Filterer.Richness", "Gatherer.Percent",
                        "Gatherer.Richness")
      
      #plot.data<-data.frame(categories=c("Community.Structure", "Biodiversity", "Sensitivity", "Hydrology", "Physical.Habitat", "Food.Web"), test=NA)
      plot.data<-data.frame(t(data.frame(max=rep(1,6),min=rep(0,6),test=NA)))
      colnames(plot.data)<-c("Community.Structure", "Biodiversity", "Sensitivity", "Hydrology", "Habitat.Guilds", "Feeding.Guilds")
      rownames(plot.data)[3]<-input$in_test_site_select
      
      for (i in as.character(colnames(plot.data))) {
        try1<-try(eval(parse(text=paste0("data1<-z.scores[,colnames(z.scores)%in%",i, "]"))),silent=T)
        if(class(try1)=="try-error"){
          next()
        }
        eval(parse(text=paste0("data1<-z.scores[,colnames(z.scores)%in%",i, "]")))
        if (is.data.frame(data1)){
          are.nas<-unlist(apply(data1,2, function(x) !any(is.na(x)))) & unlist(apply(data1,2, function(x) IQR(x)>0))
          if (sum(are.nas)==0) {
            next()
          }
          if(sum(are.nas)==1){
            data1<-data1[,are.nas]
            tsa.dist<-abs(data1[length(data1)]-mean(data1[1:(length(data1)-1)]))*sqrt(nRef)
            tsa.lambda<-qchisq(0.05,1, ncp = 0, lower.tail = FALSE, log.p = FALSE)*(nRef/2)
            tsa.NCPinterval<-1-pf(tsa.dist, 1, (nRef-1), tsa.lambda, log=FALSE)
            plot.data[3,i]<-tsa.NCPinterval
          } else {
            tsa.dist<-mahalanobis(data1[nrow(data1),are.nas],apply(data1[1:(nrow(data1)-1),are.nas],2,mean),cov(data1[1:(nrow(data1)-1),are.nas]))
            tsa.lambda<-qchisq(0.05,ncol(data1), ncp = 0, lower.tail = FALSE, log.p = FALSE)*(nRef/2)
            tsa.F<-((nRef-ncol(data1))*nRef*tsa.dist)/(ncol(data1)*(nRef-1))
            tsa.NCPinterval<-1-pf(tsa.F, ncol(data1), (nRef-ncol(data1)), tsa.lambda, log=FALSE)
            plot.data[3,i]<-tsa.NCPinterval
          }
        } else {
          are.nas<-!any(is.na(data1))
          if (sum(are.nas)==0) {
            next()
          }
          tsa.dist<-abs(data1[length(data1)]-mean(data1[1:(length(data1)-1)]))*sqrt(nRef)
          tsa.lambda<-qchisq(0.05,1, ncp = 0, lower.tail = FALSE, log.p = FALSE)*(nRef/2)
          tsa.NCPinterval<-1-pf(tsa.dist, 1, (nRef-1), tsa.lambda, log=FALSE)
          plot.data[3,i]<-tsa.NCPinterval
        }
      }
      plot.data<-plot.data[,!is.na(plot.data[3,])]
      validate(need(ncol(plot.data)>2,"Insufficient Metrics selected"))
      p1<-fmsb::radarchart(plot.data, axistype=1 , 
                           
                           #custom polygon
                           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                           
                           #custom the grid
                           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,length.out=5), cglwd=0.8,
                           
                           #custom labels
                           vlcex=0.8 
      )
      p1
      dev.off()
    }
  )
  
  tsa.circle.plot.raw<-reactive({
    validate(need(!is.null(tsa.results$data),""))
    validate(need(!input$metdata,""))
    validate(need(class(tsa.results$data)!="try-error", ""))
    
    if (input$in_test_site_select!="None"){
      #tsa.object<-tsa.results$output.list[which(names(tsa.results$output.list)%in%input$in_test_site_select)]
      #tsa.object<-tsa.object[[1]]
      tsa.object<-tsa.results$output.list
      
      z.scores<-data.frame(tsa.object$z.scores)
      z.scores$ref<-1
      z.scores$ref[nrow(z.scores)]<-0
      z.scores$ref<-as.factor(z.scores$ref)
      z.scores$names<-rownames(z.scores)
      
      nRef<-length(which(z.scores$ref==1))
      
      Community.Structure<-c("O.E","Bray.Curtis","CA1","CA2")
      Biodiversity<-c("Richness","Simpson")
      Sensitivity<-c("HBI","Percent.Intolerants","Intolerants.Richness")
      Hydrology<-c("CEFI")
      Habitat.Guilds<-c("Clinger.Percent","Clinger.Richness","Burrower.Percent","Burrower.Richness","Sprawler.Percent",
                 "Sprawler.Richness")
      Feeding.Guilds<-c("Predator.Percent","Predator.Richness", "ScraperGrazer.Percent","ScraperGrazer.Richness",
      "Shredder.Percent", "Shredder.Richness", "Filterer.Percent", "Filterer.Richness", "Gatherer.Percent",
      "Gatherer.Richness")
      
      #plot.data<-data.frame(categories=c("Community.Structure", "Biodiversity", "Sensitivity", "Hydrology", "Physical.Habitat", "Food.Web"), test=NA)
      plot.data<-data.frame(t(data.frame(max=rep(1,6),min=rep(0,6),test=NA)))
      colnames(plot.data)<-c("Community.Structure", "Biodiversity", "Sensitivity", "Hydrology", "Habitat.Guilds", "Feeding.Guilds")
      rownames(plot.data)[3]<-input$in_test_site_select
      
      for (i in as.character(colnames(plot.data))) {
        try1<-try(eval(parse(text=paste0("data1<-z.scores[,colnames(z.scores)%in%",i, "]"))),silent=T)
        if(class(try1)=="try-error"){
          next()
        }
        eval(parse(text=paste0("data1<-z.scores[,colnames(z.scores)%in%",i, "]")))
        if (is.data.frame(data1)){
          are.nas<-unlist(apply(data1,2, function(x) !any(is.na(x)))) & unlist(apply(data1,2, function(x) IQR(x)>0))
          if (sum(are.nas)==0) {
            next()
          }
          if(sum(are.nas)==1){
            data1<-data1[,are.nas]
            tsa.dist<-abs(data1[length(data1)]-mean(data1[1:(length(data1)-1)]))*sqrt(nRef)
            tsa.lambda<-qchisq(0.05,1, ncp = 0, lower.tail = FALSE, log.p = FALSE)*(nRef/2)
            tsa.NCPinterval<-1-pf(tsa.dist, 1, (nRef-1), tsa.lambda, log=FALSE)
            plot.data[3,i]<-tsa.NCPinterval
          } else {
            tsa.dist<-mahalanobis(data1[nrow(data1),are.nas],apply(data1[1:(nrow(data1)-1),are.nas],2,mean),cov(data1[1:(nrow(data1)-1),are.nas]))
            tsa.lambda<-qchisq(0.05,ncol(data1), ncp = 0, lower.tail = FALSE, log.p = FALSE)*(nRef/2)
            tsa.F<-((nRef-ncol(data1))*nRef*tsa.dist)/(ncol(data1)*(nRef-1))
            tsa.NCPinterval<-1-pf(tsa.F, ncol(data1), (nRef-ncol(data1)), tsa.lambda, log=FALSE)
            plot.data[3,i]<-tsa.NCPinterval
          }
        } else {
          are.nas<-!any(is.na(data1))
          if (sum(are.nas)==0) {
            next()
          }
          tsa.dist<-abs(data1[length(data1)]-mean(data1[1:(length(data1)-1)]))*sqrt(nRef)
          tsa.lambda<-qchisq(0.05,1, ncp = 0, lower.tail = FALSE, log.p = FALSE)*(nRef/2)
          tsa.NCPinterval<-1-pf(tsa.dist, 1, (nRef-1), tsa.lambda, log=FALSE)
          plot.data[3,i]<-tsa.NCPinterval
        }
      }
      plot.data<-plot.data[,!is.na(plot.data[3,])]
      validate(need(ncol(plot.data)>2,"Insufficient Metrics selected"))
      p1<-fmsb::radarchart(plot.data, axistype=1 , 
                  
                  #custom polygon
                  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                  
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,length.out=5), cglwd=0.8,
                  
                  #custom labels
                  vlcex=0.8 
      )
      p1
    }
  })
  
  output$tsa.pcoa.plot <- renderPlot( {
    print(tsa.pcoa.plot.raw())
  })
  
  output$tsa.pcoa.download<-downloadHandler(
    filename = function() {
      paste0("TSAPCOAplot.pdf", sep="")
    },
    content = function(file) {
      pdf(file, height=4,width=5.33334)
      tsa.object<-tsa.results$output.list
      
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
      dev.off()
    }
  )
  
  tsa.pcoa.plot.raw<-reactive({
    validate(need(!is.null(tsa.results$data),""))
    validate(need(class(tsa.results$data)!="try-error", ""))
    
    if (input$in_test_site_select!="None"){
      #tsa.object<-tsa.results$output.list[which(names(tsa.results$output.list)%in%input$in_test_site_select)]
      #tsa.object<-tsa.object[[1]]
      tsa.object<-tsa.results$output.list
      
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
  
  output$tsa.ca.plot <- renderPlot({
    validate(need(!is.null(tsa.results$data),""))
    validate(need(class(tsa.results$data)!="try-error", ""))
    
    if (input$in_test_site_select!="None"){
      temp<-all.data$data
      colnames(temp)<-gsub(".",";",colnames(temp),fixed = T)
      
      Test<-temp[input$in_test_site_select,colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
      ref.set<-nn.sites$data$TF.matrix[rownames(Test),]
      Reference<-temp[names(ref.set)[ref.set==T],colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
      nRef<-nrow(Reference)
      raw.data<-rbind(Reference,Test)
      pRef<-colSums(decostand(Reference,"pa"))/nrow(Reference)
      
      ca.ord<-vegan::cca(log(raw.data[,names(which(pRef>=0.05))]+1))
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
  
  output$tsa.ca.plot.download<-downloadHandler(
    filename = function() {
      paste0("TSACAplot.pdf", sep="")
    },
    content = function(file) {
      pdf(file, height=4,width=5.33334)
      temp<-all.data$data
      colnames(temp)<-gsub(".",";",colnames(temp),fixed = T)
      
      Test<-temp[input$in_test_site_select,colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
      ref.set<-nn.sites$data$TF.matrix[rownames(Test),]
      Reference<-temp[names(ref.set)[ref.set==T],colnames(temp)%in%colnames(bio.data$data$Raw.Data)]
      nRef<-nrow(Reference)
      raw.data<-rbind(Reference,Test)
      pRef<-colSums(decostand(Reference,"pa"))/nrow(Reference)
      
      ca.ord<-vegan::cca(log(raw.data[,names(which(pRef>=0.05))]+1))
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
      dev.off()
    }
  )
  

  #########################################################
  #Data summaries Table
  #########################################################
  datasum_table<-reactiveValues(data=NULL)
  
  output$datsum_tabresponse<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    if(input$metdata==F){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Taxa=colnames(all.data$data)[colnames(all.data$data)%in%colnames(taxa.by.site$data.alt.colnames)],
        Feeding_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(feeding.data$data.reduced)],
        Habitat_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.data$data)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")]
      )
    }
    if(input$metdata==T){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")]
      )
    }
    selectInput("datsum_tabresponse_in",label="Response",choices=c("Choose",avail.params),multiple = FALSE)
  })
  
  output$datsum_tabgrpfct1<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    avail.params<-list(
      Site_ID=site.ID.cols$data,
      Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
    )
    selectInput("datsum_tabgrpfct1_in",label="Group Factor 1",choices=c("Choose",avail.params),multiple = FALSE)
  })
  output$datsum_tabgrpfct2<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    avail.params<-list(
      Site_ID=site.ID.cols$data,
      Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
    )
    selectInput("datsum_tabgrpfct2_in",label="Group Factor 2",choices=c("Choose",avail.params),multiple = FALSE)
  })
  output$datsum_tabgrpfct3<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    avail.params<-list(
      Site_ID=site.ID.cols$data,
      Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
    )
    selectInput("datsum_tabgrpfct3_in",label="Group Factor 3",choices=c("Choose",avail.params),multiple = FALSE)
  })
  
  output$datsumtable<-renderDataTable({
    validate(
      need(!is.null(all.data$data),"")
    )
    validate(
      need(!is.null(input$datsum_tabgrpfct1_in),"Select Response and at least 1 grouping factor")
    )
    fact1<-NA
    fact2<-NA
    fact3<-NA
    if(input$datsum_tabgrpfct1_in!="Choose") {fact1<-as.factor(all.data$data[,input$datsum_tabgrpfct1_in])}
    if(input$datsum_tabgrpfct2_in!="Choose") {fact2<-as.factor(all.data$data[,input$datsum_tabgrpfct2_in])}
    if(input$datsum_tabgrpfct3_in!="Choose") {fact3<-as.factor(all.data$data[,input$datsum_tabgrpfct3_in])}
    fun1<-function(x) cbind(sum(x,na.rm=T),mean(x,na.rm=T),sd(x,na.rm=T),min(x,na.rm=T),max(x,na.rm=T),quantile(x,0.05,na.rm=T),
                            quantile(x,0.25,na.rm=T),quantile(x,0.50,na.rm=T),quantile(x,0.75,na.rm=T),quantile(x,0.95,na.rm=T))
                
    if(input$datsum_tabgrpfct1_in!="Choose" & input$datsum_tabresponse_in!="Choose"){
      fact.all<-list(fact1,fact2,fact3)
      fact.all<-fact.all[!is.na(fact.all)]
      names<-c(input$datsum_tabgrpfct1_in,input$datsum_tabgrpfct2_in,input$datsum_tabgrpfct3_in,
               "Sum","Mean","SD","Min","Max","5th Percentile","25th Percentile","50th Percentile","75th Percentile","95th Percentile")
      names<-names[names!="Choose"]
      if (input$datsum_tab_usetrans){
        dat1<-all.data$data[,input$datsum_tabresponse_in]
      } else {
        dat1<-all.data$untransformed[,input$datsum_tabresponse_in]
      }
      table<-aggregate(dat1, by=fact.all, FUN=fun1,simplify=T)
      table<-cbind(table[1:length(which(!is.na(list(fact1,fact2,fact3))))],table$x)
      colnames(table)<-c(names)
      datasum_table$data<-table
      
      DT::datatable(table,filter="top",selection="none",rownames=F, options=list(pageLength = 10,scrollX=T))
    }
  })
  
  output$download_datasum_table<-downloadHandler(filename = function() { paste("Summary-",input$inrawbioFile[1], sep='') },
                                            content = function(file) {write.csv(datasum_table$data,file,row.names = T)})
  
  
  #########################################################
  #Data summaries Scatterplot
  #########################################################
  output$datsum_scatx<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    if(input$metdata==F){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Taxa=colnames(all.data$data)[colnames(all.data$data)%in%colnames(taxa.by.site$data.alt.colnames)],
        Feeding_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(feeding.data$data.reduced)],
        Habitat_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.data$data)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")]
      )
    }
    if(input$metdata==T){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")]
      )
    }
    selectInput("datsum_scatx_in",label="x-variable",choices=c("Choose",avail.params),multiple = FALSE)
  })
  
  output$datsum_scaty<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    if(input$metdata==F){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Taxa=colnames(all.data$data)[colnames(all.data$data)%in%colnames(taxa.by.site$data.alt.colnames)],
        Feeding_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(feeding.data$data.reduced)],
        Habitat_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.data$data)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")]
      )
    }
    if(input$metdata==T){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")]
      )
    }
    selectInput("datsum_scaty_in",label="y-variable",choices=c("Choose",avail.params),multiple = FALSE)
  })
  
  output$datsum_scatgroup<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    avail.params<-list(
      Site_ID=site.ID.cols$data,
      Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
    )
    selectInput("datsum_scatgroup1_in",label="Grouping",choices=c("Choose",avail.params),multiple = FALSE)
  })
  output$datsum_scatcol<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    avail.params<-list(
      Site_ID=site.ID.cols$data,
      Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
    )
    selectInput("datsum_scatcol_in",label="Colour",choices=c("Choose",avail.params),multiple = FALSE)
  })
  
  dastum_scatplot.raw<-reactive({
    validate(
      need(!is.null(all.data$data),"")
    )
    validate(
      need(!is.null(input$datsum_scatx_in) & !is.null(input$datsum_scaty_in),"Select x and y variables")
    )
    
    if (input$datsum_scat_usetrans){
      dat1<-all.data$data
    } else {
      dat1<-all.data$untransformed
    }
    
    xvar<-if(input$datsum_scatx_in!="Choose") input$datsum_scatx_in else NA
    yvar<-if(input$datsum_scaty_in!="Choose") input$datsum_scaty_in else NA
    colvar<-if(input$datsum_scatcol_in!="Choose") input$datsum_scatcol_in else NA
    groupvar<-if(input$datsum_scatgroup1_in!="Choose") input$datsum_scatgroup1_in else NA
    scalevar<-if(input$datsum_scatscales=="Free") {"free"} else
      if(input$datsum_scatscales=="Both Fixed") {"fixed"} else
        if(input$datsum_scatscales=="Fixed x") {"free_y"} else
          if(input$datsum_scatscales=="Fixed y") {"free_x"} else
            
    validate(
      need(!is.na(xvar)&!is.na(yvar),"")
    )
    if (!is.na(xvar)&!is.na(yvar)){
      p<-ggplot2::ggplot(dat1, aes_string(x=paste0(xvar),y=paste0(yvar))) + geom_point() +theme_bw()
      if(!is.na(colvar)){
        p<-ggplot2::ggplot(dat1, aes_string(x=xvar,y=yvar,colour=paste0(colvar))) + geom_point() +theme_bw()
      }
      if(input$datsum_scattrend=="Linear"){
        p<-p+geom_smooth(method=lm)
      } else if (input$datsum_scattrend=="Loess"){
        p<-p+geom_smooth(method=loess)
        
      }
      if (!is.na(groupvar)){
        p<-p+facet_wrap(as.formula(paste0("~",groupvar)),scales=scalevar)
      }
      #p
    } else {
      p<-ggplot()+theme_bw()
    }
    p
  })
  
  output$dastum_scatplot <- renderPlot({
    validate(
      need(!is.null(input$datsum_scatx_in) & !is.null(input$datsum_scaty_in),"Select x and y variables")
    )
    if(!is.null(input$datsum_scatx_in) & !is.null(input$datsum_scaty_in)){
      print(dastum_scatplot.raw())
    }
  })
  
  output$download_datasum_scatter<-downloadHandler(
    filename = function() {
      paste0(input$datsum_scatx_in," v ", input$datsum_scaty_in,".pdf", sep="")
    },
    content = function(file) {
      pdf(file, height=8,width=8)
      print(dastum_scatplot.raw())
      dev.off()
    }
  )
  #########################################################
  #Data summaries Boxplots
  #########################################################
  output$datsum_boxy<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    if(input$metdata==F){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Taxa=colnames(all.data$data)[colnames(all.data$data)%in%colnames(taxa.by.site$data.alt.colnames)],
        Feeding_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(feeding.data$data.reduced)],
        Habitat_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.data$data)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")]
      )
    }
    if(input$metdata==T){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")]
      )
    }
    selectInput("datsum_boxx_in",label="y-variable",choices=c("Choose",avail.params),multiple = FALSE)
  })
  
  output$datsum_boxx<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    avail.params<-list(
      Site_ID=site.ID.cols$data,
      Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
    )
    selectInput("datsum_boxy_in",label="x-variable",choices=c("Choose",avail.params),multiple = FALSE)
  })
  
  output$datsum_boxgroup<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    avail.params<-list(
      Site_ID=site.ID.cols$data,
      Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
    )
    selectInput("datsum_boxgroup1_in",label="Grouping",choices=c("Choose",avail.params),multiple = FALSE)
  })
  
  output$datsum_box_addpoint<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    avail.params<-list(
      Site_ID=site.ID.cols$data,
      Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
      Samples=rownames(all.data$data)
    )
    
    selectInput("datsum_boxaddpoint_in",label="View individual sample",choices=c("Choose",avail.params),multiple = FALSE)
  })
  
  output$datsum_box_addpointgroup<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    
    if(any(input$datsum_boxaddpoint_in%in%site.ID.cols$data|
           input$datsum_boxaddpoint_in%in%colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)])){
      avail.params1<-as.character(all.data$data[,input$datsum_boxaddpoint_in])
      selectInput("datsum_boxaddpointgroup_in",label="Level",choices=c("Choose",avail.params1),multiple = FALSE)
    }
  })

  dastum_boxplot.raw<-reactive({
    validate(
      need(!is.null(all.data$data),"")
    )
    validate(
      need(!is.null(input$datsum_boxx_in) & !is.null(input$datsum_boxy_in),"Select x and y variables")
    )
    
    if (input$datsum_box_usetrans){
      dat1<-all.data$data
    } else {
      dat1<-all.data$untransformed
    }
    
    xvar<-if(input$datsum_boxx_in!="Choose") input$datsum_boxx_in else NA
    yvar<-if(input$datsum_boxy_in!="Choose") input$datsum_boxy_in else NA
    groupvar<-if(input$datsum_boxgroup1_in!="Choose") input$datsum_boxgroup1_in else NA
    sample_point<-if(input$datsum_boxaddpoint_in!="Choose") input$datsum_boxaddpoint_in else NA
    scalevar<-if(input$datsum_boxscales=="Free") {"free"} else
      if(input$datsum_boxscales=="Both Fixed") {"fixed"} else
        if(input$datsum_boxscales=="Fixed x") {"free_y"} else
          if(input$datsum_boxscales=="Fixed y") {"free_x"}
    
    if(any(sample_point%in%c(colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],site.ID.cols$data))) { 
      sample_pointgroup <- if(input$datsum_boxaddpointgroup_in!="Choose") as.character(input$datsum_boxaddpointgroup_in) else NA
    } else {
      sample_pointgroup <- NA
    }
    
    if(!is.na(sample_pointgroup)){
      subsamp<-dat1[as.character(dat1[,sample_point])==sample_pointgroup,]
    }

    validate(
      need(!is.na(xvar),"")
    )
    if (!is.na(xvar)){
      p<-ggplot2::ggplot(dat1) + geom_boxplot(aes_string(y=paste0(xvar),x=paste0(NA)))+
        theme(axis.text.x = element_blank()) +xlab("") #+theme_bw()
      if(!is.na(sample_point)){
        if(is.na(sample_pointgroup)){
          p<-p+geom_point(data=dat1[sample_point,],aes_string(y=paste0(xvar),x=paste0(NA)),col="red",size=3)
        } else {
          p<-p+geom_point(data=subsamp,aes_string(y=paste0(xvar),x=paste0(NA)),col="red",size=3)
        }
      }
      if (!is.na(groupvar)){
        p<-p+facet_wrap(as.formula(paste0("~",groupvar)),scales=scalevar)
      }
      
      if (!is.na(xvar)&!is.na(yvar)){
        p<-ggplot2::ggplot(dat1) + geom_boxplot(aes_string(y=paste0(xvar),x=paste0(yvar))) #+theme_bw()
        if(!is.na(sample_point)){
          if(is.na(sample_pointgroup)){
            p<-p+geom_point(data=dat1[sample_point,],aes_string(y=paste0(xvar),x=paste0(yvar)),col="red",size=3)
          } else {
            p<-p+geom_point(data=subsamp,aes_string(y=paste0(xvar),x=paste0(yvar)),col="red",size=3)
          }
        }
        if (!is.na(groupvar)){
          p<-p+facet_wrap(as.formula(paste0("~",groupvar)),scales=scalevar)
        }
        #p
      }
    }   else {
      p<-ggplot()+theme_bw()
    }
    p
  })
  
  output$dastum_boxplot <- renderPlot({
    validate(
      need(!is.null(input$datsum_boxx_in) & !is.null(input$datsum_boxy_in),"Select x and y variables")
    )
    if(!is.null(input$datsum_boxx_in) & !is.null(input$datsum_boxy_in)){
      print(dastum_boxplot.raw())
    }
  })
  
  output$download_datasum_box<-downloadHandler(
    filename = function() {
      paste0(input$datsum_boxx_in," v ", input$datsum_boxy_in,".pdf", sep="")
    },
    content = function(file) {
      pdf(file, height=20,width=20)
      print(dastum_boxplot.raw())
      dev.off()
    }
  )
  
  #########################################################
  #Data summaries Piecharts
  #########################################################
  
  
  #########################################################
  #Mapping
  #########################################################


  output$out.map_chart_variables<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    if(input$metdata==F){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")],
        Feeding_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(feeding.data$data.reduced)],
        Habitat_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.data$data)],
        Taxa=colnames(all.data$data)[colnames(all.data$data)%in%colnames(taxa.by.site$data.alt.colnames)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
      )
    }
    if(input$metdata==T){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
      )
    }
    
    selectInput("in.map_chart_variables",label="Chart Variables",choices=avail.params,multiple = FALSE)
    
    if (input$map_pointtype=="Points") {
      l1<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        #Feeding_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(feeding.data$data.reduced)],
        #Habitat_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.data$data)],
        Taxa=colnames(all.data$data)[colnames(all.data$data)%in%colnames(taxa.by.site$data.alt.colnames)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)],
        Reference=colnames(all.data$data)[colnames(all.data$data)%in%reftest.ID.cols$data],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")]
      )
    }
    if (input$map_pointtype=="Pie") {
      l1<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Feeding_Abundances=colnames(all.data$data)[colnames(all.data$data)%in%colnames(feeding.data$data.reduced)],
        Habitat_Abundances=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.data$data)],
        Taxa=colnames(all.data$data)[colnames(all.data$data)%in%colnames(taxa.by.site$data.alt.colnames)]
      )
    }
    selectInput("in.map_chart_variables",label="Chart Variables",choices=l1,multiple = if (input$map_pointtype=="Pie"){TRUE} else {FALSE})
  })
  
  output$out.map_time_variables<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    validate(
      need(input$time.ID!="","")
    )
    radioButtons("in.map_time_variables",label="Time",
                 choices=c("All",unique(all.data$data[,input$time.ID])),
                 selected="All")
  })
  
  map.data<-reactiveValues(map.null=NULL,map.mod=NULL)
  
  observeEvent(c(input$finalize_raw,
                 input$calculate_metrics,
                 input$apply.trans,
                 input$apply.trans.batch,
                 input$habitat_convert_fact_to_numb,
                 input$habitat_convert_numb_to_fact,
                 input$tsa_batch_go),
               {
                 validate(
                   need(passed.validation$pass,"Data must pass validation")
                 )
                 
                 validate(
                   need(!is.null(coordinates.by.site$data.unique),"")
                 )
                 
                 map.coordinates<-all.data$data
                 
                 if(is.null(is.null(map.coordinates$east))){
                   eval(parse(text=paste0("coordinates(map.coordinates) <- ~",paste0(coord.ID.cols$east),"+",paste0(coord.ID.cols$north))))
                 } else {
                   coordinates(map.coordinates) <- ~east+north
                 }
                 proj4string(map.coordinates)<-CRS("+init=epsg:4326")

                 #Map = sf::st_as_sf(map.coordinates)
                 Map = map.coordinates
                 map.data$map.null<-Map
               })
  
  observeEvent(c(input$in.map_chart_variables
                 ,input$in.map_time_variables),
               {
                 validate(
                   need(!is.null(coordinates.by.site$data.unique),"")
                 )
                 
                 map.coordinates<-map.data$map.null

                 if (input$time.ID!="" && !is.null(input$in.map_time_variables)){
                   if (input$in.map_time_variables!="All"){
                     map.coordinates<-subset(map.coordinates,map.coordinates@data[,input$time.ID]==input$in.map_time_variables)
                     #map.coordinates<-map.coordinates[map.coordinates[,input$time.ID][[1]]==input$in.map_time_variables,input$in.map_chart_variables]
                     #map.coordinates<-subset(map.coordinates,!is.na(map.coordinates[,input$in.map_chart_variables][[1]]))
                     map.coordinates<-subset(map.coordinates,!is.na(map.coordinates@data[,input$in.map_chart_variables]))
                   } else {
                     #map.coordinates<-subset(map.coordinates,map.coordinates@data[,input$in.map_chart_variables])
                     #map.coordinates<-map.coordinates[,input$in.map_chart_variables]
                     #map.coordinates<-subset(map.coordinates,!is.na(map.coordinates[,input$in.map_chart_variables][[1]]))
                     #map.coordinates<-subset(map.coordinates,!is.na(map.coordinates[,input$in.map_chart_variables]))
                   }
                 }
                 
                 #Map = sf::st_as_sf(map.coordinates)
                 Map = map.coordinates
                 map.data$map.mod<-Map
               })
  
  output$mymap<-renderLeaflet({
    validate(
      need(!is.null(coordinates.by.site$data.unique),"")
    )
    #validate(
    #  need(input$time.ID!=""&&!is.null(input$in.map_time_variables),"")
    #)
    
    validate(
      need(!is.null(map.data$map.mod),"Generating Map...")
    )
    
    Map<-map.data$map.mod
    
    #mapviewOptions(legend = input$map_legend, legend.pos="topright")
    
    #m <- mapview(map.types=c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap", "CartoDB.Positron", "CartoDB.DarkMatter"))
    
    if (input$map_pointtype=="Points"){
      m<- #m + 
        mapview(Map,zcol=paste0(input$in.map_chart_variables),na.color ="grey10",position="topright",legend=input$map_legend,
                      col.region=colorRampPalette(brewer.pal(9, input$map_pointcol))
                ,map.types=c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap", "CartoDB.Positron", "CartoDB.DarkMatter")
      )
    }
    if (input$map_pointtype=="Pie"){
          m<-addMinicharts(
            map = mapview(map.types=c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap", "CartoDB.Positron", "CartoDB.DarkMatter")),
            lng=all.data$data$east,
            lat=all.data$data$north,
            layerId=all.data$data[,colnames(all.data$data)%in%site.ID.cols$data & !colnames(all.data$data)%in%input$time.ID],
            #width = input$map_chart_site,
            #height = input$map_chart_site,
            #maxValues<-aggregate(feeding.data$data.reduced,by=list(coordinates.by.site$data.all[,input$time.ID]),max),
            type="pie",
            chartdata=all.data$data[,colnames(all.data$data)%in%input$in.map_chart_variables],
            time=all.data$data[,input$time.ID],
            showLabels = T,
            legendPosition = "bottomleft"#,
            #colorPalette=colorRamps::primary.colors(ncol(data[colnames(data)%in%colnames(feeding.data$data.reduced)]))
          )
      
    }
    
    
    m@map
  })
  
  #output$mymap <- renderLeaflet({
  #  validate(
  #    need(!is.null(coordinates.by.site$data.unique),"")
  #  )
  #  m <- leaflet()
  #  if (input$basemap_input=="Street"){
  #    #m <- addTiles(m)
  #    m<-addProviderTiles(map=m,
  #                        provider=providers$Esri.WorldTopoMap)
  #  }
  #  if (input$basemap_input=="Satellite"){
  #    m<-addProviderTiles(map=m,
  #                        provider=providers$Esri.WorldImagery)
  #    
  #    #m <- addTiles(m,urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google')
  #  }
  #  if (input$map_admin==T){
  #    m<-addProviderTiles(map=m,
  #                        provider=providers$OpenMapSurfer.AdminBounds)
  #  }
  #  
  #  if (input$map_pointtype=="pie"|input$map_pointtype=="bar"){
  #    m<-addMinicharts(
  #      map = m,
  #      lng=all.data$data$east,
  #      lat=all.data$data$north,
  #      layerId=all.data$data[,colnames(all.data$data)%in%site.ID.cols$data & !colnames(all.data$data)%in%input$time.ID],
  #      width = input$map_chart_site,
  #      height = input$map_chart_site,
  #      #maxValues<-aggregate(feeding.data$data.reduced,by=list(coordinates.by.site$data.all[,input$time.ID]),max),
  #      type=input$map_pointtype,
  #      chartdata=all.data$data[,colnames(all.data$data)%in%input$in.map_chart_variables],
  #      time=all.data$data[,input$time.ID],
  #      showLabels = T,
  #      legendPosition = "bottomleft"#,
  #      #colorPalette=colorRamps::primary.colors(ncol(data[colnames(data)%in%colnames(feeding.data$data.reduced)]))
  #    )
  #  }
  #  
  #  if (input$map_pointtype=="Points"){
  #    m <- addMarkers(map=m,
  #                    lng=all.data$data$east,
  #                    lat=all.data$data$north,
  #                    label=rownames(coordinates.by.site$data.unique)
  #    )
  #  }
  #  
  #  m
  #  })
  

  
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
  #Reports
  #########################################################
  #########################################################
  #   Define element areas
  #########################################################
  
  #report_layout_table<-reactiveValues(data=NULL, orientation=NULL, height=NULL,width=NULL,row.height=NULL,col.width=NULL)
  report_layout_table<-reactiveValues(null.data=NULL
                                      ,orientation=NULL
                                      ,cell.widths=NULL
                                      ,cell.heights=NULL
                                      ,user.groups=NULL
                                      ,px.height=NULL
                                      ,px.width=NULL
                                      ,element.numbers=NULL
                                      )
  report_grid_thresholds<-reactiveValues(breaks=seq(-0.04,1.04,length.out=12))
  
  observeEvent(input$report_orientation,{
    report_layout_table$orientation<-input$report_orientation
    if (report_layout_table$orientation=="Portrait"){
      report_layout_table$null.data<-matrix(1:144,nrow=12,byrow=T)
      report_layout_table$cell.widths<-21.59/ncol(report_layout_table$null.data)*0.75
      report_layout_table$cell.heights<-27.94/nrow(report_layout_table$null.data)*0.75
      report_layout_table$px.height<-610
      report_layout_table$px.width<-470
    }
    if (report_layout_table$orientation=="Landscape"){
      report_layout_table$null.data<-matrix(1:144,nrow=12,byrow=T)
      report_layout_table$cell.widths<-27.94/ncol(report_layout_table$null.data)*0.75
      report_layout_table$cell.heights<-21.59/nrow(report_layout_table$null.data)*0.75
      report_layout_table$px.height<-470
      report_layout_table$px.width<-610
    }
  })
  output$report_test<-renderUI({
    validate(need(!is.null(report_layout_table$orientation),"Select Orientation"))
    plotOutput("report_layout",brush = "report_brush",hover = "plot_hover",height=report_layout_table$px.height,width=report_layout_table$px.width)
  })
  
  output$report_layout<-renderPlot({
    validate(need(!is.null(report_layout_table$orientation),"Select Orientation"))
    par(mar=c(0,0,0,0))
    plot.layout<-layout(report_layout_table$null.data
                        ,widths=rep(lcm(report_layout_table$cell.widths),ncol(report_layout_table$null.data))
                        ,heights=rep(lcm(report_layout_table$cell.heights),nrow(report_layout_table$null.data))
                        ,respect=T)
    layout.show(plot.layout)
  })

  observeEvent(input$apply_report_item,{

    breaks<-classInt::classIntervals(seq(0.06,0.93,length.out=1000),n=10,style="equal")$brks
    x<-which(c(breaks>input$report_brush$xmin,T) & c(T,breaks<input$report_brush$xmax))
    y<-which(c(breaks>input$report_brush$ymin,T) & c(T,breaks<input$report_brush$ymax))
    y<-13-c(y)
    
    if (length(x)==1 & length(y)==1){
      showModal(modalDialog(title="Error",
                            helpText("Minimum report element size must be bigger than 1/144 the size of the page (i.e. 1 cell)"),
                            easyClose = T,
                            size="s"
      ))
      session$resetBrush("report_brush")
      validate(need(F,""))
    }
    
    previous.elements<-unique(as.vector(report_layout_table$null.data)[which(duplicated(as.vector(report_layout_table$null.data)))])
    previous.elements<-previous.elements[order(previous.elements,decreasing=T)]
    
    focus.obj.num<-max(report_layout_table$null.data)+1
    report_layout_table$null.data[y,x]<-focus.obj.num
    mat.red<-max(report_layout_table$null.data)-(length(x)*length(y))
    
    if (any(previous.elements%in%as.vector(report_layout_table$null.data[y,x]))){
      showModal(modalDialog(title="Error",
                            helpText("Cannot overlap report elements"),
                            easyClose = T,
                            size="s"
      ))
      session$resetBrush("report_brush")
      validate(need(F,"Cannot overlap report elements"))
    }
    
    number<-1
    for(i in 1:12){
      for (n in 1:12){
        val<-report_layout_table$null.data[i,n]
        if(val%in%previous.elements) {
          report_layout_table$null.data[i,n]<-mat.red-which(val==previous.elements)
        } else if(val==focus.obj.num) {
          report_layout_table$null.data[i,n]<-mat.red
        } else {
          report_layout_table$null.data[i,n]<-c(1:mat.red)[number]
          number<-number+1
        }
      }
    }
    
    report_layout_table$element.numbers<-unique(as.vector(report_layout_table$null.data)[which(duplicated(as.vector(report_layout_table$null.data)))])
    report_layout_table$element.numbers<-report_layout_table$element.numbers[order(report_layout_table$element.numbers,decreasing=F)]
    
    if(!is.null(report_layout_table$element.numbers) & length(report_layout_table$element.numbers)>0){
      output$populate_report_element<-reactive({TRUE})
      outputOptions(output, "populate_report_element", suspendWhenHidden = FALSE)
    }
    
    session$resetBrush("report_brush")
  })
  #########################################################
  #   Define element contents
  #########################################################
  output$out.report_element_number<-renderUI({
    validate(
      need(!is.null(report_layout_table$element.numbers),"")
    )
    selectInput("in.Report_element_number",label="Report Element Type",
                choices=report_layout_table$element.numbers,
                multiple = FALSE)
  })
  
  output$out.report_element_type<-renderUI({
    avail.params<-c("None","Text","Table","Figure")
    if (any(colnames(all.data$data)=="EPSG")){
      avail.params<-c(avail.params,"Map")
    }
    selectInput("in.Report_element_type",label="Report Element Type",
                choices=avail.params,
                multiple = FALSE)
  })
  
  output$out.report_element_content<-renderUI({
    validate(
      need(!is.null(all.data$data),"")
    )
    if(input$metdata==F){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")],
        Feeding_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(feeding.data$data.reduced)],
        Habitat_Groups=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.data$data)],
        Taxa=colnames(all.data$data)[colnames(all.data$data)%in%colnames(taxa.by.site$data.alt.colnames)],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
      )
    }
    if(input$metdata==F){
      avail.params<-list(
        Summary_Metrics=colnames(all.data$data)[colnames(all.data$data)%in%colnames(bio.data$data$Summary.Metrics)],
        Impairment=colnames(all.data$data)[colnames(all.data$data)%in%c("TSA.Impairment","Test.Site.D2")],
        Habitat=colnames(all.data$data)[colnames(all.data$data)%in%colnames(habitat.by.site$data)]
      )
    }
    
  })
  
  #output$plot_hoverinfo <- renderPrint({
  #  cat("input$plot_hover:\n")
  #  str(input$plot_hover)
  #})
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
  observeEvent(
    c(input$raw.help,
      input$getting_started
    ), {
      if ((input$getting_started>0 | input$raw.help>0) & loggedin1$data==T){
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
          box(width=12,
              fluidRow(
                h4("1) Select your input file from the Data Input / Raw Data upload tab:"),
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
                h4("2) Select the format of your input data (see exampled above)"),
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
                h4(" 3) Highlight column(s) (multiple while holding shift) and assign them to the appropriate attributes"),
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
                h4("Additional tools may now be available from the sidebar depending on what data were specified:"),
                h4("- Indicator metrics can be further transformed from the Data Input / Metric Transformations tab"),
                h4("- If GIS coordinates and ESPG codes are specified, the Mapping tab will be functional"),
                h4("- If samples were classified as 'test' or 'reference' and habitat data or a matrix of user matched reference sites are supplied, the Integrity Assessments tab will be functional"),
                br()
              )
              
              ),
          
          footer = modalButton("Dismiss"),
          easyClose = TRUE
        ))
      }
    })
  
  output$download_ex_long<-downloadHandler(filename = function() { paste("Long_example.csv") },
                                            content = function(file) {write.csv(read.csv("Long_example.csv",header=T),file,row.names = F)})
  output$download_ex_wide1<-downloadHandler(filename = function() { paste("Wide_example1.csv") },
                                           content = function(file) {write.csv(read.csv("Wide_example1.csv",header=T),file,row.names = F)})
  output$download_ex_wide2<-downloadHandler(filename = function() { paste("Wide_example2.csv") },
                                           content = function(file) {write.table(read.csv("Wide_example2.csv",header=F),file,row.names = F,col.names=F, sep = ",")})
  
  
  #Raw Data input
  
  
  #########################################################
  #Server End
  ########################################################
  
})
