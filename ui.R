


header <- dashboardHeader(
  title = "Aquatic Analysis Online Tool - Development Version",
  titleWidth = 550,
  dropdownMenu(type = "notifications",
               notificationItem(
                 text = "Development version",
                 icon("question")
               )
               )
  )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
    menuItem("Data Input", icon = icon("th"), tabName = "data_input",
             menuItem("Raw Data upload",tabName = "rawdatainput",icon=NULL,
                         fileInput(inputId="inrawbioFile",multiple = FALSE, label = h5("Choose CSV file"),accept=c(".csv",".txt")),
                         checkboxInput("metdata",label=h5("Input data are metrics"),value=F),
                         radioButtons(inputId="rawFormat", label = h5("Input File Format"),choices = list("Long" ,"Wide" ),inline=T)
                         ),
             conditionalPanel(condition="input.rawFormat == 'Long'",
               menuItem("Specify fields",tabName = "Specifyfields",icon=NULL)
             )
             
             )
  )
)

body <- dashboardBody(
    tabItem(tabName = "introduction",
            h2("Dashboard tab content")
    ),
    tabItem(tabName = "rawdatainput",
            fluidRow(
              box(h2("plot"), width=12)
              ),
            fluidRow(
              box(h2("Describe below"), width=12)
            ),
            
            fluidRow(
              box(h2("Site Identifiers"),width=3),
              box(h2("Taxa Identifiers"),width=3),
              box(h2("Habitat Descriptors"),width=3),
              box(h2("Coordinates"),width=3)
            )
    )
)

dashboardPage(
  header,
  sidebar,
  body
)


