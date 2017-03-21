


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
             menuSubItem("Raw Data upload",tabName = "rawdata_input",
                         fileInput(inputId="inrawbioFile",multiple = FALSE, label = h4("Choose CSV file"),accept=c(".csv",".txt")),
                         checkboxInput("metdata",label="Input data are metrics",value=F),
                         radioButtons(inputId="rawFormat", label = h4("Input File Format"),choices = list("Long" ,"Wide" ),inline=T))
             )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "introduction",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "data_input",
            h2("Widgets tab content")
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)


