

library(shiny)
library(plotly)

countries<-read.csv("countries.csv")
shinyUI(fluidPage(
    titlePanel("COVID-19 vaccination rate"),
    p("This app uses data from", a("OWID", href="https://github.com/owid/covid-19-data"),
    "and displays the COVID-19 vaccination rate for 189 countries from 2021-12-15 to the most recent date 
    available, updating automatically evetytime it's launched.", em("(Data referring to the very last day are 
    discarded, as they're often incomplete).")),
    p("You are allowed to select all the countries you're interested in , or even all the 189 countries 
    (computation could take a while); you can also choose a date range and decide if you want to see", strong("the percentage of 
    people who received at least one vaccine dose ('All')"), "and/or", strong("the percentage of people who
    completed the vaccination course ('Fully').")), 
    p("In the", strong("Plot tab"), "you can visualize vaccination rates by date based on options you 
    choose, as well as the mean vaccinated percentage of global population (thicker lines/ shadowed area).
    You can hover on the plot for more details, and use tools in the top right corner of the plot to Zoom 
    and Download the plot as a PNG image."),
    p("In the", strong("Countries Data tab"), "you can find an interactive table with vaccination data 
      based on options you set. You can sort data by clicking on the column names and look
      for a specific entry with the search window. Use the 'Download as CSV' button if you want to save 
      selected data."),
    p("In the", strong("World Data tab"), "you can find an interactive table with mean global vaccination data 
      with your custom filters. You can again sort, filter and download data as a CSV file."),
    p("See", a("here", href="https://elenena.github.io/DevelopingDataProducts_CourseProject/Shiny_App_COVID_Presentation.html#/"), "for more details."),
    sidebarLayout(
        sidebarPanel(width=5,
          checkboxInput("world", strong("Check to display all countries")),
          conditionalPanel(condition="input.world==0",
          selectInput("where", "Select countries of interest  ", countries$x,
                multiple = T, selectize = TRUE)
            )),
        sidebarPanel(width=3, 
        checkboxGroupInput("doses", "Choose to display All vaccinated people or just Fully
                               vaccinated people", choices=c("All", "Fully"),
                               selected=c("All", "Fully"))
        )
    ),
    sidebarLayout(
      sidebarPanel(width=5,
      dateRangeInput("dates", "Select Dates", start=as.Date("2020-12-15"), end=(Sys.Date()-2),
                     min=as.Date("2020-12-15"), max=(Sys.Date()-2))
      ),
      mainPanel(width=3,
            column(width=12, align="center",h3(em("Data updated to ", textOutput("today", inline=T))))
            )
      ),
    
    sidebarLayout(
      sidebarPanel(width=12,
            tabsetPanel(
            tabPanel("Plot", plotlyOutput("prova")),
            tabPanel("Countries Data", br(),
                     column(width=12, align="center", downloadButton("downloadData", "Download as CSV")),
                     br(), 
                     dataTableOutput("data")),
            tabPanel("World Data", br(),
                     column(width=12, align="center", downloadButton("downloadDataW", "Download as CSV")),
                     br(),
                     dataTableOutput("wdata"))
            )
      ),
      mainPanel(width=3, h3(""))
    )
))
