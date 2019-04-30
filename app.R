library(shiny)
require(pdftools)
require(stringr)
require(tm)
require(dplyr)
require(data.table)
require(openxlsx)

source("horsereportfunction.R")


ui<-fluidPage(
  titlePanel(h1("Add Pony")),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date",h4("Qualifying Period"),start='2018-12-01',end='2019-11-30'),
      br(),
      selectInput("division",h4("Division"),choices = list(
       "SMALL PONY" = "PONY HUNTER-SMALL",
        "MEDIUM PONY" = "PONY HUNTER-MEDIUM",
        "LARGE PONY" = "PONY HUNTER-LARGE",
        "SMALL GREEN" = "GREEN PONY HUNTER-SMALL",
        "MEDIUM GREEN" = "GREEN PONY HUNTER-MEDIUM",
        "LARGE GREEN" = "GREEN PONY HUNTER-LARGE"
      )),
      #br(),
      fileInput("horsereport",h4("Horse Report")),
      br(),
      fileInput("excelfile",h4("Points File"))
      #br(),
      #actionButton("goButton","Add Pony")
    ),
    mainPanel(
      textOutput("horse_div"),
      textOutput("qualifying"),
      textOutput("pdf")
      
    )
  )
)

server<-function(input,output){
  
  #observeEvent(input$goButton,{
  #  data$df<-horseRecord()
  #})
  
  output$horse_div <- renderText({ 
    paste("You have chosen",input$division)
    })
  
  output$qualifying<-renderText({
    paste("Qualifying Period from",input$date[1],"to",input$date[2])
  })
  
  
  output$pdf<-renderText({
    req(input$horsereport)
    req(input$excelfile)
    ponyoutput<-horseRecord(input$horsereport$datapath,input$date[1],input$date[2],input$division,input$excelfile$datapath)
    paste("mylist",ponyoutput[1])
  })
  
  
  
  
    
    
    
  
  
  #output$downloadExcel<-downloadHandler(
    #move excel instructions in here
  #)
}

# Run the app ----
shinyApp(ui = ui, server = server)