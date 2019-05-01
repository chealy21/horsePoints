library(shiny)
require(pdftools)
require(stringr)
require(tm)
require(dplyr)
require(data.table)
require(openxlsx)

source("horsereportsplitfunction.R")


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
      fileInput("excelfile",h4("Points File")),
      br(),
      downloadLink("downloadData","Download File")
    ),
    #Report Choices and Rank added
    mainPanel(
      textOutput("horse_div"),
      textOutput("qualifying"),
      textOutput("pdf"),
      h4("Shows"),
      tableOutput("shows")
      
    )
  )
)

server<-function(input,output){
  
  ponyoutput<-reactive({
    req(input$horsereport)
    req(input$excelfile)
    df<-horseRecord(input$horsereport$datapath)
    divisionReport(df,input$date[1],input$date[2],input$division)
  })
  
  ## Need to work on this!!!
  #ponyrank<-reactive({
  #  AddPonytoRank(ponyoutput()[1:3],input$excelfile$datapath,input$division)
  #})
  
  output$horse_div <- renderText({ 
    paste("You have chosen",input$division)
    })
  
  output$qualifying<-renderText({
    paste("Qualifying Period from",input$date[1],"to",input$date[2])
  })
  
  output$pdf<-renderText({
    paste(ponyoutput()[length(ponyoutput())-1],"has",ponyoutput()[length(ponyoutput())],"points")
  })
  
  output$shows<-renderTable({
    ponyoutput()[1:3]
  })
  
  output$downloadData<-downloadHandler(
    filename = function(){
      paste(input$excelfile$datapath,Sys.Date(),".xlsx")
    },
    content=function(file){
      AddPonytoRank(ponyoutput()[1:3],input$excelfile$datapath,input$division)
    }
  )
  
  #output$ranks<-renderText({
  #  paste("Ranks Table Sample:",head(ponyoutput()[1]))
  #})
  
  #output$shows<-renderText({
  #  paste("Shows Table Sample:",ponyoutput()[4][4])
  #})
  
  
  
  
    
    
    
  
  
  #output$downloadExcel<-downloadHandler(
    #move excel instructions in here
  #)
}

# Run the app ----
shinyApp(ui = ui, server = server)