### Calculate show points and add to excel ###

library(shiny)
library(shinyjs)
library(dplyr)
library(data.table)
require(stringr)
require(tm)
require(openxlsx)  

source("helper.R")


# Define UI ----
ui <- fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(width=6, useShinyjs(),id='side-panel',
        fluidRow(
          column(4,
          radioButtons("rating","Rating",choices=list("AA"="AA","A"="A","B"="B","C"="C"),inline=TRUE)
          #close row1 column1
          ),
          column(3,
          checkboxInput("cali",tags$b("Cali Split"),value=FALSE)
          #close row1 column2
          )
        #close fluidRow1
        ),
        fluidRow(
          column(4,
            selectInput("division","Division",choices = list(
              "SMALL PONY" = "PONY HUNTER-SMALL",
              "MEDIUM PONY" = "PONY HUNTER-MEDIUM",
              "LARGE PONY" = "PONY HUNTER-LARGE",
              "SMALL GREEN" = "GREEN PONY HUNTER-SMALL",
              "MEDIUM GREEN" = "GREEN PONY HUNTER-MEDIUM",
              "LARGE GREEN" = "GREEN PONY HUNTER-LARGE"
            ))
          #close row2 column1
          ),
          column(3,
                 numericInput("diventries","Division Entries",value=1)
          #close row2 column2  
          ),
          column(3,
                 numericInput("classicentries","Classic Entries",value=1)
          #close row2 column3
          )
        #close fluidrow2  
        ),
        fluidRow(
        column(12,
               actionButton("addmodel","Model"),
               actionButton("addconf","Confirmation"),
               actionButton("addhunter1","Hunter 1"),
               actionButton("addhunter2","Hunter 2"),
               actionButton("addhandy","Handy"),
               actionButton("addus","Under Saddle"),
               actionButton("addclassic","Classic")
               #close row3 column1
               )
        #close fluidrow3    
        ),
        fluidRow(
        tags$div(
          tags$p(id='model'),
          tags$p(id='conf'),
          tags$p(id='hunter1'),
          tags$p(id='hunter2'),
          tags$p(id='handy'),
          tags$p(id='us'),
          tags$p(id='classic')
        )
        #close fluidrow4
        ),
        fluidRow(
          column(10,radioButtons("championship","Champion/Reserve:",
                                 choices = list("Champion"=1,"Reserve"=2,"Neither"=0),inline=TRUE,selected=0)),
          
          column(4,actionButton("calculate","Calculate")),
          column(4,actionButton("reset","Reset"))
        #close fluidrow5
        )
      #close sidebarpanel
      ),
    
      
    mainPanel(width = 5,
      h1("Points for Show"),
      fluidRow(
        column(6,
      tableOutput("displayshows")
        ),
      column(5, offset = 1,
             textInput("ponyname","Pony Show Name:"),
             textInput("showname","Horse Show:"),
             dateInput("showdate","Competition Date:"),
             fileInput("excelfile","Excel File"),
             downloadButton("downloadData","Download"),
            actionButton("addtofile","Add Show to Excel"),
            textOutput("test"),
      br()#,
      #tags$div(id='excelfile')
      
      )
      #close fluidRow
      )
      
    #close mainpanel  
    )
  #close sidebar layout
  )
  )


# Define server logic ----
server <- function(input, output) {
  #Adjust for cali split
  observeEvent(input$cali,{
    req(input$diventries)
    if(input$cali){
      x<-ceiling(input$diventries/2)
      
      updateNumericInput(session,"diventries",value=x)
    }
  })
  
  #Add Classes
  observeEvent(input$addmodel,{
    modelbtn<-input$addmodel
    if(!(modelbtn[1] %% 2)==0){
    insertUI(
      selector='#model',
      ui=tags$div(
        column(2,numericInput("model","Model",value=0))
      )
    )} else{
      removeUI(
        selector='#model'
      )
    }
    
  })
  
  observeEvent(input$addconf,{
    confbtn<-input$addconf
    if(!(confbtn[1] %% 2)==0){
      insertUI(
        selector='#conf',
        ui=tags$div(
            column(2,numericInput("conf","Confirmation",value=0))
        )
      )} else{
        removeUI(
          selector='#conf'
        )
      }
    
  })
  
  observeEvent(input$addhunter1,{
    hunter1btn<-input$addhunter1
    if(!(hunter1btn[1] %% 2)==0){
      insertUI(
        selector='#hunter1',
        ui=tags$div(
            column(2,numericInput("hunter1","Hunter 1",value=0))
        )
      )} else{
        removeUI(
          selector='#hunter1'
        )
      }
    
  })
  
  observeEvent(input$addhunter2,{
    hunter2btn<-input$addhunter2
    if(!(hunter2btn[1] %% 2)==0){
      insertUI(
        selector='#hunter2',
        ui=tags$div(
            column(2,numericInput("hunter2","Hunter 2",value=0))
        )
      )} else{
        removeUI(
          selector='#hunter2'
        )
      }
    
  })
  
  observeEvent(input$addhandy,{
    handybtn<-input$addhandy
    if(!(handybtn[1] %% 2)==0){
      insertUI(
        selector='#handy',
        ui=tags$div(
            column(2,numericInput("handy","Handy",value=0))
        )
      )} else{
        removeUI(
          selector='#handy'
        )
      }
    
  })
  
  observeEvent(input$addus,{
    usbtn<-input$addus
    if(!(usbtn[1] %% 2)==0){
      insertUI(
        selector='#us',
        ui=tags$div(
            column(2,numericInput("us","U/S",value=0))
        )
      )} else{
        removeUI(
          selector='#us'
        )
      }
    
  })
  
  observeEvent(input$addclassic,{
    classicbtn<-input$addclassic
    if(!(classicbtn[1] %% 2)==0){
      insertUI(
        selector='#classic',
        ui=tags$div(
            column(2,numericInput("classic","Classic",value=0))
        )
      )} else{
        removeUI(
          selector='#classic'
        )
      }
    
  })
  
  
  modelreac<-reactive({
    if(is.null(input$model)){model<-0
    } else{model<-input$model}
  })
  
  confreac<-reactive({
    if(is.null(input$conf)){conf<-0
    } else{conf<-input$conf}
  })
  
  hunter1reac<-reactive({
    if(is.null(input$hunter1)){hunter1<-0
    } else{hunter1<-input$hunter1}
  })
  
  hunter2reac<-reactive({
    if(is.null(input$hunter2)){hunter2<-0
    } else{hunter2<-input$hunter2}
  })
  
  handyreac<-reactive({
    if(is.null(input$handy)){handy<-0
    } else{handy<-input$handy}
  })
  
  usreac<-reactive({
    if(is.null(input$us)){us<-0
    } else{us<-input$us}
  })
  
  classicreac<-reactive({
    if(is.null(input$classic)){classic<-0
    } else{classic<-input$classic}
  })
  
  showpointsoutput<-eventReactive(input$calculate,{
    pointsoutput<-calcpoints(input$rating,input$diventries,input$classicentries,modelreac(),confreac(),hunter1reac(),hunter2reac(),handyreac(),usreac(),classicreac(),input$championship)
    pointsoutput
  })
  
  output$test<-renderText({
    showtotal<-showpointsoutput()[nrow(showpointsoutput()),ncol(showpointsoutput())]
    paste("Total points:",showtotal)
  })
  
  output$displayshows<-renderTable({
    totals<-as.data.frame(showpointsoutput(),stringsAsFactors=FALSE)
    totals
  })
  
  observeEvent(input$reset,{
    reset("side-panel")
  })
  
  #Collect Info to add to excel file
  #observeEvent(input$addtofile,{
  #  insertUI(
  #    selector='#excelfile',
  #    ui=tags$div(
  #      textInput("ponyname","Pony Show Name:"),
  #      textInput("showname","Horse Show:"),
  #      dateInput("showdate","Competition Date:"),
  #      fileInput("excelfile","Excel File"),
  #      downloadButton("downloadData","Download")
  #    ))
  #})
  
  #NOT WORKING!
  addshows<-observeEvent(input$addtofile,{
    showpoints<-showpointsoutput()[nrow(showpointsoutput()),ncol(showpointsoutput())]
    addpointstofile(showpoints,input$division,input$showname,input$ponyname,input$showdate,input$excelfile$datapath)
  })
  
  output$downloadData<-downloadHandler(
    file = function(){
      filename<-strsplit(input$excelfile$name,"\\.")
      filename<-filename[[1]][1]
      filename<-paste0(filename,"_",Sys.Date())
      paste(filename,"xlsx",sep=".")
    },
    content = function(file){
      file.rename(input$excelfile$datapath,file)
    },
    contentType = "application/xlsx"
    
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)