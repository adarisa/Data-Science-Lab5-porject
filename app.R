library(shiny)

song <- read.csv("Song.csv", stringsAsFactors = FALSE, header = TRUE)
patient <- read.csv("pt_data.csv", stringsAsFactors =  FALSE, header = TRUE)


ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Summary",
             titlePanel("Data Summary"),
             sidebarPanel(
               selectInput(inputId = "dataset",
               label = "Choose a data set:",
               choices = c("Song","Patient")),
               
               numericInput(inputId = "obs",
                            label = "Number of observations to view:",
                            value = 10)
               ),
             mainPanel(
               verbatimTextOutput("summary"),
               tableOutput("view")
                 
               )
             )
            
          ,
    tabPanel("Import",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Choose CSV File",
                           multiple = TRUE,
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 tags$hr(),
                 checkboxInput("header", "Header", TRUE),
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                              selected = ","),
                 radioButtons("quote", "Quote",
                              choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                              selected = '"'),
                 tags$hr(),
                 radioButtons("disp", "Display",
                              choices = c(Head = "head", All = "all"),
                              selected = "head")
               ),
               mainPanel(
                 tableOutput("contents") 
               ) 
             )
          ),
             
    tabPanel("Download file",
             titlePanel("Downloading Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset2", "Choose a dataset:",
                             choices = c("Song", "Patient")),
                 downloadButton("downloadData", "Download")
               ),
             mainPanel(
                 tableOutput("table")
               )
             )
          ),
    
    tabPanel("Timer",
             h2(textOutput("currentTime"))
       )
    )
  )



server <- function(input, output, session){
  datasetInput <- reactive({
    switch(input$dataset,
           "Song" = song,
           "Patient" = patient
           )
  })
  
 
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  datasetInput <- reactive({
    switch(input$dataset2,
           "Song" = song,
           "Patient" = patient
    )
  })
  
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset2, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput, file, row.names = FALSE)
    })
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
}

shinyApp(ui = ui, server = server)



