library(shiny)
library(tidyverse)
library(data.table)
library(tidyverse)
library(dplyr)
options(shiny.maxRequestSize=30*1024^2) #increase server to 30 MB

ui <- fluidPage(
  titlePanel("Data Preparation App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csvs",
                label="Upload CSVs here",
                multiple = TRUE)
    ),
    mainPanel(
      h3("Total rows"),
      textOutput("count"),
      h3("This is example of your cleaned data"),
      tableOutput("preview"),
      downloadButton("download", "Download your clean CSV")
    )
  )
)

server <- function(input, output) {
  mycsvs<-reactive({
    if (is.null(input$csvs))
      return(NULL)
    mycsvs <- rbindlist(lapply(input$csvs$datapath, fread ),
              use.names = TRUE, fill = TRUE)
    
    colnames(mycsvs)<- c("Participant_PPID","Participant_Registration.Date","Participant_Gender" ,"Visit_Clinical.Diagnosis..Deprecated.","Visit_Name" ,"Specimen_Type" ,"Specimen_Anatomic.Site","Specimen_Collection.Date","Specimen_Barcode" ,"Specimen_Class" ,"Specimen_Pathological.Status" ,"Specimen_Container.Name","Specimen_Container.Position","Project" )
    #rename.values(data,'Buffy Coat'='Buffy_Coat') because SQL is sensitive
    mycsvs$Specimen_Type <- as.character(mycsvs$Specimen_Type)
    mycsvs[mycsvs == "Buffy Coat"] <- "Buffy_Coat"
    mycsvs[mycsvs == "Fresh Tissue"] <- "Fresh_Tissue"
    #extract year from datetime
    mycsvs$Year <- format(as.Date(mycsvs$Specimen_Collection.Date,format="%d/%m/%Y"),"%Y")
    #select only useful columns
    mycsvs <- select(mycsvs,Participant_PPID,Specimen_Type,Specimen_Pathological.Status,Project,Year,Participant_Gender)
  })
  
  
  output$count <- renderText(nrow(mycsvs()))
  
  output$preview <- renderTable({
    print(head(mycsvs()))
    #print(colnames(mycsvs))
  })
  
  output$download <- downloadHandler(
    filename = function(){"datasetTest.csv"}, 
    content = function(fname){
      write.csv(mycsvs(), fname)
    }
  )
  
  
}

shinyApp(ui = ui, server = server)