library(shiny)
library(dplyr)
library(shinydashboard)
library(gridExtra)
library(grid)
library(ggplot2)
library(tidyr)
library(data.table)
library(tidyverse)
library(plotly)
library(highcharter)
library(epicalc)
library(sqldf)
library(reshape)
library(officer)
library(rvg)

options(shiny.maxRequestSize=30*1024^2) #increase server to 30 MB

#choices of user input
C_year <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
C_specimen <- c('Plasma','Serum','Buffy_Coat','Fresh_Tissue')

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Import data", tabName = "import", icon = icon("file-import")),
    menuItem("Overall Visualization", tabName = "overall", icon = icon("globe")),
    menuItem("Query", icon = icon("search-plus"), tabName = "query"),
    menuItem("Table", icon = icon("table"), tabName = "table")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "import",
            h2("Data preparation")
    ),
    
    tabItem(tabName = "overall",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "query",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "table",
            h2("Widgets tab content")
    )
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "BioBank Dashboard"),
  sidebar,
  dashboardBody(
    tabItems(
    tabItem(tabName = "import",
  fluidRow(
    fileInput('my_file',label="Upload CSV. here", multiple = TRUE),
    tableOutput("preview"),
    downloadButton("download", "Download your report")
  )),
    tabItem(tabName = "overall",
  fluidRow(
    infoBoxOutput("project_box"),
    infoBoxOutput("patient_box"),
    infoBoxOutput("plasma_box"),
    infoBoxOutput("serum_box"),
    infoBoxOutput("tissue_box"),
    infoBoxOutput("buffy_box")),
  fluidRow(
    column(4,
           h5(strong("Pie chart from Specimen_Type")),
           plotlyOutput("Pie_specimen")),
    column(4,
           h5(strong("Piecharts of gender")),
           plotlyOutput("Pie_gender")),
    column(4,
           h5(strong("Pie chart from Specimen_Pathological.Status")),
           plotlyOutput("Pie_status")),
    column(6,
           h5(strong("line graph specimen type in each year")),
           plotlyOutput("Line_specimenType")),
    column(6,
           h5(strong("Bar graph specimen type in each project")),
           highchartOutput("Bar_specimenType"))
  )),
  tabItem(tabName = "query",
        fluidRow(
          selectInput(inputId="year", label = "Select Year:", choices = c(C_year,"Select All")),
          selectInput(inputId="project", label = "Select Project:", choices = C_specimen %>% sort()),
          infoBoxOutput("project_box2"),
          infoBoxOutput("patient_box2"),
          infoBoxOutput("plasma_box2"),
          infoBoxOutput("serum_box2"),
          infoBoxOutput("tissue_box2"),
          infoBoxOutput("buffy_box2")
          ),
        fluidRow(
          column(6,
                 h5(strong("Bar chart of specimen type")),
                 highchartOutput("Bar_specimen")),
          column(6,
                 h5(strong("Pie chart of gender")),
                 plotlyOutput("Pie_gender2")),
          column(6,
                 h5(strong("Query Table")),
                 dataTableOutput("table_page2"))
          )),
  tabItem(tabName = "table",
          h5(strong("Table Query")),      
          dataTableOutput("table_query"))
  
  )))  



server<-function(input,output, session) { 
  
  my_file <- reactive({
    if (is.null(input$my_file))
      return(NULL)
    my_file <- rbindlist(lapply(input$my_file$datapath, fread ),
                        use.names = TRUE, fill = TRUE)
    
    colnames(my_file)<- c("Participant_PPID","Participant_Registration.Date","Participant_Gender" ,"Visit_Clinical.Diagnosis..Deprecated.","Visit_Name" ,"Specimen_Type" ,"Specimen_Anatomic.Site","Specimen_Collection.Date","Specimen_Barcode" ,"Specimen_Class" ,"Specimen_Pathological.Status" ,"Specimen_Container.Name","Specimen_Container.Position","Project" )
    #rename values(ex.'Buffy Coat'='Buffy_Coat') because SQL is sensitive
    my_file$Specimen_Type <- as.character(my_file$Specimen_Type)
    my_file[my_file == "Buffy Coat"] <- "Buffy_Coat"
    my_file[my_file == "Fresh Tissue"] <- "Fresh_Tissue"
    #extract year from datetime
    my_file$Year <- format(as.Date(my_file$Specimen_Collection.Date,format="%d/%m/%Y"),"%Y")
    #select only useful columns
    my_file <- my_file %>% dplyr::select(Participant_PPID,Specimen_Type,Specimen_Pathological.Status,Project,Year,Participant_Gender)
    return(my_file)
  })
  
  df <- reactive({
    my_file <- my_file()
    df <- sqldf("SELECT Year,Project,COUNT(*) AS Total_samples,COUNT(DISTINCT Participant_PPID) AS Total_patients,
            SUM(CASE WHEN Specimen_Type = 'Plasma' THEN 1 ELSE 0 END) as Plasma,
            SUM(CASE WHEN Specimen_Type = 'Serum' THEN 1 ELSE 0 END) as Serum,
            SUM(CASE WHEN Specimen_Type = 'Buffy_Coat' THEN 1 ELSE 0 END) as Buffy_Coat,
            SUM(CASE WHEN Specimen_Type = 'Fresh_Tissue' THEN 1 ELSE 0 END) as Fresh_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Non-Malignant' THEN 1 ELSE 0 END) as Normal_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Malignant' THEN 1 ELSE 0 END) as Diseased_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Not Specified' THEN 1 ELSE 0 END) as Not_Specified
            FROM my_file
            GROUP BY Year,Project
            ");
    return(df)
  })
  
  
  output$preview <- renderTable({
    print(head(my_file()))
  })
  
  specimen_type <-reactive({
    my_file() %>% group_by(Specimen_Type) %>% tally()
  }) 
  
  sub_dataset <- reactive({
    # Filter data based on selected year
    if (input$year == "Select All") {
      my_file<- my_file()
    }
    if (input$year != "Select All") {
      my_file<- filter(my_file(), Year == input$year)
    }
    
    # Filter data based on selected project
    if (input$project == "Select All") {
      my_file <- my_file()
    }
    
    if (input$project != "Select All") {
      my_file <- filter(my_file(), Project == input$project)
    }
    
    return(my_file)
    
  })
  
  output$project_box <- renderInfoBox({
    infoBox(
      title =  paste("Total Project",":",sep = ""), value = length(unique(my_file()$Project))
      , icon = icon("list", lib = "font-awesome"),
      color = "blue", fill = TRUE)
  })
  
  output$project_box2 <- renderInfoBox({
    infoBox(
      title =  paste("Total Project",":",sep = ""), value = length(unique(sub_dataset()$Project))
      , icon = icon("list", lib = "font-awesome"),
      color = "blue", fill = TRUE)
  })
  
  
  output$patient_box <- renderInfoBox({
    infoBox(
      title =  paste("Total_Patients",":",sep = ""), value = length(unique(my_file()$Participant_PPID))
      , icon = icon("hospital-user", lib = "font-awesome"),
      color = "green", fill = TRUE)
  })
  
  output$patient_box2 <- renderInfoBox({
    infoBox(
      title =  paste("Total_Patients",":",sep = ""), value = length(unique(sub_dataset()$Participant_PPID))
      , icon = icon("hospital-user", lib = "font-awesome"),
      color = "green", fill = TRUE)
  })
  
  output$plasma_box <- renderInfoBox({
    infoBox(
      title =  paste(specimen_type()$Specimen_Type[3],":",sep = ""), value = paste(specimen_type()$n[3])
      , icon = icon("flask", lib = "font-awesome"),
      color = "red", fill = TRUE)
  })
  output$plasma_box2 <- renderInfoBox({
    infoBox(
      title =  paste(specimen_type()$Specimen_Type[3],":",sep = ""), value = paste(table(sub_dataset()$Specimen_Type)['Plasma'])
      , icon = icon("flask", lib = "font-awesome"),
      color = "red", fill = TRUE)
  })
  
  
  output$buffy_box <- renderInfoBox({
    infoBox(
      title =  paste(specimen_type()$Specimen_Type[1],":",sep = ""), value = paste(specimen_type()$n[1])
      , icon = icon("virus", lib = "font-awesome"),
      color = "blue", fill = TRUE)
  })
  
  output$buffy_box2 <- renderInfoBox({
    infoBox(
      title =  paste(specimen_type()$Specimen_Type[1],":",sep = ""), value = paste(table(sub_dataset()$Specimen_Type)['Buffy_Coat'])
      , icon = icon("virus", lib = "font-awesome"),
      color = "blue", fill = TRUE)
  })
  
  output$tissue_box <- renderInfoBox({
    infoBox(
      title =  paste(specimen_type()$Specimen_Type[2],":",sep = ""), value = paste(specimen_type()$n[2])
      , icon = icon("bacon", lib = "font-awesome"),
      color = "green", fill = TRUE)
  })
  output$tissue_box2 <- renderInfoBox({
    infoBox(
      title =  paste(specimen_type()$Specimen_Type[2],":",sep = ""), value = paste(table(sub_dataset()$Specimen_Type)['Fresh_Tissue'])
      , icon = icon("bacon", lib = "font-awesome"),
      color = "green", fill = TRUE)
  })
  
  output$serum_box <- renderInfoBox({
    infoBox(
      title =  paste(specimen_type()$Specimen_Type[4],":",sep = ""), value = paste(specimen_type()$n[4])
      , icon = icon("tint", lib = "font-awesome"),
      color = "red", fill = TRUE)
  })
  
  output$serum_box2 <- renderInfoBox({
    infoBox(
      title =  paste(specimen_type()$Specimen_Type[4],":",sep = ""), value = paste(table(sub_dataset()$Specimen_Type)['Serum'])
      , icon = icon("tint", lib = "font-awesome"),
      color = "red", fill = TRUE)
  })

  output$Pie_specimen <- renderPlotly({
    plot_ly(data= specimen_type(), labels = ~specimen_type,values = ~n, type = "pie")
  })
  
  
  # Pie chart from gender------------------------------------------------
  output$Pie_gender <- renderPlotly( {
    my_file() %>%
      group_by(Participant_PPID, Participant_Gender) %>% tally() %>%
      plot_ly(labels = ~Participant_Gender, type = "pie")
  })
  
  
  # Pie chart from Specimen_Pathological.Status--------------------------------
  output$Pie_status <- renderPlotly({
    my_file() %>%
      group_by(Specimen_Pathological.Status) %>% tally() %>%
      plot_ly(labels = ~Specimen_Pathological.Status,values = ~n, type = "pie")
  })
  
  #line graph specimen type in each year----------------------------------------
  output$Line_specimenType <- renderPlotly({
    ggplot(my_file(), aes(x=Year, color = Specimen_Type)) +
  geom_line(stat = "count") + theme_minimal()
  })
  
  #barchart each specimen type group by project----------------------------------------
  output$Bar_specimenType <- renderHighchart({
    my_file() %>% count(Project,Specimen_Type) %>%
    dplyr::rename(sum_value = n) %>%
    hchart('column', hcaes(x = Project, y = sum_value, group = Specimen_Type), dataLabels = list(
      enabled = TRUE)) %>% hc_colors(c("#FC9D96", "#FCD096", "#B5D4FC", "#A2E2A6"))
  })
  
  
  
  #Highchart bar chart of specimen type query by year and project----------------
  output$Bar_specimen <- renderHighchart( {
    
    # Error message for when user has filtered out all data
    validate (
      need(nrow(sub_dataset()) > 0, "No data found. Please make another selection.")
    )
    
    specimens <- sub_dataset() %>% group_by(Specimen_Type) %>% tally()
    
    # Bar chart
    specimens %>% hchart('column', hcaes(x = Specimen_Type, y = n, color = Specimen_Type))
    
  })
  
  
  #Highchart bar pie of gender query by year and project--------------------------
  output$Pie_gender2 <- renderPlotly( {
 
      # Error message for when user has filtered out all data
      validate (
        need(nrow(sub_dataset()) > 0, "No data found. Please make another selection.")
      )
      
      #plot Pie chart

        sub_dataset() %>%
          group_by(Participant_PPID, Participant_Gender) %>% tally() %>%
          plot_ly(labels = ~Participant_Gender, type = "pie")
    })
  
  
  # show summary table of query page2
  output$table_page2 <-renderDataTable({
    
    data_melt <-melt((df() %>% dplyr::select(Year,Project,Plasma,Serum,Buffy_Coat,Fresh_Tissue)), id = c("Year","Project"))
    
    # Filter data based on selected Style
    if (input$year == "Select All") {
      data_melt <- data_melt
    }
    
    if (input$year != "Select All") {
      data_melt <- filter(data_melt, Year == input$year)
    }
    
    # Filter data based on selected Country
    if (input$project == "Select All") {
      data_melt <- data_melt
    }
    
    if (input$project != "Select All") {
      data_melt <- filter(data_melt, Project == input$project)
    }
    
    # Hide table when user has filtered out all data
    validate (
      need(nrow(data_melt) > 0, "")
    )
    
    data_melt[,]
    
  })
  
  # Table summary -------------------------------------------------------------

  output$table_query <- renderDataTable({
    
    df()[,]
  })
  
  #update select output
  observe({ updateSelectInput(session,
                              inputId = "project",
                              choices = c(unique(my_file()
                                               [my_file()$Year == input$year,"Project"]),"Select All"))
  })
  
  #Download report
  output$download <- downloadHandler(
    filename = function(){"Biobank_report.pptx"}, 
                                     content = function(fname){
                                       
                                     }
                                     
  )
  
  
}

shinyApp(ui=ui,server=server)
  
