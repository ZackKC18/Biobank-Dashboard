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

dataset <- read.csv("dataset.csv",header = TRUE)

specimen_type <- dataset %>% group_by(Specimen_Type) %>% tally() #dataset %>% count(Specimen_Type)

#specimen_status <- dataset %>% count(Specimen_Pathological.Status)




gender_df <- dataset[!duplicated(dataset[, c("Participant_PPID", "Participant_Gender")]), ]
dataset$Year.f <- factor(dataset$Year)



ui<-navbarPage(span("Biobank Dashboard", style = "color: #0176a5 "),
               tabPanel("Overall visualization",
                        fluidRow(
                          column(2,
                                 valueBoxOutput("project_box")),
                          column(2,
                                 valueBoxOutput("patient_box")),
                          column(2,
                                 valueBoxOutput("plasma_box")),
                          column(2,
                                 valueBoxOutput("serum_box")),
                          column(2,
                                 valueBoxOutput("buffyCoat_box")),
                          column(2,
                                 valueBoxOutput("freshTissue_box")),
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
                                 h5(strong("Project in each years")),
                                 plotlyOutput("Bar_Project")),
                          column(6,
                                 h5(strong("line graph specimen type in each year")),
                                 plotlyOutput("Line_specimenType"))
                        )
                        
                        ),
               tabPanel("Query",
                        sidebarLayout(sidebarPanel(
                          selectInput(inputId="year", label = "Select Year:", choices = unique(dataset$Year)),
                          selectInput(inputId="project", label = "Select Project:", choices = unique(dataset$Project) %>% sort())),
                          

                      
                          #uiOutput(("my_inputProject")),
                          mainPanel(h5(strong("Bar chart of specimen type")),
                                              highchartOutput("Bar_specimen"),
                                              
                                              h5(strong("Pie chart of gender")),
                                              plotOutput("Pie_gender2")))
                        
                        
                        
                        ),
               
               tabPanel("Tendency"
                       ),
                        
                        
                        

               tabPanel("Summary Table",
            
                          mainPanel(h5(strong("Table Query")),
                                    dataTableOutput("table_query")))
               )


server<-function(input,output, session) { 
  

  #value box ----------------------------------------------------------

  
  output$project_box <- renderValueBox({
    Total_Project <- length(unique(dataset$Project))
    valueBox(value = Total_Project, 
             #subtitle = "Project" ,
             icon = icon("arrow-up"),
             color = "red")
  })
  output$patient_box <- renderValueBox({
    Total_Patients <-length(unique(dataset$Participant_PPID))
    valueBox(value = Total_Patients, 
             #subtitle = 'Total Patients',
             icon = icon("fa-tasks"),
             color = "red",
             href = NULL)
  })
  output$plasma_box <- renderValueBox({
    Total_plasma <- specimen_type[specimen_type$Specimen_Type == 'Plasma' , 'n']
    valueBox(value = Total_plasma, 
             #subtitle = "Total plasma" ,
             icon = icon("arrow-down"),
             color = "red")
  })
  output$serum_box <- renderValueBox({
    Total_Serum <- specimen_type[specimen_type$Specimen_Type == 'Serum', 'n']
    valueBox(value = Total_Serum,
             #subtitle = "Total Serum" ,
             icon = icon("arrow-down"),
             color = "red")
  })
  output$buffyCoat_box <- renderValueBox({
    Total_Buffy_Coat <- specimen_type[specimen_type$Specimen_Type == 'Buffy_Coat', 'n']
    valueBox(value = Total_Buffy_Coat,
             #subtitle = "Total Buffy Coat" ,
             icon = icon("arrow-down"),
             color = "red")
  })
  output$freshTissue_box <- renderValueBox({
    Total_Fresh_Tissue <- specimen_type[specimen_type$Specimen_Type == 'Fresh_Tissue' , 'n']
    valueBox(value = Total_Fresh_Tissue,
             #subtitle = "Total Fresh Tissue" ,
             icon = icon("arrow-down"),
             color = "red")
  })

  
  
  
  
  
  # Pie chart from Specimen_Type------------------------------------------
  output$Pie_specimen <- renderPlotly({
      p2 <- plot_ly(data= specimen_type, labels = ~Specimen_Type,values = ~n, type = "pie")
  })
  
  
  # Pie chart from gender------------------------------------------------
  output$Pie_gender <- renderPlotly( {
    dataset %>%
      group_by(Participant_PPID, Participant_Gender) %>% tally() %>%
      plot_ly(labels = ~Participant_Gender, type = "pie")
  })
  
  
  # Pie chart from Specimen_Pathological.Status--------------------------------
  output$Pie_status <- renderPlotly({
    dataset %>%
      group_by(Specimen_Pathological.Status) %>% tally() %>%
      plot_ly(labels = ~Specimen_Pathological.Status,values = ~n, type = "pie")
  })
  
  # Project in each years-----------------------------------------------------
  output$Bar_Project <- renderPlotly({
    p <- ggplot(dataset, aes(x = Year.f, y = Project, color = Specimen_Type)) +
      geom_jitter(width = 0.3, height = 0.4, alpha = 0.5) + ylab("Years")
    ggplotly(p)
  })
  
  #line graph specimen type in each year----------------------------------------
  output$Line_specimenType <- renderPlotly({
    dataset %>%
      group_by(Year.f, Specimen_Type) %>% tally() %>% 
      plot_ly(x = ~Year.f, y = ~n, color = ~Specimen_Type, type = "scatter", mode = "lines+markers")
  })
  
  
  
  
  #Highchart bar chart of specimen type query by year and project----------------
  output$Bar_specimen <- renderHighchart( {
    
    # Filter data based on selected Style
    if (input$year != "All") {
      data_melt <- filter(data_melt, Year == input$year)
    }
    
    # Filter data based on selected Country
    if (input$project != "All") {
      data_melt <- filter(data_melt, Project == input$project)
    }
    
    # Error message for when user has filtered out all data
    validate (
      need(nrow(data_melt) > 0, "No data found. Please make another selection.")
    )
    
    # Get top 20 brands
    specimens <- group_by(data_melt, variable) %>% 
      summarise(sumValue = sum(value)) %>% 
      arrange(desc(sumValue))
    
    # Bar chart
    specimens %>% hchart('column', hcaes(x = variable, y = sumValue, color = variable))
    
  })
  
  #Highchart bar pie of gender query by year and project--------------------------
  output$Pie_gender2 <- renderPlot( {

    # Filter data based on selected Style
    if (input$year != "All") {
      gender_df <- filter(gender_df, Year == input$year)
    }
    
    # Filter data based on selected Country
    if (input$project != "All") {
      gender_df <- filter(gender_df, Project == input$project)
    }
    
    # Error message for when user has filtered out all data
    validate (
      need(nrow(gender_df) > 0, "No data found. Please make another selection.")
    )
    
    # Get sum of each gender
    gend <- as.data.frame(table(gender_df$Participant_Gender))
    gend <- gend %>% mutate(per=Freq/sum(Freq)) %>% arrange(desc(Var1))
    gend$label <- scales::percent(gend$per)
    
    #plot Pie chart
    ggplot(data =gend)+geom_bar(aes(x='',y=per,fill=Var1), stat='identity', width = 1)+coord_polar("y",start=0)+ theme_void()+geom_text(aes(x=1,y=cumsum(per)- per/2,label=label))+ggtitle("Gender Pie chart")
    #ggplot(gend, aes(x="", y=Freq, fill=Var1)) +
    # geom_bar(stat="identity", width=1) +
    #coord_polar("y", start=0)

  })
  
  
  
  # Table summary -------------------------------------------------------------
  library(sqldf)
  df <- sqldf("SELECT Year,Project,COUNT(*) AS Total_samples,COUNT(DISTINCT Participant_PPID) AS Total_patients,
            SUM(CASE WHEN Specimen_Type = 'Plasma' THEN 1 ELSE 0 END) as Plasma,
            SUM(CASE WHEN Specimen_Type = 'Serum' THEN 1 ELSE 0 END) as Serum,
            SUM(CASE WHEN Specimen_Type = 'Buffy_Coat' THEN 1 ELSE 0 END) as Buffy_Coat,
            SUM(CASE WHEN Specimen_Type = 'Fresh_Tissue' THEN 1 ELSE 0 END) as Fresh_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Non-Malignant' THEN 1 ELSE 0 END) as Normal_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Malignant' THEN 1 ELSE 0 END) as Diseased_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Not Specified' THEN 1 ELSE 0 END) as Not_Specified
            FROM dataset
            GROUP BY Year,Project
            ");
  output$table_query <- renderDataTable({

    df[,]
  })
  
  #update select output
  observe({ updateSelectInput(session,
                              inputId = "project",
                              choices = unique(dataset
                                               [dataset$Year == input$year,"Project"]))
    
  })
  
  
}
shinyApp(ui=ui,server=server)