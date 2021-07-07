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

valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 56px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}

dataset <- read.csv("dataset.csv",header = TRUE)

specimen_type <- dataset %>% group_by(Specimen_Type) %>% tally() #dataset %>% count(Specimen_Type)

#specimen_status <- dataset %>% count(Specimen_Pathological.Status)




gender_df <- dataset[!duplicated(dataset[, c("Participant_PPID", "Participant_Gender")]), ]
dataset$Year.f <- factor(dataset$Year)


ui<-navbarPage(span("Biobank Dashboard", style = "color: #0176a5 "),
               tabPanel("Overall visualization",
                        fluidRow(
                          valueBox(value ="projectBox" , subtitle = "Total Project", icon= "list-alt" ,color = "red"),
                          valueBox(value ="patientBox" , subtitle = "Total Patients", icon = "user-friends",color = "olive"),
                          valueBox(value ="plasmaBox" , subtitle = "Total Plasma", icon = "tint",color = "aqua"),
                          valueBox(value ="serumBox" , subtitle = "Total Serum", icon = "vial",color = "teal"),
                          valueBox(value ="buffyCoatBox" , subtitle = "Total Buffy coat", icon = "vials",color = "fuchsia"),
                          valueBox(value ="freshTissueBox" , subtitle = "Total Fresh tissue", icon ="disease",color = "maroon"),
                          ),
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
                                 h5(strong("Bar plot specimen type in each project")),
                                 highchartOutput("Bar_Project")),
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
               
               tabPanel("Summary Table",
                        
                        mainPanel(h5(strong("Table Query")),
                                  dataTableOutput("table_query")))
)


server<-function(input,output, session) { 
  
  
  #value box ----------------------------------------------------------
  
  output$projectBox<- renderText({ 
    Total_Project <- length(unique(dataset$Project))
    print(Total_Project)
  })
  
  output$patientBox<- renderText({ 
    Total_Patients <-length(unique(dataset$Participant_PPID))
    print(Total_Patients)
  })
  
  output$plasmaBox<- renderText({ 
    Total_plasma <- specimen_type$n[specimen_type$Specimen_Type=='Plasma']
    print(Total_plasma)
  })
  
  output$serumBox<- renderText({ 
    Total_Serum <- specimen_type$n[specimen_type$Specimen_Type=='Serum']
    print(Total_Serum)
  })
  
  output$buffyCoatBox<- renderText({ 
    Total_Buffy_Coat <- specimen_type$n[specimen_type$Specimen_Type=='Buffy_Coat']
    print(Total_Buffy_Coat)
  })
  
  output$freshTissueBox<- renderText({ 
    Total_Buffy_Coat <- specimen_type$n[specimen_type$Specimen_Type=='Fresh_Tissue']
    print(Total_Fresh_Tissue)
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
  
  
  #line graph specimen type in each year----------------------------------------
  output$Line_specimenType <- renderPlotly({
    dataset %>%
      group_by(Year.f, Specimen_Type) %>% tally() %>% 
      plot_ly(x = ~Year.f, y = ~n, color = ~Specimen_Type, type = "scatter", mode = "lines+markers")
  })
  
  #Bar chart specimen type in each project-------------------------------------
  output$Bar_Project <- renderHighchart({
    dataset %>% count(Project,Specimen_Type) %>%
      dplyr::rename(sum_value = n) %>%
      hchart('column', hcaes(x = Project, y = sum_value, group = Specimen_Type), dataLabels = list(
        enabled = TRUE)) %>% hc_colors(c("#FC9D96", "#FCD096", "#B5D4FC", "#A2E2A6"))
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