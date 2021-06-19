setwd("C:/Users/acer/Desktop/Biobank_Dashboards")
library(shiny)
library(tidyverse)
library(spData) # For getting spatial data
library(sf) # For preserving spatial data
library(leaflet) # For making maps
library(DT) # For making fancy tables
library(reshape)
library(tidyr)
library(dplyr)
library(ggplot2)

#library(shinydashboard)

# Load review data
data <- read.csv("df.csv")


#Prepare data into difference form 
#df_table <- as.data.table(data, keep.rownames = TRUE)
df_table <- select(data,Year,Project,Plasma,Serum,Buffy_Coat,Fresh_Tissue)
df_table
data_melt <-melt(df_table,id = c("Year","Project"))
data_melt


# Define UI for application
ui <- fluidPage(
  
  # CSS
  tags$head(
    tags$style(HTML("
      body { background-color: #f2efe9; }
      .container-fluid { background-color: #fff; width: 1100px; padding: 60px; }
      .topimg { width: 120px; display: block; margin: 0px auto 40px auto; }
      .title { text-align: center; }
      .toprow { margin: 60px 0px; padding: 30px; background-color: #fae8bb; }
      .filters { margin: 0px auto; }
      .shiny-input-container { width:100% !important; }
      .table { padding: 30px; margin-top: 30px; }
      .leaflet-top { z-index:999 !important; }
      "))
    ),
   
  # Top image
  img(class = "topimg", src = "data:image/png;base64,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"),
  
   # Application title
   h1("Biobank Dashboard", class = "title"),
   
   fluidRow(class = "toprow",
     fluidRow (class = "filters",
               
       column(6,
         # Year Menu
         selectInput("year", "Year", unique(data_melt$Year))
       ),
       
       
       column(6,
         # Project menu
         selectInput("project", "Project", unique(data_melt$Project) %>% 
                          # append("All") %>% # Add "All" option
                           sort()) # Sort options alphabetically
         )
     )
   ),
   
   fluidRow (
     column(6, class = "bar",
       # Bar Chart
       plotOutput("specimenBar") )
     ),
  
   fluidRow (class = "table",
     # Table
     dataTableOutput("table")
   )
   
)

# Define server logic
server <- function(input, output, session) {

  # Create bar chart of brands
  output$specimenBar <- renderPlot( {
    
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
    ggplot(specimens, aes(reorder(variable, sumValue))) +
      geom_bar(aes(weight = sumValue), fill = "#FF6666") + 
      coord_flip() +
      ggtitle("Specimen Type") +
      xlab("Specimen") +
      ylab("sum value") +
      theme_bw(base_size = 16)
  })

 
  # Create data table
  output$table <- renderDataTable({
    
    # Filter data based on selected Style
    if (input$year != "All") {
      data_melt <- filter(data_melt, Year == input$year)
    }
    
    # Filter data based on selected Country
    if (input$project != "All") {
      data_melt <- filter(data_melt, Project == input$project)
    }
    
    # Hide table when user has filtered out all data
    validate (
      need(nrow(data_melt) > 0, "")
    )
    
    data_melt[,]
    
  })
  
  #update select output
  observe({ updateSelectInput(session,
                              inputId = "project",
                              choices = unique(data_melt
                                               [data_melt$Year == input$year,"Project"]))
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

