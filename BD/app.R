library(shiny)
library(dplyr)
library(shinydashboard)
library(data.table)
library(plotly)
#library(epicalc)
library(sqldf)
library(reshape)
#library(officer)
#library(rvg)
#library(flextable)
library(RColorBrewer)

options(shiny.maxRequestSize=30*1024^2) #increase server to 30 MB


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
    tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  '))),
    tabItems(
      tabItem(tabName = "import",
              fluidRow(
                column(12,align="center", uiOutput("descrip")),
                column(12,align="center", fileInput('my_file',label="Upload CSV. here", multiple = TRUE)),
                column(12,align="center", span("Please upload file before navigate to the other pages",style="color:red")),
                verbatimTextOutput("file_checker"),
                htmlOutput("before_text"),
                tableOutput("before_cleaned"),
                htmlOutput("after_text"),
                tableOutput("after_cleaned"),
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
                       plotlyOutput ("Bar_specimenType"))
              )),
      tabItem(tabName = "query",
              fluidRow(
                uiOutput("C_years"),
                uiOutput("C_projects"),
                verbatimTextOutput("test"),
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
                       plotlyOutput("Bar_specimen")),
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
  
  raw_combine <- reactive({
    if (is.null(input$my_file))
      return(NULL)
    data.table::rbindlist(lapply(input$my_file$datapath, fread ),
                          use.names = TRUE, fill = FALSE)
  })
  
  my_file <- reactive({
    if (is.null(input$my_file))
      return(NULL)
    my_file <- raw_combine()
    colnames(my_file) <- c("Participant_PPID","Participant_Registration.Date","Participant_Gender" ,"Visit_Clinical.Diagnosis..Deprecated.","Visit_Name" ,"Specimen_Type" ,"Specimen_Anatomic.Site","Specimen_Collection.Date","Specimen_Barcode" ,"Specimen_Class" ,"Specimen_Pathological.Status" ,"Specimen_Container.Name","Specimen_Container.Position","Project" )
    #rename values(ex.'Buffy Coat'='Buffy_Coat') because SQL is sensitive
    my_file[my_file == "Buffy Coat"] <- "Buffy_Coat"
    my_file[my_file == "Fresh Tissue"] <- "Fresh_Tissue"
    my_file[my_file == "Not Specified"] <- "Blood_Samples"
    #extract year from datetime
    my_file$Year <- format(as.Date(my_file$Specimen_Collection.Date,format="%d/%m/%Y"),"%Y")
    my_file$Year <- factor(my_file$Year)
    my_file$Project <- factor(my_file$Project)
    #select only useful columns
    my_file <- my_file %>% dplyr::select(Participant_PPID,Specimen_Type,Specimen_Pathological.Status,Project,Year,Participant_Gender)
    return(my_file)
  })
  
  output$C_years <- renderUI({
    C_year <- levels(my_file()$Year)
    selectInput(inputId="year", label = "Select Year:", choices = c(C_year,"All Years") %>% sort())
  })
  
  output$C_projects  <- renderUI({
    
    if(input$year!="All Years"){
      datproj <- dplyr::filter(my_file(), Year == input$year)
      datproj$Project <- factor(datproj$Project)
      selectInput(inputId = "proj", label = "Select Project:",
                  choices = c(as.list(levels(datproj$Project)), "All Projects"),multiple = FALSE)
      
    } else {
      C_proj <- levels(my_file()$Project)
      selectInput(inputId = "proj", label = "Select Project:",
                  choices =c(C_proj,"All Projects"), multiple = FALSE)
    }
  })
  
  df <- reactive({
    req(my_file())
    my_file <- my_file()
    df <- sqldf::sqldf("SELECT Year,Project,COUNT(*) AS Total_samples,COUNT(DISTINCT Participant_PPID) AS Total_patients,
            SUM(CASE WHEN Specimen_Type = 'Plasma' THEN 1 ELSE 0 END) as Plasma,
            SUM(CASE WHEN Specimen_Type = 'Serum' THEN 1 ELSE 0 END) as Serum,
            SUM(CASE WHEN Specimen_Type = 'Buffy_Coat' THEN 1 ELSE 0 END) as Buffy_Coat,
            SUM(CASE WHEN Specimen_Type = 'Fresh_Tissue' THEN 1 ELSE 0 END) as Fresh_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Non-Malignant' THEN 1 ELSE 0 END) as Normal_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Malignant' THEN 1 ELSE 0 END) as Diseased_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Blood_Samples' THEN 1 ELSE 0 END) as Blood_Samples
            FROM my_file
            GROUP BY Year,Project
            ");
    return(df)
  })
  
  
  
  specimen_type <-reactive({
    req(my_file())
    my_file() %>% dplyr::group_by(Specimen_Type) %>% tally()
  }) 
  
  sub_dataset <- reactive({
    req(input$year,input$proj)
    # Filter data based on selected year
    if (input$year == "All Years") {
      if(input$proj == "All Projects"){
        my_file<- my_file()
      } else {
        my_file <- dplyr::filter(my_file(), Project == input$proj)
      }
      
    } else {
      if(input$proj == "All Projects"){
        my_file<- dplyr::filter(my_file(), Year == input$year)
      } else {
        my_file <- dplyr::filter(my_file(), Project == input$proj & Year == input$year) 
      }
    }
    
  })
  
  Valid_function <- function() {return(validate (need(nrow(my_file()) > 0, "No data found. Please upload your file in first page.")))}
  
  output$before_text <- renderText({
    if (is.null(input$my_file))
      return(NULL)
    HTML(paste0("<h3>" , "The data table you uploaded :","</h3>"))
  })
  
  output$before_cleaned <-  renderTable({
    print(head(raw_combine()))
  })
  
  output$after_text <- renderText({
    if (is.null(input$my_file))
      return(NULL)
    HTML(paste0("<h3>" , "Cleaned data table :","</h3>"))
  })
  
  output$after_cleaned <- renderTable({
    print(head(my_file()))
  })
  
  output$descrip <- renderUI({
    str1 <- (paste("<h2>","The functionality of this page will help prepare the uploaded data in the proper format", "</h3>"))
    str2 <- (paste("You have now uploaded the data : ",dim(raw_combine())[2],"  Columns  ", dim(raw_combine())[1],"  Rows" ))
    HTML(paste(str1, str2, sep = '<br/>'))
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
  
  
  # Pie chart from specimen type
  output$Pie_specimen <- renderPlotly({
    Valid_function()
    my_file() %>%
      count(Specimen_Type) %>%
      plot_ly(labels = ~Specimen_Type, values= ~n , type='pie',
              marker = list(colors = brewer.pal(n = 4, name = "Pastel1"), line = list(color = '#FFFFFF', width = 1)))
  })
  
  # ggplot specimen type
  type_pie <- reactive({
    ggplot(my_file(), aes(x = "", fill = factor(Specimen_Type))) +
      geom_bar(stat="count", width=1, color="white") +
      geom_text(aes(label = scales::percent(..count.. / sum(..count..))), 
                stat = "count", position = position_stack(vjust = .5)) +
      coord_polar("y", start=0) + labs(fill = "Type of Specimen") +
      theme_void() +
      scale_fill_brewer(palette="Pastel1")
  })
  
  output$Pie_specimen1 <- renderPlot({
    type_pie()
  })
  
  
  # Pie chart from gender
  output$Pie_gender <- renderPlotly({
    Valid_function()
    my_file() %>%
      dplyr::group_by(Participant_PPID, Participant_Gender) %>% tally() %>%
      plot_ly(labels = ~Participant_Gender, type = "pie", 
              marker = list(colors = brewer.pal(n = 3, name = "Pastel1"), line = list(color = '#FFFFFF', width = 1)))
  })
  
  # ggplot gender
  gender_pie <- reactive({
    ggplot(my_file()[!duplicated(my_file()[, c("Participant_PPID", "Participant_Gender")]), ], 
           aes(x = factor(1), fill = factor(Participant_Gender))) +
      geom_bar(stat="count", width=1, color="white") +
      geom_text(aes(label = scales::percent(..count.. / sum(..count..))), 
                stat = "count", position = position_stack(vjust = .5)) +
      coord_polar("y", start=0) + labs(fill = "Specimen Status") +
      theme_void() +
      scale_fill_brewer(palette="Pastel1")
  })
  
  output$Pie_gender1 <- renderPlot({
    gender_pie()
  })
  
  # Pie chart from Specimen_Pathological.Status
  output$Pie_status <- renderPlotly({
    Valid_function()
    my_file() %>%
      dplyr::group_by(Specimen_Pathological.Status) %>% tally() %>%
      plot_ly(labels = ~Specimen_Pathological.Status,values = ~n, type = "pie",
              marker = list(colors = brewer.pal(n = 3, name = "Pastel1"), line = list(color = '#FFFFFF', width = 1)))
  })
  
  status_pie <- reactive({
    ggplot(my_file(), aes(x = factor(1), fill = factor(Specimen_Pathological.Status))) +
      geom_bar(stat="count", width=1, color="white") +
      geom_text(aes(label = scales::percent(..count.. / sum(..count..))), 
                stat = "count", position = position_stack(vjust = .5)) +
      coord_polar("y", start=0) + labs(fill = "Specimen Status") +
      theme_void() +
      scale_fill_brewer(palette="Pastel1")
    
  })
  
  output$Pie_status1 <- renderPlot({
    status_pie()
  })
  
  #line graph specimen type in each year
  line_chart <- reactive({
    Valid_function()
    my_file() %>%
      count(Year, Specimen_Type) %>% 
      dplyr::rename(sum_value = n) %>%
      ggplot(aes(x = Year, y = sum_value, group = Specimen_Type)) +
      geom_line(aes(color=Specimen_Type)) +
      geom_point(aes(color=Specimen_Type)) +
      geom_text(aes(label = sum_value), position = position_dodge(width=0.8), hjust=-.25, size=2.5)
  })
  
  output$Line_specimenType <- renderPlotly({
    ggplotly(line_chart())
  })
  
  output$Line_specimenType1 <- renderPlot({
    line_chart()
  })
  
  
  #barchart each specimen type group by project
  bar_chart <- reactive({
    Valid_function()
    my_file() %>% count(Project,Specimen_Type) %>% 
      dplyr::rename(sum_value = n) %>%
      ggplot(aes(x = Project, y = sum_value, fill = Specimen_Type)) +
      geom_bar(width=0.7, position=position_dodge(width=0.75), stat="identity") +
      geom_text(aes(label = sum_value), position = position_dodge(width=0.9), size=2.5) +
      scale_y_sqrt() +
      scale_fill_brewer(palette="Pastel1")
  })
  
  output$Bar_specimenType <- renderPlotly({
    ggplotly(bar_chart())
  })
  
  output$Bar_specimenType1 <- renderPlot({
    bar_chart()
  })
  
  
     
  #Highchart bar chart of specimen type query by year and project
  output$Bar_specimen <- renderPlotly({
    Valid_function()
    #Plot
    specimens <- sub_dataset() %>% dplyr::group_by(Specimen_Type) %>% tally()
    plot_ly(specimens, x= ~Specimen_Type ,y=~n , type = 'bar',
            color = ~Specimen_Type, colors = brewer.pal(n = 4, name = "Pastel1"))
    
  })
  
  
  #Highchart bar pie of gender query by year and project
  output$Pie_gender2 <- renderPlotly( {
    Valid_function()
    #Plot
    sub_dataset() %>%
      dplyr::group_by(Participant_PPID, Participant_Gender) %>% tally() %>%
      plot_ly(labels = ~Participant_Gender, type = "pie",
              marker = list(colors = brewer.pal(n = 3, name = "Pastel1"), line = list(color = '#FFFFFF', width = 1)))
  })
  
  
  # show summary table of query page2
  output$table_page2 <-renderDataTable({
    Valid_function()
    # show message in case of there are no any input from user
    validate (
      need(nrow(my_file()) > 0, "No data found. Please make another selection."))
    
    data_melt <-  reshape::melt((df() %>% dplyr::select(Year,Project,Plasma,Serum,Buffy_Coat,Fresh_Tissue)), id = c("Year","Project"))
   
    # Filter data based on selected year and project
    if (input$year == "All Years") {
      if(input$proj == "All Projects"){
        data_melt<- data_melt
      } else {
        data_melt <- filter(data_melt, Project == input$proj)
      }
      
    } else {
      if(input$proj == "All Projects"){
        data_melt<- filter(data_melt, Year == input$year)
      } else {
        data_melt <- filter(data_melt, Project == input$proj & Year == input$year) 
      }
    }
    
    data_melt[,]
    
  })
  
  # Table summary
  
  output$table_query <- renderDataTable({
    Valid_function()
    df()[,]
  })
  
  #update select output
  observe({ updateSelectInput(session,
                              inputId = "project",
                              choices = c(unique(my_file()
                                                 [my_file()$Year == input$year,"Project"]),"All Projects"))
  })
  
  
  
  #Download report
  output$download <- downloadHandler(
    filename = function(){"Biobank_report.pptx"}, 
    content = function(fname){
      
      flex_df <- flextable(df()[,]) %>%
        bg(i = 1, bg = "#CFEAF8", part = "header") %>%
        font(i = 1, fontname = "Chulabhorn Likit Text", part = "header") %>%
        font(fontname = "Chulabhorn Likit Text", part = "body") %>%
        fontsize(i = 1, size = 12, part = "header") %>%
        fontsize(size = 10, part = "body") %>%
        align(align = "center", part = "all")
      
      text_font <- fp_text(font.family = "Chulabhorn Likit Text", font.size = 32)
      
      doc <-
        read_pptx() %>%
        add_slide(layout="Title Slide", master="Office Theme") %>%
        ph_with(value = fpar(ftext("Biobank Report", fp_text(bold = TRUE, font.family = "Chulabhorn Likit Text", font.size = 48))), 
                location = ph_location_type(type = "ctrTitle")) %>%
        ph_with(value = fpar(ftext("Data Visualization and Summary Table", text_font)), 
                location = ph_location_type(type = "subTitle")) %>%
        
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = fpar(ftext("Pie chart of specimen type", text_font)), location = ph_location_type(type = "title")) %>%
        ph_with(value = type_pie(), location = ph_location_type(type = "body")) %>%
        
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = fpar(ftext("Pie chart of specimen status", text_font)), location = ph_location_type(type = "title")) %>%
        ph_with(value = status_pie(), location = ph_location_type(type = "body")) %>%
        
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = fpar(ftext("Pie chart of gender", text_font)), location = ph_location_type(type = "title")) %>%
        ph_with(value = gender_pie(), location = ph_location_type(type = "body")) %>%
        
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = fpar(ftext("Specimen type in each year", text_font)), location = ph_location_type(type = "title")) %>%
        ph_with(value = line_chart(), location = ph_location_type(type = "body")) %>%
        
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = fpar(ftext("Specimen type in each project", text_font)), location = ph_location_type(type = "title")) %>%
        ph_with(value = bar_chart(), location = ph_location_type(type = "body")) %>%
        
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = fpar(ftext("Summary table", text_font)), location = ph_location_type(type = "title")) %>%
        ph_with(value = flex_df, location = ph_location_type(type = "body", position_top = FALSE)) %>%
        
        print(doc, target = fname)
      
    }
    
  )
  
  
}

shinyApp(ui=ui,server=server)
