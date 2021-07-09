library(DT)
library(ggplot2)
library(dplyr)
library(sqldf)
library(tidyverse)
library(officer)
library(rvg)

dataset <- read.csv("C:/data/dataset.csv",header = TRUE)
gender_df <- dataset[!duplicated(dataset[, c("Participant_PPID", "Participant_Gender")]), ]
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

#pie chart ggplot
typepie <- ggplot(dataset, aes(x = factor(1), fill = factor(Specimen_Type))) +
  geom_bar(stat="count", width=1, color="white") +
  coord_polar("y", start=0) + labs(fill = "Type of Specimen") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_brewer(palette="Pastel1")

#pie chart ggplot
statuspie <- ggplot(dataset, aes(x = factor(1), fill = factor(Specimen_Pathological.Status))) +
  geom_bar(stat="count", width=1, color="white") +
  coord_polar("y", start=0) + labs(fill = "Specimen Status") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_brewer(palette="Pastel1")

#pie chart ggplot
genderpie <- ggplot(gender_df, aes(x = factor(1), fill = factor(Participant_Gender))) +
  geom_bar(stat="count", width=1, color="white") +
  coord_polar("y", start=0) + labs(fill = "Specimen Status") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_brewer(palette="Pastel1")

#line chart ggplot
linechart <- dataset %>%
  count(Year, Specimen_Type) %>% 
  dplyr::rename(sum_value = n) %>%
  ggplot(aes(x = Year, y = sum_value, group = Specimen_Type)) +
  geom_line(aes(color=Specimen_Type)) +
  geom_point(aes(color=Specimen_Type)) +
  ggtitle("Specimen Types in each year")

#bar chart ggplot
barchart <- dataset %>% count(Project,Specimen_Type) %>% 
  dplyr::rename(sum_value = n) %>%
  ggplot(aes(x = Project, y = sum_value, fill = Specimen_Type)) +
  geom_bar(width=0.7, position=position_dodge(width=0.75), stat="identity") +
  # geom_text(aes(label = sum_value), vjust = 0) +
  scale_y_sqrt() +
  scale_fill_brewer(palette="Pastel1")

--------------------------------------------------------------------------------------------------------------------------------------
#### Export to powerpoint

doc <-
  # Load template
  read_pptx() %>%
  # Add a slide
  add_slide(layout="Title Slide", master="Office Theme") %>%
  # Add title (ctrTitle)
  ph_with(value = "Biobank Report", location = ph_location_type(type = "ctrTitle")) %>% 
  # Add subtitle (subTitle)
  ph_with(value = "Data Visualization and Summary Table", location = ph_location_type(type = "subTitle")) %>%
  
  ### NEW PAGE ###
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Pie chart of specimen type", location = ph_location_type(type = "title")) %>%
  ph_with(value = typepie, location = ph_location_type(type = "body"), bg = "transparent") %>%
  
  ### NEW PAGE ###
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Pie chart of specimen status", location = ph_location_type(type = "title")) %>%
  ph_with(value = statuspie, location = ph_location_type(type = "body"), bg = "transparent") %>%
  
  ### NEW PAGE ###
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Pie chart of gender", location = ph_location_type(type = "title")) %>%
  ph_with(value = genderpie, location = ph_location_type(type = "body"), bg = "transparent") %>%
  
  ### NEW PAGE ###
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Specimen type in each year", location = ph_location_type(type = "title")) %>%
  ph_with(value = linechart, location = ph_location_type(type = "body"), bg = "transparent") %>%
  
  ### NEW PAGE ###
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Specimen type in each project", location = ph_location_type(type = "title")) %>%
  ph_with(value = barchart, location = ph_location_type(type = "body"), bg = "transparent") %>%
  
  ### NEW PAGE ###
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Summary Table ", location = ph_location_type(type = "title")) %>%
  ph_with(value = slice(df, 1:7), location = ph_location_type(type = "body", position_top = FALSE)) %>%
  
  ### NEW PAGE ###
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = slice(df, 8:14), location = ph_location_type(type = "title", position_top = TRUE)) %>%
  
  ### NEW PAGE ###
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = slice(df, 15:21), location = ph_location_type(type = "title", position_top = TRUE)) %>%
  
  ### NEW PAGE ###
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = slice(df, 22:28), location = ph_location_type(type = "title", position_top = TRUE)) %>%
  
  ### NEW PAGE ###
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = slice(df, 29:36), location = ph_location_type(type = "title", position_top = TRUE)) %>%
  
  print(doc, target = "Biobank_report.pptx")

