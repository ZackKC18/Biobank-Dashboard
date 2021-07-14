library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(sqldf)
library(tidyverse)
library(officer)
library(rvg)
library(htmlwidgets)
library(webshot)
library(magrittr)

dataset <- read.csv("C:/data/dataset.csv",header = TRUE)
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

# plotly graph -------------------------------------------------------------------------------------------------

# Specimen type
type <- dataset %>%
  count(Specimen_Type) %>% 
  plot_ly(labels = ~Specimen_Type, values = ~n, 
          marker = list(colors = c("#FECCC9", "#FEE7C9", "#C9E0FE", "#C9FECD"), 
          line = list(color = '#FFFFFF', width = 1)), type = "pie")

# Specimen status
status <- dataset %>%
  count(Specimen_Pathological.Status) %>%
  plot_ly(labels = ~Specimen_Pathological.Status, values = ~n, 
          marker = list(colors = c("#FEE7C9", "#C9FECD", "#E2E0FE"), 
          line = list(color = '#FFFFFF', width = 1)), type = "pie")

# Gender
gender <- dataset %>%
  count(Participant_PPID, Participant_Gender) %>%
  plot_ly(labels = ~Participant_Gender, 
          marker = list(colors = c("#C9E0FE", "#FECCC9"), 
          line = list(color = '#FFFFFF', width = 1)), type = "pie")

# Line chart
line <- dataset %>%
  count(Year, Specimen_Type) %>% 
  dplyr::rename(sum_value = n) %>%
  plot_ly(x = ~Year, y = ~sum_value, color = ~Specimen_Type, type = "scatter", mode = "lines+markers",
          text = ~sum_value, textposition= "outside")

# Bar chart
bar <- dataset %>% count(Project, Specimen_Type) %>%
  dplyr::rename(sum_value = n) %>% 
  plot_ly(x = ~Project, y = ~sum_value, type = "bar", color = ~Specimen_Type)

# plotly to image ----------------------------------------------------------------------------------------------

type_file <- tempfile(pattern = "", fileext = ".png")
saveWidget(type, "type.html")
webshot("type.html", type_file)
type_image <- external_img(type_file, width = 7, height = 5)

status_file <- tempfile(pattern = "", fileext = ".png")
saveWidget(status, "status.html")
webshot("status.html", status_file)
status_image <- external_img(status_file, width = 7, height = 5)

gender_file <- tempfile(pattern = "", fileext = ".png")
saveWidget(gender, "gender.html")
webshot("gender.html", gender_file)
gender_image <- external_img(gender_file, width = 7, height = 5)

line_file <- tempfile(pattern = "", fileext = ".png")
saveWidget(line, "line.html")
webshot("line.html", line_file)
line_image <- external_img(line_file, width = 7, height = 5)

bar_file <- tempfile(pattern = "", fileext = ".png")
saveWidget(bar, "bar.html")
webshot("bar.html", bar_file)
bar_image <- external_img(bar_file, width = 7, height = 5)

# export to pptx -----------------------------------------------------------------------------------------------

# doc <- read_pptx() %>%
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with(value = "Title of the PPT", location = ph_location_type(type = "title")) %>%
#   ph_with(image1, location = ph_location_type(type = "body"), use_loc_size = FALSE)
# 
# print(doc, target = "testtest.pptx")

doc <-
  read_pptx() %>%
  add_slide(layout="Title Slide", master="Office Theme") %>%
  ph_with(value = "Biobank Report", location = ph_location_type(type = "ctrTitle")) %>% 
  ph_with(value = "Data Visualization and Summary Table", location = ph_location_type(type = "subTitle")) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Pie chart of specimen type", location = ph_location_type(type = "title")) %>%
  ph_with(type_image, location = ph_location_type(type = "body"), use_loc_size = FALSE) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Pie chart of specimen status", location = ph_location_type(type = "title")) %>%
  ph_with(status_image, location = ph_location_type(type = "body"), use_loc_size = FALSE) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Pie chart of gender", location = ph_location_type(type = "title")) %>%
  ph_with(gender_image, location = ph_location_type(type = "body"), use_loc_size = FALSE) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Specimen type in each year", location = ph_location_type(type = "title")) %>%
  ph_with(line_image, location = ph_location_type(type = "body"), use_loc_size = FALSE) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Specimen type in each project", location = ph_location_type(type = "title")) %>%
  ph_with(bar_image, location = ph_location_type(type = "body"), use_loc_size = FALSE) %>%

  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Summary Table ", location = ph_location_type(type = "title")) %>%
  ph_with(value = slice(df, 1:7), location = ph_location_type(type = "body", position_top = FALSE)) %>%

  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = slice(df, 8:14), location = ph_location_type(type = "title", position_top = TRUE)) %>%

  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = slice(df, 15:21), location = ph_location_type(type = "title", position_top = TRUE)) %>%

  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = slice(df, 22:28), location = ph_location_type(type = "title", position_top = TRUE)) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = slice(df, 29:36), location = ph_location_type(type = "title", position_top = TRUE)) %>%
  
  print(doc, target = "Biobank_report.pptx")