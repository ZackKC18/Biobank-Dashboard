# Biobank-Dashboard
This repository is for storing codes related to Biobank analysis and dashboard.

This repository consist of:

* Dashboards.Rmd - The file that the dashboard is generated from. flexdashboard framework and runtime with shiny.
* app.R - Shiny web app file
* Percent_Export.R -This is a file for calculating the percentage of samples that are removed each year.
* Powerpoint(edit).R - The process of creating powerpoint files
* random2.R - Analysis of the sampled data 45 percent.

### Required packages
<p> •	library(shiny) It is a web application framework for system development. </p>
<p> •	library(dplyr) for basic data managing </p>
<p> •	library(shinydashboard) Used to manage the structure of the user interface(ui). </p>
<p> •	library(data.table) used for generating tabular data. </p>
<p> •	library(plotly) for display in interactive graphics </p>
<p> •	library(epicalc) There are computational functions used in epidemiology. </p>
<p> •	library(sqldf) Used to change query data by SQL language. </p>
<p> •	library(reshape) Used to change the format of the table display. </p>
<p> •	library(officer) Used to generate reports in Microsoft word and powerpoint formats from R programming. </p>
<p> •	library(flextable) Used to create tables for reports. and easy to change the layout of the table </p>
<p> •	library (RColorBrewer) color palette used in making Visualization </p>


### Usage
=> If you want to open `Dashboards.Rmd` a dashboard created from flexdashboard + shiny, 
you need to open file Dashboards.R  by clicking `run document`
Or click on file and then click on `Knit Document`.

=> If you want to open `app.R` a dashboard created from shiny only,
You need to open the app.R file by clicking `run app`.

### shinyapp.IO 
I tried deploying app.R file, you can check it out at...https://tanatcha.shinyapps.io/dashboard/?_ga=2.201106473.606120721.1627287899-734185002.1624776163


### Additional tasks in progress
* dockertize app.R

Note: The update date of this file is 26/7/2021 

