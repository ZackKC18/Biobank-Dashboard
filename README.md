# Biobank-Dashboard
This repository is for storing codes related to Biobank analysis and dashboard.

This repository consist of:

* clean.R - It is a file for merging and cleaning the data and exporting the dataset.csv.
* dataset.csv - The cleaned file is complete and ready to use.
* Dashboards.Rmd - The file that the dashboard is generated from. flexdashboard framework and runtime with shiny.
* app.R - Shiny web app file

### Required packages
* library(shiny)
* library(dplyr)
* library(ggplot2)
* library(tidyr)
* library(data.table)
* library(tidyverse)
* library(plotly)
* library(highcharter)

### Usage
start to run clean.R 
you will get dataset.csv

=> If you want to open `Dashboards.Rmd` a dashboard created from flexdashboard + shiny, 
you need to open file Dashboards.R  by clicking `run document`
Or click on file and then click on `Knit Document`.

=> If you want to open `app.R` a dashboard created from shiny only,
You need to open the app.R file by clicking `run app`.

### How to download file in Dropbox to R program?
To force the R program to download the contents of a Dropbox link, you can use dl=1 as a query parameter in your URL. For example:
- The original shared link : `https://www.dropbox.com/s/a1b2c3d4ef5gh6/example.docx?dl=0`
- Modify links that can be downloaded :`https://www.dropbox.com/s/a1b2c3d4ef5gh6/example.docx?dl=1`

So now you can download files from dropbox to R program by using

` read.csv("Modify links that can be downloaded")`

### shinyapp.IO 

I tried deploying Dashboards.R file, you can check it out at...https://thanrada.shinyapps.io/Dashboards/?_ga=2.39631520.229271646.1625041166-1597395094.1623831708
Note: The website takes a long time to load.

### Additional tasks in progress
* Edit label of visualization
* Decorate the colors into the same palette.
* Fix the layout to be appropriate.
* Add search pages by year only.
* Add search page by project only
* Remove Jitterplot
* Run time analysis
* Create value box in app.R

### Example of user interface
[![sPbcx9.png](https://sv1.picz.in.th/images/2021/07/01/sPbcx9.png)](https://www.picz.in.th/image/sPbcx9)

Note: The update date of this file is 1/7/2021
