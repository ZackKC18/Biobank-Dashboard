#install packages
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("sqldf")
library(sqldf)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("naniar")
library(naniar)
#install.packages("tidyr")
library(tidyr)
#install.packages("data.table")
library(data.table)
#install.packages("reshape")
library(reshape)
library(reshape2)
#install.packages("readr")
library(readr)

#import data
BS1001 <- read.csv("https://www.dropbox.com/s/mt0y2seqcmdfmw8/BS1001data.csv?dl=1")
BS1101 <- read.csv("https://www.dropbox.com/s/31uzgrtlgk3uc2q/BS1101data.csv?dl=1")
BS1103 <- read.csv("https://www.dropbox.com/s/ho7ekafgwik8uzk/BS1103data.csv?dl=1")
BS1202 <- read.csv("https://www.dropbox.com/s/jy61kgqvyeki8nz/BS1202data.csv?dl=1")
BS1203 <- read.csv("https://www.dropbox.com/s/mkg0e2v62v4nnmj/BS1203data.csv?dl=1")
BS1402 <- read.csv("https://www.dropbox.com/s/vy17rdlkg5sxlm0/BS1402data.csv?dl=1")
BS1701 <- read.csv("https://www.dropbox.com/s/a9wwvhha4e9cvuq/BS1701data.csv?dl=1")
BS1904 <- read.csv("https://www.dropbox.com/s/t3b4ya6cqo19o95/BS1904data.csv?dl=1")


#concatenate data
data <- rbind(BS1001,BS1101,BS1103,BS1202,BS1203,BS1402,BS1701,BS1904)
#replace blank value with NA value
data[data==""] <- NA
colSums(is.na(data))

#Change some column name
names(data)[14] <- "Project"
#rename.values(data,'Buffy Coat'='Buffy_Coat') because SQL is sensitive
data$Specimen_Type <- as.character(data$Specimen_Type)
data[data == "Buffy Coat"] <- "Buffy_Coat"
data[data == "Fresh Tissue"] <- "Fresh_Tissue"

#extract year from datetime
data$Year <- format(as.Date(data$Specimen_Collection.Date,format="%d/%m/%Y"),"%Y" )

#select only useful columns
data <- select(data,Participant_PPID,Specimen_Type,Specimen_Pathological.Status,Project,Year,Participant_Gender)


#This part for make report table
#summary samples by year ans project
df <- sqldf("SELECT Year,Project,COUNT(*) AS Total_samples,COUNT(DISTINCT Participant_PPID) AS Total_patients,
            SUM(CASE WHEN Specimen_Type = 'Plasma' THEN 1 ELSE 0 END) as Plasma,
            SUM(CASE WHEN Specimen_Type = 'Serum' THEN 1 ELSE 0 END) as Serum,
            SUM(CASE WHEN Specimen_Type = 'Buffy_Coat' THEN 1 ELSE 0 END) as Buffy_Coat,
            SUM(CASE WHEN Specimen_Type = 'Fresh_Tissue' THEN 1 ELSE 0 END) as Fresh_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Non-Malignant' THEN 1 ELSE 0 END) as Normal_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Malignant' THEN 1 ELSE 0 END) as Diseased_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Not Specified' THEN 1 ELSE 0 END) as Not_Specified
            FROM data
            GROUP BY Year,Project
            ");


#Summary Sample by project
df5 <- sqldf("SELECT Project,COUNT(DISTINCT Participant_PPID) AS Total_patients,
            SUM(CASE WHEN Specimen_Type = 'Plasma' THEN 1 ELSE 0 END) as Plasma,
            SUM(CASE WHEN Specimen_Type = 'Serum' THEN 1 ELSE 0 END) as Serum,
            SUM(CASE WHEN Specimen_Type = 'Buffy_Coat' THEN 1 ELSE 0 END) as Buffy_Coat,
            SUM(CASE WHEN Specimen_Type = 'Fresh_Tissue' THEN 1 ELSE 0 END) as Fresh_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Non-Malignant' THEN 1 ELSE 0 END) as Normal_Tissue,
            SUM(CASE WHEN [Specimen_Pathological.Status] = 'Malignant' THEN 1 ELSE 0 END) as Diseased_Tissue
            FROM data
            GROUP BY Project
            ");

#summary samples by Specimen_Type, Project
df4 <- sqldf("SELECT Specimen_Type, Project,
              SUM(CASE WHEN Year = '2010' THEN 1 ELSE 0 END) as [2010],
              SUM(CASE WHEN Year = '2011' THEN 1 ELSE 0 END) as [2011],
              SUM(CASE WHEN Year = '2012' THEN 1 ELSE 0 END) as [2012],
              SUM(CASE WHEN Year = '2013' THEN 1 ELSE 0 END) as [2013],
              SUM(CASE WHEN Year = '2014' THEN 1 ELSE 0 END) as [2014],
              SUM(CASE WHEN Year = '2015' THEN 1 ELSE 0 END) as [2015],
              SUM(CASE WHEN Year = '2016' THEN 1 ELSE 0 END) as [2016],
              SUM(CASE WHEN Year = '2017' THEN 1 ELSE 0 END) as [2017],
              SUM(CASE WHEN Year = '2018' THEN 1 ELSE 0 END) as [2018],
              SUM(CASE WHEN Year = '2019' THEN 1 ELSE 0 END) as [2019],
              SUM(CASE WHEN Year = '2020' THEN 1 ELSE 0 END) as [2020]
              FROM data
              GROUP BY Specimen_Type,Project

             ");


write.csv(data,"C:/Users/acer/Desktop/Biobank_Dashboards/dataset.csv")

