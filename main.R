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

#import data
BS1001 <- read.csv("C:/Users/acer/Desktop/data/BS1001data.csv")
BS1101 <- read.csv("C:/Users/acer/Desktop/data/BS1101data.csv")
BS1103 <- read.csv("C:/Users/acer/Desktop/data/BS1103data.csv")
BS1202 <- read.csv("C:/Users/acer/Desktop/data/BS1202data.csv")
BS1203 <- read.csv("C:/Users/acer/Desktop/data/BS1203data.csv")
BS1402 <- read.csv("C:/Users/acer/Desktop/data/BS1402data.csv")
BS1701 <- read.csv("C:/Users/acer/Desktop/data/BS1701data.csv")
BS1904 <- read.csv("C:/Users/acer/Desktop/data/BS1904data.csv")

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
View(data)


#extract year from datetime
data$Year <- format(as.Date(data$Specimen_Collection.Date,format="%d/%m/%Y"),"%Y" )


#data cleaning
vis_miss(data)


#Visualization for imputation

data %>% select(Year_Regis,Year)
plot(data$Participant_PPID, data$Year, type = "l", col = 1, ylim = c(2010, 2020))
lines(data$Participant_PPID, data$Year_Regis, type = "l", col = 2)

data$compare <- ifelse(data$Year_Regis == data$Year, TRUE ,
                       ifelse(data$Year_Regis != Year, FALSE))
data$compare



#Replace NA value with the value from another column
data$Year <- ifelse(is.na(data$Year), data$Year_Regis, data$Year)


#filter row with missing both registration date and specimen collection date
sum(is.na(data$Year))
data %>% 
  filter(is.na(Year_Regis) & is.na(Year))


#There are some NA in column year because there are also NA in Year_regis .
#I delete them
data <- data[!is.na(data$Year),]
sum(is.na(data$Year)) #Now column year is complete


#select only useful columns
data <- select(data,Participant_PPID,Specimen_Type,Specimen_Pathological.Status,Project,Year)
colnames(data)


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
View(df)


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
View(df5)

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

View(df4)

#switch from wide to long by melt df
df_table <- as.data.table(df, keep.rownames = TRUE)
df_table <- select(df,Year,Project,Plasma,Serum,Buffy_Coat,Fresh_Tissue)
df_table
df_melt <-melt(df_table,id = c("Year","Project"))
df_melt


#write.csv(df,"C:/Users/acer/Desktop/data/df.csv")
#write.csv(df_melt,"C:/Users/acer/Desktop/data/df_melt.csv")
#write.csv(df4,"C:/Users/acer/Desktop/data/df4.csv")
#write.csv(data,"C:/Users/acer/Desktop/data/combine.csv")

