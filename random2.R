library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(sqldf)
library(lubridate)
library(Rcpp)

#import data
BS1001 <- read.csv("C:/data/BS1001data.csv")
BS1101 <- read.csv("C:/data/BS1101data.csv")
BS1103 <- read.csv("C:/data/BS1103data.csv")
BS1202 <- read.csv("C:/data/BS1202data.csv")
BS1203 <- read.csv("C:/data/BS1203data.csv")
BS1402 <- read.csv("C:/data/BS1402data.csv")
BS1701 <- read.csv("C:/data/BS1701data.csv")
BS1904 <- read.csv("C:/data/BS1904data.csv")

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
data$Datetime <- as.Date(data$Specimen_Collection.Date,format="%d/%m/%Y")
data <- select(data, Participant_PPID,Specimen_Type,Specimen_Pathological.Status, Specimen_Barcode, Specimen_Container.Name, Specimen_Container.Position, Project,
               Datetime, Participant_Gender)

# check data type of Datetime
class(data$Datetime)

max_date <- max(data$Datetime) 
min_date <- min(data$Datetime)
# date of 6 latest months
x <- seq(as.Date(max_date), to=as.Date(min_date), by='-6 months')[2]
# "2019-09-19"

# dataframe 6 latest months
df <- data[data$Datetime > x &
             data$Datetime <= max_date, ]

# random 45 samples ------------------------------------------------------------------------------------------

set.seed(1)

randdata <- sample_n(df, 45)
randdata

# random 45% of samples in 6 latest months

set.seed(2)

randdf <- df %>% sample_frac(.45)
randdf
