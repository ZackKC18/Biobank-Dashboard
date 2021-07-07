library(tidyverse) 
library(dplyr)

#import raw data
BS1001 <- read.csv("https://www.dropbox.com/s/mt0y2seqcmdfmw8/BS1001data.csv?dl=1")
BS1101 <- read.csv("https://www.dropbox.com/s/31uzgrtlgk3uc2q/BS1101data.csv?dl=1")
BS1103 <- read.csv("https://www.dropbox.com/s/ho7ekafgwik8uzk/BS1103data.csv?dl=1")
BS1202 <- read.csv("https://www.dropbox.com/s/jy61kgqvyeki8nz/BS1202data.csv?dl=1")
BS1203 <- read.csv("https://www.dropbox.com/s/mkg0e2v62v4nnmj/BS1203data.csv?dl=1")
BS1402 <- read.csv("https://www.dropbox.com/s/vy17rdlkg5sxlm0/BS1402data.csv?dl=1")
BS1701 <- read.csv("https://www.dropbox.com/s/a9wwvhha4e9cvuq/BS1701data.csv?dl=1")
BS1904 <- read.csv("https://www.dropbox.com/s/t3b4ya6cqo19o95/BS1904data.csv?dl=1")

#concatenate raw data
data <- bind_rows(BS1001,BS1101,BS1103,BS1202,BS1203,BS1402,BS1701,BS1904)


#import raw data of transfer dataset
BS1001_1st_transfer <- read.csv("https://www.dropbox.com/s/39e6df37mc3ymfl/BS1001_1st_transfer_2020.csv?dl=1")
BS1001_2nd_transfer <- read.csv("https://www.dropbox.com/s/1r8euv513ykkefk/BS1001_2nd_transfer_2020.csv?dl=1")
BS1103_1st_transfer <- read.csv("https://www.dropbox.com/s/9pwxjszktwbx17a/BS1103_1st_transfer_2018.csv?dl=1")
BS1203_1st_transfer <- read.csv("https://www.dropbox.com/s/1cgrhg709r7ehja/BS1203_1st_transfer_2019.csv?dl=1")
BS1204_1st_transfer <- read.csv("https://www.dropbox.com/s/squzceb7eje150c/BS1204_1st_transfer_2017.csv?dl=1")

#combine transfer dataset
data_transfer <- rbind(BS1001_1st_transfer,BS1001_2nd_transfer,BS1103_1st_transfer, BS1203_1st_transfer, BS1204_1st_transfer)


#extract year from datetime
data$Year <- format(as.Date(data$Specimen_Collection.Date,format="%d/%m/%Y"),"%Y" )
data_transfer$Year <- format(as.Date(data_transfer$Exported.On,format="%m/%d/%Y"),"%Y" )

#summary from raw data
mytable <- data %>% count(Year) %>% rename(Total_Sample = n) %>% mutate(Cumulative_Sample = cumsum(Total_Sample))
#summary from transfer data
trans <- data_transfer %>% count(Year) %>% rename(Total_Transfer = n)
#combine raw data with transfer data
merge <- merge(x=mytable,y=trans,by="Year",all.x=TRUE) %>% mutate(Total_Transfer = replace(Total_Transfer, is.na(Total_Transfer),0))
#calculate percent of total tranfer by year
final_table <- merge %>% mutate(Total_Transfer = replace(Total_Transfer, is.na(Total_Transfer),0)) %>%  mutate(Percent_Transfer =Total_Transfer/Cumulative_Sample*100 ) 

