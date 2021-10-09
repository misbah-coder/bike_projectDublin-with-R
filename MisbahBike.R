#dataset(https://data.gov.ie/dataset/dublin-city-centre-cycle-counts?package_type=dataset)
#rename the files as same and change the colum name "Date and Time" to "Date-Time"
install.packages("tidyverse")
install.packages("data.table")              # Install data.table package
#library
library(dplyr)
library(tidyverse)
# Data Visualization
library(gridExtra)
library(ggplot2)
# Displaying results
library(IRdisplay)
library(repr)
# Machine Learning
library(caret)
#Data import
library(readr)
library(readxl)
library(lubridate)
library("data.table")
# Convert to a tibble
library("tibble")

#import file 
dublin_count_jan_dec_2019_cycle <- read_excel("dublin_count_jan_dec_2019_cycle.xlsx")
View(dublin_count_jan_dec_2019_cycle)

dublin_count_jan_dec_2020_cycle <- read_excel("dublin_count_jan_dec_2020_cycle.xlsx")
View(dublin_count_jan_dec_2020_cycle)

dublin_count_jan_july_2021_cycle <- read_excel("dublin_count_jan_july_2021_cycle.xlsx")
View(dublin_count_jan_july_2021_cycle)

#splitting of date and time for 2019

df19<-dublin_count_jan_dec_2019_cycle
df19$Time <- format(as.POSIXct(df19$`Date and Time`,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
df19$Date <- format(as.POSIXct(df19$`Date and Time`,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")
View(df19)

df20<-dublin_count_jan_dec_2020_cycle
df20$Time <- format(as.POSIXct(df20$`Date and Time`,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
df20$Date <- format(as.POSIXct(df20$`Date and Time`,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")
View(df20)

df21<-dublin_count_jan_july_2021_cycle
df21$Time <- format(as.POSIXct(df21$`Date and Time`,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
df21$Date <- format(as.POSIXct(df21$`Date and Time`,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")
View(df21)

#cleaning 
# taking a quick look
glimpse(df19)
glimpse(df20)
glimpse(df21)

# converts doubles to integers
#dropping column same in all table and what to do with "in" and "out" column
df20[3:4]<- NULL
df20[4:5]<- NULL
df20[5:6]<- NULL
View(df20)

df21[3:4]<- NULL
df21[4:5]<- NULL
df21[5:6]<- NULL
df21[6:7]<- NULL
df21[7:8]<- NULL
df21[8:9]<- NULL
df21[9:10]<- NULL
df21[12:13]<- NULL
df21[13:14]<- NULL
View(df21)

#renaming column 
setnames(df19, old = c('Grove Road Totem','North Strand Rd S/B',
                       'North Strand Rd N/B','Charleville Mall','Guild Street'),
         new = c('Grove_Road','North_Strand_Rd_S_B','North_Strand_Rd_N_B','Charleville_Mall','Guild_Street'))
View(df19)

setnames(df20, old = c('Charleville Mall','Grove Road Totem',
                       'Guild Street','North Strand Rd S/B',
                       'North Strand Rd N/B'),
         new = c('Charleville_Mall','Grove_Road',
                 'Guild_Street','North_Strand_Rd_S_B',
                 'North_Strand_Rd_N_B'))
View(df20)

setnames(df21, old = c('Charleville Mall','Clontarf - James Larkin Rd',
                       'Clontarf - Pebble Beach Carpark','Drumcondra Cyclists 1',
                       'Drumcondra Cyclists 2','Grove Road Totem','Guild Street','North Strand Rd N/B',
                       'North Strand Rd S/B','Richmond Street Cyclists 1','Richmond Street Cyclists 2'),
         new = c('Charleville_Mall','Clontarf_James_Larkin_Rd',
                 'Clontarf_Pebble_Beach_Carpark','Drumcondra_Cyclists_1',
                 'Drumcondra_Cyclists_2','Grove_Road','Guild_Street','North_Strand_Rd_N_B',
                 'North_Strand_Rd_S_B','Richmond_Street_Cyclists_1','Richmond_Street_Cyclists_2'))
View(df21)

#drop Date and Time column fro all tables 
df19[1]<- NULL
View(df19)

df20[1]<- NULL
View(df20)

df21[1]<- NULL
View(df21)

#splitting of date
df19$Date<-as.character(df19$Date)
split_date<-strsplit(df19$Date,":")
split_date<-as.numeric(unlist(split_date))
split_date<-matrix(split_date,dim(df19)[1],3,byrow =T)
df19$Day<-split_date[,3]
df19$Month<-split_date[,2]
df19$Year<-split_date[,1]
View(df19)

df20$Date<-as.character(df20$Date)
split_date<-strsplit(df20$Date,":")
split_date<-as.numeric(unlist(split_date))
split_date<-matrix(split_date,dim(df20)[1],3,byrow =T)
df20$Day<-split_date[,3]
df20$Month<-split_date[,2]
df20$Year<-split_date[,1]
View(df20)

df21$Date<-as.character(df21$Date)
split_date<-strsplit(df21$Date,":")
split_date<-as.numeric(unlist(split_date))
split_date<-matrix(split_date,dim(df21)[1],3,byrow =T)
df21$Day<-split_date[,3]
df21$Month<-split_date[,2]
df21$Year<-split_date[,1]
View(df21)

# Get column names
colnames(df19)
colnames(df20)
colnames(df21)

#It’s possible to reorder the column by position as follow:
my_df19 <- df19[, c(1, 4, 5, 2, 3, 6,8, 9 , 10)]
View(my_df19)

my_df20 <- df20[, c(2, 1, 3, 5, 4, 6,8, 9 , 10)]
View(my_df20)

my_df21 <- df21[, c(6, 1, 7, 9, 8, 12, 14, 15, 16)]
View(my_df21)

#WORKING ON NA 
sum(is.na(my_df19))
sum(is.na(my_df20))
sum(is.na(my_df21))

#Excluding Missing Values from Analyses using Arithmetic functions mean
#2019 cols
sum(is.na(my_df19$Grove_Road))
my_df19$Grove_Road[is.na(my_df19$Grove_Road)] <- mean(my_df19$Grove_Road, na.rm = TRUE)
round(my_df19$Grove_Road, 2)
sum(is.na(my_df19$Grove_Road))

sum(is.na(my_df19$Guild_Street))
my_df19$Guild_Street[is.na(my_df19$Guild_Street)] <- mean(my_df19$Guild_Street, na.rm = TRUE)
round(my_df19$Guild_Street, 2)
sum(is.na(my_df19$Guild_Street))

#no need 
sum(is.na(my_df19$Charleville_Mall))
sum(is.na(my_df19$North_Strand_Rd_S_B))
sum(is.na(my_df19$North_Strand_Rd_N_B))

#2020 cols
sum(is.na(my_df20$Grove_Road))
my_df20$Grove_Road[is.na(my_df20$Grove_Road)] <- mean(my_df20$Grove_Road, na.rm = TRUE)
round(my_df20$Grove_Road, 2)
sum(is.na(my_df20$Grove_Road))

sum(is.na(my_df20$Guild_Street))
my_df20$Guild_Street[is.na(my_df20$Guild_Street)] <- mean(my_df20$Guild_Street, na.rm = TRUE)
round(my_df20$Guild_Street, 2)
sum(is.na(my_df20$Guild_Street))

sum(is.na(my_df20$Charleville_Mall))
my_df20$Charleville_Mall[is.na(my_df20$Charleville_Mall)] <- mean(my_df20$Charleville_Mall, na.rm = TRUE)
round(my_df20$Charleville_Mall, 2)
sum(is.na(my_df20$Charleville_Mall))

sum(is.na(my_df20$North_Strand_Rd_S_B))
my_df20$North_Strand_Rd_S_B[is.na(my_df20$North_Strand_Rd_S_B)] <- mean(my_df20$North_Strand_Rd_S_B, na.rm = TRUE)
round(my_df20$North_Strand_Rd_S_B, 2)
sum(is.na(my_df20$North_Strand_Rd_S_B))

sum(is.na(my_df20$North_Strand_Rd_N_B))
my_df20$North_Strand_Rd_N_B[is.na(my_df20$North_Strand_Rd_N_B)] <- mean(my_df20$North_Strand_Rd_N_B, na.rm = TRUE)
round(my_df20$North_Strand_Rd_N_B, 2)
sum(is.na(my_df20$North_Strand_Rd_N_B))

#2021 cols
sum(is.na(my_df21$Grove_Road))
my_df21$Grove_Road[is.na(my_df21$Grove_Road)] <- mean(my_df21$Grove_Road, na.rm = TRUE)
round(my_df21$Grove_Road, 2)
sum(is.na(my_df21$Grove_Road))

sum(is.na(my_df21$Guild_Street))
my_df21$Guild_Street[is.na(my_df21$Guild_Street)] <- mean(my_df21$Guild_Street, na.rm = TRUE)
round(my_df21$Guild_Street, 2)
sum(is.na(my_df21$Guild_Street))

sum(is.na(my_df21$Charleville_Mall))
my_df21$Charleville_Mall[is.na(my_df21$Charleville_Mall)] <- mean(my_df21$Charleville_Mall, na.rm = TRUE)
round(my_df21$Charleville_Mall, 2)
sum(is.na(my_df21$Charleville_Mall))

sum(is.na(my_df21$North_Strand_Rd_S_B))
my_df21$North_Strand_Rd_S_B[is.na(my_df21$North_Strand_Rd_S_B)] <- mean(my_df21$North_Strand_Rd_S_B, na.rm = TRUE)
round(my_df21$North_Strand_Rd_S_B, 2)
sum(is.na(my_df21$North_Strand_Rd_S_B))

sum(is.na(my_df21$North_Strand_Rd_N_B))
my_df21$North_Strand_Rd_N_B[is.na(my_df21$North_Strand_Rd_N_B)] <- mean(my_df21$North_Strand_Rd_N_B, na.rm = TRUE)
round(my_df21$North_Strand_Rd_N_B, 2)
sum(is.na(my_df21$North_Strand_Rd_N_B))


#identical test

identical(names(my_df19[["Grove_Road"]]), names(my_df20[["Grove_Road"]]) )
identical(names(my_df19[[2]]), names(my_df20[[2]]) )
identical(names(my_df19[[3]]), names(my_df20[[3]]) )
idsum(is.na(total))entical(names(my_df19[[4]]), names(my_df20[[4]]) )
identical(names(my_df19[[5]]), names(my_df20[[5]]) )
identical(names(my_df19[[6]]), names(my_df20[[6]]) )
identical(names(my_df19[[7]]), names(my_df20[[7]]) )
identical(names(my_df19[[8]]), names(my_df20[[8]]) )
identical(names(my_df19[[9]]), names(my_df20[[9]]) )

#joining data
total <- rbind(my_df19, my_df20)                # rbind two data frames in R
#View(total)
#sum(is.na(total))
total <- rbind(total, my_df21)
View(total)
sum(is.na(total))

#grapy plot 
install.packages("ggplot2")                 # Install & load ggplot2
library("ggplot2")

#convert to CSV
write.table(total, file="bike_traffic.csv",sep=",",row.names = FALSE)


