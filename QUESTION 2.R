
library(readr) ; library(tidyr) ; library(dplyr)

#Do older planes suffer more delays?
data_cleaned<- read.csv("C:/Users/fnafh/OneDrive/Documents/PROGRAMMING/PROGRAMMING COURSEWORK/cleaned_data")
data_plane<- read.csv("C:/Users/fnafh/OneDrive/Documents/PROGRAMMING/PROGRAMMING COURSEWORK/plane-data.csv")

#Take necessary columns in the data_plane and rename the columns based on the data_cleaned dataset
names(data_plane)[names(data_plane) == "year"] <- "manufactured_year"
names(data_plane)[names(data_plane) == "tailnum"] <- "TailNum"
head(data_plane)
data_plane<-select(data_plane , manufactured_year , TailNum)
data_plane

#Remove rows which has no years under manufactured column

data_plane<- subset(data_plane , manufactured_year!="" , manufactured_year!= "0")
data_plane

#Merge the data_cleaned and data_plane on the tailnum column
merged_data<- inner_join(data_plane , data_cleaned , by="TailNum")
merged_data
str(merged_data)

#add a column  indicating the age of the plane , sum of delays and remove rows where age of plane less than or equal to zero
merged_data$manufactured_year<-as.integer(merged_data$manufactured_year)
merged_data$age_of_plane <- merged_data$Year - merged_data$manufactured_year
merged_data
merged_data<- subset(merged_data , age_of_plane>0)

merged_data$sum_of_delays = merged_data$ArrDelay + merged_data$DepDelay 

#group merged_data by the age of the plane and find the mean based on the sum of delays
delay_byageofplane<- aggregate(merged_data$sum_of_delays,list(merged_data$age_of_plane), FUN=mean)
delay_byageofplane
delay_byageofplane<-delay_byageofplane[1:50,]
delay_byageofplane

#rename the column names
colnames(delay_byageofplane)[2]<- "average_delay_by_age"
colnames(delay_byageofplane)[1]<- "age_of_plane"
delay_byageofplane
#draw a scatter plot
library(ggplot2)
ggplot(delay_byageofplane , aes( x = age_of_plane , y = average_delay_by_age )) + geom_point(stat = "identity" , fill="blue") +
  geom_smooth(method= "lm" , se=TRUE)

cor(delay_byageofplane$average_delay_by_age , delay_byageofplane$age_of_plane)
#hence there is a positive relationship between average_delay_byageofplane and age_of_plane










