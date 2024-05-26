library(readr) ; library(tidyr) ; library(dplyr) ; library(lubridate) ; library(magrittr)
#How does the number of people flying between different locations change over time?

#import the data , filter out necessary columns and merge the data
data_cleaned3<- read.csv("C:/Users/fnafh/OneDrive/Documents/PROGRAMMING/PROGRAMMING COURSEWORK/cleaned_data")
data_airport<- read.csv("C:/Users/fnafh/OneDrive/Documents/PROGRAMMING/PROGRAMMING COURSEWORK/airports.csv")
data_cleaned3<- select(data_cleaned3 , Origin , Month , Year , Dest)
data_airport<- select(data_airport , iata , state)
names(data_airport)[names(data_airport) == "iata"] <- "Origin"   #rename the iata column
merged_data2<- inner_join(data_cleaned3 , data_airport , by= "Origin")
head(merged_data2)

str(merged_data2)
#change the month and year type to character 
merged_data2$Month<- as.integer(merged_data2$Month)
merged_data2$Year<- as.integer(merged_data2$Year)
str(merged_data2)
#convert to date and time format
library(zoo)
merged_data2$Year_Month <- with(merged_data2 , sprintf("%d-%02d" , Year , Month))
merged_data2

#group data by year_month and get the count of the flights 
grouped_data1<- merged_data2 %>% group_by(Year_Month) %>%
  summarise(number_of_people=n(),.groups= "drop") %>%
  as.data.frame()

grouped_data1
str(grouped_data1)

#plot a line plot 
library(ggplot2)


ggplot(grouped_data1 , aes( x=Year_Month , y=number_of_people, group = 1)) + geom_line()

#number of people travelling from a particular origin to a particular destination

merged_data2$Origin_Dest = paste(merged_data2$Origin , merged_data2$Dest , sep = "-")
merged_data2

#CONSIDER FOR 2006


#get rows only with year 2006
merged_data_2006<- merged_data2[merged_data2$Year=="2006" , ]
merged_data_2006
 # group by Origin_Dest
merged_data_2006_new<-merged_data_2006 %>% group_by(Origin_Dest) %>%
  summarise(Number_of_people=n(),.groups= "drop") %>%
  as.data.frame()
#arrange in descending order 

 merged_data_2006_new <- merged_data_2006_new %>% arrange(desc(Number_of_people))

merged_data_2006_new

#filter out merged_data_2006 having only the Origin-Dest values which are part of the Top 5 number of people

merged_data_2006_f <- merged_data_2006[merged_data_2006$Origin_Dest== "LAX-SAN" |merged_data_2006$Origin_Dest== "SAN-LAX" | 
                                         merged_data_2006$Origin_Dest== "OGG-HNL" | merged_data_2006$Origin_Dest== "HNL-OGG" |
                                       merged_data_2006$Origin_Dest== "LAX-LAS" , ]


merged_data_2006_f<- merged_data_2006_f %>% group_by(Origin_Dest , Year_Month) %>%
  summarise(Number_of_people=n(),.groups= "drop") %>%
  as.data.frame()

merged_data_2006_f

#plot a line graph for each of the Origin_Dest selected in 2006

ggplot(data=merged_data_2006_f, aes(Year_Month , Number_of_people , group=Origin_Dest , colour=Origin_Dest))+ geom_line()
+ scale_x_continuous("Year_Month" , labels=as.character(merged_data_2006_f$Year_Month) , breaks = merged_data_2006_f$Year_Month) +
  geom_point()

#CONSIDER FOR 2007


#get rows only with year 2007
merged_data_2007<- merged_data2[merged_data2$Year=="2007" , ]
merged_data_2007
# group by Origin_Dest
merged_data_2007_new<-merged_data_2007 %>% group_by(Origin_Dest) %>%
  summarise(Number_of_people=n(),.groups= "drop") %>%
  as.data.frame()
#arrange in descending order 
merged_data_2007_new <- merged_data_2007_new %>% arrange(desc(Number_of_people))

merged_data_2007_new

#filter out merged_data_2007 having only the Origin-Dest values which are part of the Top 5 number of people

merged_data_2007_f <- merged_data_2007[merged_data_2007$Origin_Dest== "OGG-HNL" |merged_data_2007$Origin_Dest== "HNL-OGG" | 
                                         merged_data_2007$Origin_Dest== "LAX-LAS" | merged_data_2007$Origin_Dest== "LAS-LAX" |
                                         merged_data_2007$Origin_Dest== "HNL-LIH" , ]


merged_data_2007_f<- merged_data_2007_f %>% group_by(Origin_Dest , Year_Month) %>%
  summarise(Number_of_people=n(),.groups= "drop") %>%
  as.data.frame()

merged_data_2007_f

#plot a line graph for each of the Origin_Dest selected in 2007

ggplot(data=merged_data_2007_f, aes(Year_Month , Number_of_people , group=Origin_Dest , colour=Origin_Dest))+ geom_line()
+ scale_x_continuous("Year_Month" , labels=as.character(merged_data_2007_f$Year_Month) , breaks = merged_data_2007_f$Year_Month) +
  geom_point()




