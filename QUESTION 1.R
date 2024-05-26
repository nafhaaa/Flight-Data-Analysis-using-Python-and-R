


data<- read.csv("C:/Users/fnafh/OneDrive/Documents/PROGRAMMING/PROGRAMMING COURSEWORK/cleaned_data")
head(data)

#create a new column adding the departure delay and arrival delay to the data's dataframe
data$sum_of_delays = data$ArrDelay + data$DepDelay 
head(data)

#BEST DAY OF THE WEEK
#group data in terms of week and get the average sum of delays of each group 
#Save the data to a variable called dow_mean
dow_mean<- aggregate(data$sum_of_delays,list(data$DayOfWeek), FUN=mean)
dow_mean
colnames(dow_mean)[2]<- "mean_delay_of_week"
colnames(dow_mean)[1]<- "Dayofweek"
dow_mean
#plot a bar plot
library(ggplot2)
ggplot(data=dow_mean ,aes(x=Dayofweek , y= mean_delay_of_week , fill=Dayofweek)) + 
  geom_bar(stat = "identity" )  
#hence Saturday is the best day of the week 



#BEST YEAR OF THE TIME
dom_mean<- aggregate(data$sum_of_delays,list(data$Month), FUN=mean)
dom_mean
colnames(dom_mean)[2]<- "mean_delay_of_month"
colnames(dom_mean)[1]<- "Month"
dom_mean
ggplot(data=dom_mean ,aes(x=Month , y= mean_delay_of_month , fill=Month)) + geom_bar(stat = "identity") 

#the month September has the minimum delay hence the best month to fly



#BEST TIME OF THE DAY TO FLY

#Add a new coloumn named "CRSDep_interval"

data$CRSDep_interval<- NULL
data$CRSDep_interval[data$CRSDepTime<600]<- "Early Morning"
data$CRSDep_interval[data$CRSDepTime>=600 & data$CRSDepTime<1200]<- "Morning"
data$CRSDep_interval[data$CRSDepTime>=1200 & data$CRSDepTime<1800]<- "Evening"
data$CRSDep_interval[data$CRSDepTime>=1800 & data$CRSDepTime<=2359]<- "Night"
data

#plot a barplot by grouping by the CRSDep_interval

dod_mean<- aggregate(data$sum_of_delays,list(data$CRSDep_interval), FUN=mean)
dod_mean
colnames(dod_mean)[2]<- "mean_delay_of_day"
colnames(dod_mean)[1]<- "CRSDep_interval"
dod_mean
ggplot(data=dod_mean ,aes(x=CRSDep_interval , y= mean_delay_of_day , fill=CRSDep_interval)) + geom_bar(stat = "identity") 

#hence the best time of the day to fly is early morning


