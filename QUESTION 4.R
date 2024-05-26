

library(readr) ; library(tidyr) ; library(dplyr) ; library(lubridate) ; library(magrittr) ; library(zoo)
#Can you detect cascading failures as delays in one airport create delays in others?

#import the data
data<- read.csv("C:/Users/fnafh/OneDrive/Documents/PROGRAMMING/PROGRAMMING COURSEWORK/cleaned_data")
head(data)

#add a column with date
data$Date<- as.Date(with(data , paste(Year , Month , DayofMonth , sep = "-")) , "%Y-%m-%d")
data

#add a column with date and time 

date_time_function<- function(year , month , day,time){
  make_datetime(year , month , day , time%/%100 , time%%100)
}

data$DepTime <- as.integer(data$DepTime)
data<- data%>% mutate(DateTime = date_time_function(Year , Month , DayofMonth , DepTime))

data

#filter out necessary columns

data<- select(data ,Date , TailNum , DepTime ,ArrTime , ArrDelay , DepDelay , Origin , Dest  , DateTime)
data
str(data)


#remove column with TailNum=0
data<- data[data$TailNum!="0" ,]
data

#sort the values by the DateTime column and date

data<- data[order(data$TailNum , data$DateTime) , ]
data

#add a column for lagged destination and check if the dest is same as the origin of the next flight
data<-data%>% mutate(set_origin =lag(Dest , n=1))

data

#add a column if the origin and set_origin is matched returning 1 else 0
data$origin_matched[(data$Origin) == (data$set_origin) ] <- 1
data$origin_matched[(data$Origin) != (data$set_origin) ] <- 0
data


#add a column for the lagged TailNum and check if the current tailnum is same as the tailnum of the next flight
data<-data%>% mutate(set_tailnumber =lag(TailNum , n=1))
data
#add a column if the TailNum and set_tailnumber is matched returning 1 else 0
data$tailnumber_matched[(data$TailNum) == (data$set_tailnumber) ] <- 1
data$tailnumber_matched[(data$TailNum) != (data$set_tailnumber) ] <- 0
data


#consider rows only if the tailnumber_matched=1 and origin_matched=1
data<- data[(data$origin_matched==1) & (data$tailnumber_matched==1) , ]
data

#add a column for the lagged arrival delay
data<- data%>%
  mutate(following_Arr_delay = lead(ArrDelay))

data

#change the column name
colnames(data)[5]<- "Current_Arr_delay"
data

data<- na.omit(data)
data

#plot a scatter plot between the Current_Arr_delay and following_Arr_delay and calculate the correlation 
cor(data$Current_Arr_delay , data$following_Arr_delay)

library(ggplot2)
ggplot(data=data , aes(x=Current_Arr_delay , y= following_Arr_delay))+geom_point(stat = "identity" , fill="blue") +
  geom_smooth(method="lm" , se=TRUE)


#consider values which has Current_Arr_delay being greater than zero

data<- data[data$Current_Arr_delay >0 , ]
data

#consider it being a cascading delay (cascading delay = 1 ) only if the following_Arr_Delay is greater than 0 and the Current_Arr_delay is greater than 0 

data$cascading_failure <- NULL
data$cascading_failure[(data$Current_Arr_delay >0) & (data$following_Arr_delay>0)] <- 1
data$cascading_failure[(data$Current_Arr_delay >0) & (data$following_Arr_delay<0)] <- 0
data
#construct a catplot for the cascading failures ( 1= cascading failure , 0= not a cascading failure)
data<- na.omit(data)
data

ggplot(data , aes(x=factor(cascading_failure) , fill= cascading_failure)) + geom_bar()  
#hence more proportion of the flights end up in a cascading failure due to the previous flight being delayed(considering the arrival delay) 













