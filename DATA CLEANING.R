#uploading the datasets and installing necessary libraries
library(readr) ; library(tidyr) ; library(dplyr)

dataset_2006<- read.csv("C:/Users/fnafh/OneDrive/Documents/PROGRAMMING/PROGRAMMING COURSEWORK/2006.csv.bz2")
dataset_2007 <- read.csv("C:/Users/fnafh/OneDrive/Documents/PROGRAMMING/PROGRAMMING COURSEWORK/2007.csv.bz2")
combined_dataset=rbind(dataset_2006,dataset_2007)

head(combined_dataset)

#get the dataset structure
str(combined_dataset)
#dimension of the dataset
dim(combined_dataset)
 
#check for null values
colSums(is.na(combined_dataset))

#remove the cancellation column as most of the values are empty
combined_dataset$CancellationCode<- NULL
head(combined_dataset)

#remove rows with empty values
combined_dataset <-na.omit(combined_dataset) 
dim(combined_dataset)

#number of rows removed
14595137 -14279090 

#remove time outliers
max(combined_dataset$DepTime)
max(combined_dataset$CRSDepTime)
max(combined_dataset$ArrTime)
max(combined_dataset$CRSArrTime)

#time values more than 2359 has to be removed
combined_dataset <- filter(combined_dataset, ArrTime<2400 , DepTime <2400  )
max(combined_dataset$ArrTime)
max(combined_dataset$DepTime)

#Saving cleaned data to a new variable which could be used for other questions
write.csv(combined_dataset , "C:/Users/fnafh/OneDrive/Documents/PROGRAMMING/PROGRAMMING COURSEWORK/cleaned_data.csv" , row.names=FALSE )




















