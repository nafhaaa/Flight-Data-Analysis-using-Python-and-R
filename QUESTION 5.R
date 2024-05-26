

#Use the available variables to construct a model that predicts delays.

library(e1071)
library(ggplot2)
library(caret)
library(dplyr)
library(tidyr)
library(MASS)

data_5<- read.csv("C:/Users/fnafh/OneDrive/Documents/PROGRAMMING/PROGRAMMING COURSEWORK/cleaned_data")
head(data_5)

colnames(data_5)
colSums(is.na(data_5))

#removing columns which wont be used in the model
data_5= subset(data_5 , select = -c(UniqueCarrier ,FlightNum ,TailNum , Cancelled , Diverted ,Year ,
                                    DepTime , ArrTime ,Origin , Dest , ActualElapsedTime , AirTime,CRSElapsedTime , CarrierDelay , WeatherDelay, NASDelay , SecurityDelay))
data_5

#add a column for delaystatus
data_5$delaystatus[(data_5$ArrDelay)>0]<- 1
data_5$delaystatus[(data_5$ArrDelay)<=0]<- 0
data_5

#count the number of 1 and 0 in the delay status column
table(data_5$delaystatus) #Hence, shows that there is no imbalance.


#drop the Arrdelay column
data_5= subset(data_5 ,select=-c(ArrDelay , X))

#plot a correlation matrix using the heatmap graph

library(corrplot)
corrplot(abs(cor(data_5)), method="color" ,  addCoef.col = "black" , number.cex = 0.5 , tl.cex = 0.5
         , is.corr = FALSE , col=colorRampPalette(c("lightblue" , "khaki" , "darkblue"))(100))



#consider features where the correlation between the Arrdelay is more than 0.1
#features selected- CRSDepTime , CRSArrTime , DepDelay , TaxiIn , TaxiOut , LateAircraftDelay , delaystatus



#build the model
library(caTools)
set.seed(290)


x<- data_5[ , c("CRSDepTime" , "CRSArrTime" , "DepDelay" , "TaxiIn" , "TaxiOut" ,"LateAircraftDelay" , "delaystatus")]
y<- data_5$delaystatus
split_data=sample.split(data_5$delaystatus , SplitRatio=0.75)
training_data<- subset(x , split_data== TRUE)
testing_data<- subset(x , split_data== FALSE)


#Scale the data using the standard scaler
scaling <-scale(training_data[,c("CRSDepTime" , "CRSArrTime" , "DepDelay" , "TaxiIn" , "TaxiOut" ,"LateAircraftDelay")])
training_data[,c("CRSDepTime" , "CRSArrTime" , "DepDelay" , "TaxiIn" , "TaxiOut" ,"LateAircraftDelay")] <- scale(training_data[,c("CRSDepTime" , "CRSArrTime" , "DepDelay" , "TaxiIn" , "TaxiOut" ,"LateAircraftDelay")],center= attr(scaling , "scaled:center"), 
                                                                                                                 scale = attr(scaling,"scaled:scale"))
testing_data[,c("CRSDepTime" , "CRSArrTime" , "DepDelay" , "TaxiIn" , "TaxiOut" ,"LateAircraftDelay")] <-scale(testing_data[,c("CRSDepTime" , "CRSArrTime" , "DepDelay" , "TaxiIn" , "TaxiOut" ,"LateAircraftDelay")],center= attr(scaling , "scaled:center"), 
                                                                                                               scale = attr(scaling,"scaled:scale"))


ml<-glm(delaystatus~., data=training_data , family = binomial)
ml
#predict testing data results
predict<- predict(ml , type="response" , newdata = testing_data[,c("CRSDepTime" , "CRSArrTime" , "DepDelay" , "TaxiIn" , "TaxiOut" ,"LateAircraftDelay")])
target_predict<- ifelse(predict>0.5 , 1 , 0)
head(testing_data)



# plot a Confusion matrix
cmx<-table(testing_data[, "delaystatus"] , target_predict>0.5  )
cmx

#Plot a pheatmap using the confusion matrix
library(pheatmap)
pheatmap(cmx , display_numbers=T, fontsize = 14)

#Plot a ROC curve and calculate the AUC 
library(ROCR)
library(pROC)
test_roc<- roc(testing_data$delaystatus~predict , plot=TRUE , print.auc=TRUE , main="ROC GRAPH" , xlab= "False positive rate" , ylab="True positive rate")
#Make the x axis in ascending order 
test_roc$specificities<- rev(test_roc$specificities)
plot(test_roc , col="blue" , lwd=2 , legacy.axes=TRUE ,print.auc=TRUE , main="ROC GRAPH" , xlab= "False positive rate" , ylab="True positive rate" )


