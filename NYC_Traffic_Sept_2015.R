
setwd(tempdir()) 
#1. QUESTION NO :1 
getwd()
#setting a temporary working directory since the size of that dataset is huge
download.file("https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv", "green_taxi.csv")
green_taxi<-read.csv("green_taxi.csv")
#programmatically downloading the dataset from the given link and loading into the dataframe named green_taxi
dim(green_taxi)
#Displaying the no of rows and columns in the dataset

attach(green_taxi)
#attaching so as to directly use the column names instead of using the $ sign

range(Trip_distance)
summary(Trip_distance)

green_taxi_ten <- green_taxi[Trip_distance < 10,]
green_taxi_four <- green_taxi[Trip_distance < 4,]
green_taxi_one <- green_taxi[Trip_distance < 1,]

par(mfrow=c(2,2))

hist(Trip_distance,main = "Histogram for the given Data Set",col=c("steelblue"))
hist(green_taxi_ten$Trip_distance,main="Histogram for the given Data Set with Trip distance less than 10",xlab="Trip Distance Less than 10",col=c("red"))
hist(green_taxi_four$Trip_distance,main="Histogram for the given Data Set with Trip distance less than 4",xlab="Trip Distance Less than 4",col=c("green"))
hist(green_taxi_one$Trip_distance,main="Histogram for the given Data Set with Trip distance less than 1",xlab="Trip Distance Less than 1",col=c("yellow"))

boxplot(Trip_distance,main = "Boxplot for the given Data Set",col=c("steelblue"))
boxplot(green_taxi_ten$Trip_distance,main="Boxplot for the given Data Set with Trip distance less than 10",xlab="Trip Distance Less than 10",col=c("red"))
boxplot(green_taxi_four$Trip_distance,main="Boxplot for the given Data Set with Trip distance less than 4",xlab="Trip Distance Less than 4",col=c("green"))
boxplot(green_taxi_one$Trip_distance,main="Boxplot for the given Data Set with Trip distance less than 1",xlab="Trip Distance Less than 1",col=c("yellow"))

dev.off()


green_taxi_hour <- green_taxi
green_taxi_hour$Lpep_dropoff_datetime <- as.numeric(substr(as.character(green_taxi_hour$Lpep_dropoff_datetime),12,13))
mean_hour_dist <- c()
med_hour_dist <- c()
returnMeanTripDistance <-function(){
  
  for (a in c(0:23)) {
    mean_hour_dist <<- c(mean_hour_dist , mean(green_taxi_hour[green_taxi_hour$Lpep_dropoff_datetime == a,]$Trip_distance))
    med_hour_dist <<- c(med_hour_dist , median(green_taxi_hour[green_taxi_hour$Lpep_dropoff_datetime == a,]$Trip_distance))
  }
  
}
returnMeanTripDistance()
mean_hour_dist<-data.frame(c(0:23),mean_hour_dist,med_hour_dist)
colnames(mean_hour_dist) <- c("Hour","MeanDistance","MedianDistance")


jfk <- green_taxi[(Pickup_latitude >= 40.63 & Pickup_latitude <= 40.66 & Pickup_longitude <= -73.76 & Pickup_longitude >= -73.78)|(Dropoff_latitude >= 40.63 & Dropoff_latitude <= 40.66 & Dropoff_longitude <= -73.76 & Dropoff_longitude >= -73.78),]
dim(jfk)
jfk[1,]
avg_fair <- mean(jfk$Fare_amount)

tip_percentage <- Tip_amount/Fare_amount*100
green_taxi$tip_percentage <- tip_percentage
green_taxi_fare_amount<-green_taxi[tip_percentage>0&tip_percentage<100&Fare_amount>0,]
summary(green_taxi_fare_amount$Fare_amount)
hist(tip_vector)
predicted_tip <- c()
predictTip <-function(){
  
  for (a in c(0:20)) {
    tip_vector<-green_taxi_fare_amount[green_taxi_fare_amount$Fare_amount>=a&green_taxi_fare_amount$Fare_amount<a+1,]$tip_percentage
    tip<- names(sort(table(tip_vector),decreasing=TRUE))[1]
    predicted_tip <<- c(predicted_tip , tip)
  }
  tip_vector<-green_taxi_fare_amount[green_taxi_fare_amount$Fare_amount>21,]$tip_percentage
  tip<- names(sort(table(tip_vector),decreasing=TRUE))[1]
  predicted_tip <<- c(predicted_tip , tip)
}
predictTip()
summary(green_taxi$Fare_amount)
range <- c()
fareRange <-function(){
  
  for (a in c(0:20)) {
    r<- paste(paste(a,' - '),a+1)
    range <<- c(range , r)
  }
  range <<- c(range , '>21')
}
fareRange()
tip_prediction <- data.frame(range,predicted_tip)
colnames(tip_prediction) <- c("MeanDistance","Tip")



x<-as.POSIXct(as.character(green_taxi$Lpep_dropoff_datetime),format="%Y-%m-%d %H:%M:%S")
y<-as.POSIXct(as.character(green_taxi$lpep_pickup_datetime),format="%Y-%m-%d %H:%M:%S")
time<-as.numeric(difftime(x, y,units="hours"))
green_taxi$Trip_Time <- time
avgspeed<-green_taxi$Trip_distance/time
day<-as.numeric(substr(as.character(green_taxi$Lpep_dropoff_datetime),9,10))
hours<-as.numeric(substr(as.character(green_taxi$Lpep_dropoff_datetime),12,13))
green_taxi$Hours <- hours
week<-ceiling(day/7)
avgspeed[!is.finite(avgspeed)] <- 0
#Replacing NA, INF NAN by 0
green_taxi$Average_Speed <- avgspeed

green_taxi$Week <- as.factor(week)
green_taxi[green_taxi$Hours==0,]$Hours<-24





summary(green_taxi[green_taxi$Week==1,]$Average_Speed)
summary(green_taxi[green_taxi$Week==2,]$Average_Speed)
summary(green_taxi[green_taxi$Week==3,]$Average_Speed)
summary(green_taxi[green_taxi$Week==4,]$Average_Speed)

green_taxi[green_taxi$Average_Speed==35370,]
#For a trip distance of 19.65 ,it is impossible to cover the distance in 3 seconds , hence lets remove outliers

par(mfrow=c(2,2))
boxplot(green_taxi[green_taxi$Week==1,]$Average_Speed,col="steelblue",main="First Week")
boxplot(green_taxi[green_taxi$Week==2,]$Average_Speed,col="red",main="Sec Week")
boxplot(green_taxi[green_taxi$Week==3,]$Average_Speed,col="Green",main="Third Week")
boxplot(green_taxi[green_taxi$Week==4,]$Average_Speed,col="Yellow",main="Fourth Week")

fWeek <- green_taxi[green_taxi$Week==1,]
sWeek <- green_taxi[green_taxi$Week==2,]
tWeek <- green_taxi[green_taxi$Week==3,]
forWeek <- green_taxi[green_taxi$Week==4,]



q <-fWeek$Average_Speed[!fWeek$Average_Speed %in% boxplot.stats(fWeek$Average_Speed)$out]
w <-sWeek$Average_Speed[!sWeek$Average_Speed %in% boxplot.stats(sWeek$Average_Speed)$out]
e <-tWeek$Average_Speed[!tWeek$Average_Speed %in% boxplot.stats(tWeek$Average_Speed)$out]
r <-forWeek$Average_Speed[!forWeek$Average_Speed %in% boxplot.stats(forWeek$Average_Speed)$out]

 

summary(q)
summary(w)
summary(e)
summary(r)      

boxplot(q,col="steelblue",main="First Week")
boxplot(w,col="red",main="Sec Week")
boxplot(e,col="Green",main="Third Week")
boxplot(r,col="Yellow",main="Fourth Week")

kruskal.test(list(green_taxi[green_taxi$Week==1 ,]$Average_Speed,
                  green_taxi[green_taxi$Week==2 ,]$Average_Speed,
                  green_taxi[green_taxi$Week==3 ,]$Average_Speed,
                  green_taxi[green_taxi$Week==4 ,]$Average_Speed))
kruskal.test(list(q,w,e,r))
#Null Hypothesis : There is no significant difference in the average speeds among 4 weeks in september
#Alternate Hypothesis : There is significant difference in the average speeds among 4 weeks in september


#Since the p-value is less than .01 we reject the null hypothesis at 99% confidence interval
#i.e  there is significant difference in the average speeds among 4 weeks in september

dev.off()

meanWEekSpeedsWithoutOutliers <- c(mean(q),mean(w),mean(e),mean(r))

meanWEekSpeeds <- c(mean(green_taxi[green_taxi$Week==1,]$Average_Speed),mean(green_taxi[green_taxi$Week==2,]$Average_Speed)
                    ,mean(green_taxi[green_taxi$Week==3,]$Average_Speed),mean(green_taxi[green_taxi$Week==4,]$Average_Speed))
par(mfrow=c(1,2))
barplot(meanWEekSpeeds,names.arg = c("First","Second","Third","Fourth"),col=c("steelblue","red","green","yellow"),ylab="Average week speeds",xlab="Weeks")

barplot(meanWEekSpeedsWithoutOutliers,names.arg = c("First","Second","Third","Fourth"),col=c("steelblue","red","green","yellow"),ylab="Without Outliers",xlab="Weeks")

dim(fWeek)
dim(sWeek)
dim(tWeek)
dim(forWeek)

#we can infer from the dimensions that for the first and last week the number of trips were less
#Hence the cab drivers , to seek more customers would have drove fast
#which is why there is a difference in the average speeds among weeks

hourlySpeed = list()
names <-c()
meanHourSpeed<-c()
for (a in c(1:24)) {
  hourlySpeed[a]<-list(green_taxi[green_taxi$Hours==a,]$Average_Speed)
  names <-c(names,a)
  meanValues <- (mean(green_taxi[green_taxi$Hours==a,]$Average_Speed))
  meanHourSpeed <-c(meanHourSpeed,meanValues)
}
hourlySpeedWithoutOutliers = list()
meanHourSpeedWithoutOutliers<-c()
for (a in c(1:24)) {
  hourSpeedWithoutOutliers <-green_taxi[green_taxi$Hours==a,]$Average_Speed[!green_taxi[green_taxi$Hours==a,]$Average_Speed %in% boxplot.stats(green_taxi[green_taxi$Hours==a,]$Average_Speed)$out]
  hourlySpeed[a]<-list(hourSpeedWithoutOutliers)
  meanValues <- mean(hourSpeedWithoutOutliers)
  meanHourSpeedWithoutOutliers <-c(meanHourSpeedWithoutOutliers,meanValues)
}

kruskal.test(hourlySpeedWithoutOutliers)
kruskal.test(hourlySpeed)

#Null Hypothesis : There is no significant difference in the average speeds among 24 hours
#Alternate Hypothesis : There is significant difference in the average speeds among 24 hours

#Since the p-value is less than .01 we reject the null hypothesis at 99% confidence interval

par(mfrow=c(2,1))

barplot(meanHourSpeed,names.arg = names,col=c("steelblue"),main="Average Hourly Speed ")
barplot(meanHourSpeedWithoutOutliers,names.arg = names,col=c("red"),main="Without Outliers")
#In both the cases the average speed is at peak at 5 o clock in the morning when the traffic would be very less
#Also it hits the lowest point when the time is at 6(18:00 hours) o clock in the evening when the traffic would be very high
#as it is a peak time
