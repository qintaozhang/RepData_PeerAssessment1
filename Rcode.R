#1. load the data
data <-read.csv("activity.csv")
#remove NA values
data$date<-as.Date(data$date)

#What is mean total number of steps taken per day?
sumStepsDays<-with(data,tapply(steps,date,sum))
hist(sumStepsDays)
mean(sumStepsDays)
median(sumStepsDays)

## What is the average daily activity pattern?
averageStepsDays<-with(data,tapply(steps,interval,mean,na.rm = TRUE))
intervals<-unique(data$interval)
plot(intervals,averageStepsDays,type = "l")
title("average daily activity pattern")
averageStepsDays<-data.frame(averageStepsDays)
colnames(averageStepsDays)<-"Average"
maxSteps<-which.max(averageStepsDays$Average)
averageStepsDays[maxSteps,]

#modify missing values: stratergy is: use the mean/median for  that 5-minute interval, or the mean for that 5-minute interval, etc
#calcuate mean of everyday

Meansteps1Days<-with(data,tapply(steps,date,mean,na.rm = TRUE))
FixMissing <- function(x) {
  xlabel<-as.character(as.numeric(x["interval"]))
  if(is.na(x["steps"])) {
    return(as.numeric(averageStepsDays[xlabel,"Average"])) }
  else {
    return(x["steps"])
  }
}

FixMissing2 <- function(x) {
  xlabel<-as.character(as.numeric(x["interval"]))
  print(x["NA_marker"])
  if(x["NA_marker"]==" TRUE") {
   
    return(as.numeric(averageStepsDays[xlabel,"Average"])) }
  else {
    return(x["steps"])
  }
}
newData<-data
newData["steps"]<-apply(newData,1,FixMissing2)
newData$steps<-as.numeric(newData$steps)
sumStepsDaysNew<-with(newData,tapply(steps,date,sum))
hist(sumStepsDaysNew)
#data2<-apply(data,1,function(x) if(is.na(x["steps"])) print(averageStepsDays[as.character(x["interval"]),"Average"]))
#apply(data,1,function(x) if(is.na(x["steps"])) print("found"))

newData["Weekdays"]<-ifelse(weekdays(newData$date)=="Saturday" |weekdays(newData$date)=="Sunday","Weekend","weekdays")
#calculate weekday average

newData<-data.frame(newData)
weekMean<-aggregate(x=newData$steps,by=list(interval=newData$interval, Weekdays=newData$Weekdays), FUN=mean)
library(ggplot2)
ggplot(weekMean, aes(interval,x))+ geom_line()+facet_grid(Weekdays~.)+ xlab("5 minute interval") + ylab("average number of steps")

library(lattice)
weekMean <- aggregate(steps ~ interval + Weekdays, newData, mean)
xyplot(steps ~ interval | Weekdays, weekMean, type = "l", layout 
       = c(1, 2), xlab = "5min Interval", ylab = "Averaged Number of steps")
