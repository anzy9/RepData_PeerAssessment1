# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
#This will setup the directory for the assignent, It will create directory, download the files from the web and will unzip the files to be use for the analysis
setwd("F:/Shared/Drive/AnjaliS/Coursera/RepData/Week2")
mainDir<-getwd()
subDir<-"Course5Assignment1"
if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
  
}

#download the file and unzip into created folder

desc<-"Activity_Monitoring_Data.zip"
mDir<-paste(getwd(),"/",desc,sep = "")
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
desc<-"Activity_Monitoring_Data.zip"
if (!file.exists(mDir)){
  download.file(url, dest=desc, mode="wb") 
}
unzip (desc, exdir=getwd())

#read the data from the zip file
#household<-read.table("household_power_consumption.txt", header=TRUE, na.strings="?", sep=";")
activityData<-read.csv(file="activity.csv",header = TRUE,na.strings = "NA",sep = ",")
```


## What is mean total number of steps taken per day?
### 1 Calculate the total number of steps taken per dataset

```r
totalStepperday<-aggregate(activityData$steps,by=list(activityData$date),sum,na.rm=TRUE)
names(totalStepperday)<-c("Day","Steps")
```
### 2 Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
qplot(totalStepperday$Steps, xlab='Total steps per day', ylab='Frequency')
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


### 3 Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalStepperday[,2])
```

```
## [1] 9354.23
```

```r
median(totalStepperday[,2])
```

```
## [1] 10395
```
## What is the average daily activity pattern?
### 1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgsStepsperday<-aggregate(activityData$steps,by=list(activityData$interval),mean,na.rm=TRUE)
names(avgsStepsperday)<-c("Interval","Steps")
g<-ggplot(avgsStepsperday,aes(Interval,Steps))
g<-g+geom_line(stat = "identity")+ labs(y="Average Number of steps ",x=" 5-minute interval")
#beautifying the plot
g<-g+theme_bw() +ggtitle("average daily activity pattern")+theme(
  plot.title = element_text(color="red", size=12, face="bold.italic"),
  axis.title.x = element_text(color="#993333", size=10, face="bold"),
  axis.title.y = element_text(color="#993333", size=10, face="bold")
)
g
```

![](PA1_template2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgsStepsperday[which.max(avgsStepsperday$Steps),]
```

```
##     Interval    Steps
## 104      835 206.1698
```
## Imputing missing values
### 1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activityData))
```

```
## [1] 2304
```
### 2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc and  Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
require(Hmisc)
ImputedData<-activityData
ImputedData$steps<-impute(ImputedData$steps,fun=mean)
```

### 3 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sumofStepsperday<-aggregate(ImputedData$steps,by=list(ImputedData$date),sum)
names(sumofStepsperday)<-c("Day","Steps")
qplot(sumofStepsperday$Steps, xlab='Total steps per day', ylab='Frequency')
```

![](PA1_template2_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(sumofStepsperday[,2])
```

```
## [1] 10766.19
```

```r
median(sumofStepsperday[,2])
```

```
## [1] 10766.19
```
#### The mean and the median are same after imputing missing values with the mean value 

## Are there differences in activity patterns between weekdays and weekends?

### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.


```r
ImputedData$daytype<-as.factor(as.POSIXlt(ImputedData$date)$wday)
day<-gsub("0","Weekend",ImputedData$daytype)
day<-gsub("1","Weekday",day)
day<-gsub("2","Weekday",day)
day<-gsub("3","Weekday",day)
day<-gsub("4","Weekday",day)
day<-gsub("5","Weekday",day)
day<-gsub("6","Weekend",day)
ImputedData$daytype<-day

daytypeData<-aggregate(ImputedData$steps,by=list(ImputedData$interval,ImputedData$daytype),mean)
names(daytypeData)<-c("Interval","DayType","Steps")
```

### 1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
daytypeData<-aggregate(ImputedData$steps,by=list(ImputedData$interval,ImputedData$daytype),mean)
names(daytypeData)<-c("Interval","DayType","Steps")
```
### 2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
xyplot(Steps ~ Interval | DayType, daytypeData, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template2_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
