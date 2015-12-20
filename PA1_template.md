---
title: "Assignment_1"
author: "Juan Carpio"
date: "Friday, December 18, 2015"
output: html_document
---

### Opening relevant libraries

```r
# Opening relevant libraries
library("plyr")
library("dplyr")
library("ggplot2")
library("Hmisc")
#library("gridExtra")
```


## Loading and preprocessing the data

```r
#Setting working directory
setwd("C:/Users/K56CA/Dropbox/Big Data/COURSERA/Reproducible_Research")

#Getting data
data<-read.csv("activity.csv")

#Transforming Data into a suitable format (Steps per day)
clean_data<- data[complete.cases(data),]
steps_day<- clean_data %>% group_by(date) %>% summarise_each(funs(sum))
```

##What is mean total number of steps taken per day?

```r
#Histogram of the total number of steps taken each day

g1<-ggplot(data=steps_day, aes(steps))+ 
        geom_histogram(breaks= seq(0, 20000, by = 1000), col="black") +
        aes(fill=..count..) +
        scale_fill_gradient("count", low = "darkgreen", high = "red") +
        labs(title="Histogram: Steps per day", x="Steps", y="Count") +
        scale_y_continuous(breaks=seq(0,10, by=1)) +
        scale_x_continuous(breaks=seq(0,20000, by=4000))
g1
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
#Mean and median of the total number of steps taken per day
trend<-cbind(mean(clean_data$steps),median(clean_data$steps))
colnames(trend)<-c("Steps_Mean","Steps_Median")
trend
```

```
##      Steps_Mean Steps_Median
## [1,]    37.3826            0
```


##What is the average daily activity pattern?

```r
#Time serie 5-minute interval (x-axis) and the average number of steps taken

steps_interval<- clean_data %>% group_by(interval) %>% 
        summarise_each(funs(mean))

ggplot(data=steps_interval, aes(interval, steps)) + 
        geom_line(col="blue")+
        labs(title="Time Serie: Steps per 5-Minute Interval", 
             ylab="Steps", y="5-Minutes Interval") 
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
##5-minute interval, containing the MAXIMUM number of steps
top<-top_n(steps_interval, 1, steps) %>% select(interval, steps)
top[1,1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```


## Imputing missing values

```r
##Number of rows with NAS
sum(is.na(data$steps)) #NA's variable "steps"
```

```
## [1] 2304
```

```r
sum(is.na(data$interval)) #NA's variable "interval"
```

```
## [1] 0
```

```r
sum(is.na(data$date)) #NA's variable "date"
```

```
## [1] 0
```

```r
## Strategy for filling in all of the missing values in the dataset
## (The mean of steps for each interval is used to fill NA values)

clean_data_2<-ddply(data,"interval", mutate, 
                imputed.value = impute(steps, mean))

##New data set including NA values filled in. 
new_steps_day<- clean_data_2 %>% group_by(date) %>% summarise_each(funs(sum))

##New histogram of the total number of steps taken each day 

g2<-ggplot(data=new_steps_day, aes(imputed.value))+ 
        geom_histogram(breaks= seq(0, 20000, by = 1000), col="black") +
        aes(fill=..count..) +
        scale_fill_gradient("count", low = "darkgreen", high = "red") +
        labs(title="Histogram: Steps per day (including NA's filled values)", x="Steps", y="Count") +
        scale_y_continuous(breaks=seq(0,20, by=1)) +
        scale_x_continuous(breaks=seq(0,20000, by=4000))

g2
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
#Mean and median of the total number of steps taken per day
#(After filling in NA's values)
trend_2<-cbind(mean(clean_data_2$imputed.value),
               median(clean_data_2$imputed.value))
colnames(trend_2)<-c("Steps_Mean","Steps_Median")
trend_2
```

```
##      Steps_Mean Steps_Median
## [1,]    37.3826            0
```
## Are there differences in activity patterns between weekdays and weekends?


```r
## Create a new factor variable in the dataset with two levels - 
## "weekday" and "weekend"

w_data<-weekdays((as.Date(clean_data_2$date)))
week<-ifelse (w_data=="Saturday" | w_data=="Sunday", "Weekend","Weekday") 
new_data<-cbind(clean_data_2,week)

## Panel plot containing a time series plot of the 5-minute interval, 
## averaged across all weekday days or weekend days 

new_steps_interval<- new_data %>% group_by(interval, week) %>% 
        summarise_each(funs(mean))

ggplot(data=new_steps_interval, aes(interval, imputed.value)) + 
        geom_line(col="blue")+
        labs(title="Time Serie: Steps per 5-Minute Interval", 
             ylab="Steps", y="5-Minutes Interval") +
        facet_grid(week~ .)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
