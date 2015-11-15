---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
#### 1. Load the data (i.e. read.csv())


```r
activdata <- read.csv('activity.csv')
```
#### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activdata$date <- as.Date(activdata$date)
library(lubridate)
activdata$date <- ymd(activdata$date)
str(activdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
library(data.table)
activdata <- as.data.table(activdata)
```
# Data table with complete rows only

```r
compdata <- activdata[complete.cases(activdata$steps),]
```

## What is mean total number of steps taken per day?

```r
stepsday <- tapply(compdata$steps,compdata$date,sum)
```
#### 1. Make a histogram of the total number of steps taken each day

```r
hist(stepsday, breaks=10, main="The total number of steps per day", xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
#### 2. Calculate and report the mean and median total number of steps taken per day

```r
mean(stepsday)
```

```
## [1] 10766.19
```

```r
median(stepsday)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

####1. Make a time series plot

```r
avr <- tapply(compdata$steps, compdata$interval, mean)
pday<- data.frame(interval=names(avr), mean=avr, stringsAsFactors = FALSE)
dimnames(pday$mean) <- NULL
pday$interval <- as.numeric(pday$interval)
library(ggplot2)
```

```r
g <- ggplot(pday,aes(interval,mean))
g <- g + geom_line()
g <- g + ggtitle("Average Number of Steps during each Time Interval")
g <- g + xlab("5-Minute Time Interval") + ylab("Average N° of Steps")
g
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 
#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxst <- pday$interval[grepl(max(avr), pday$mean)]
maxst
```

```
## [1] 835
```

## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset

```r
nrow(activdata[!complete.cases(activdata$steps),])
```

```
## [1] 2304
```
#### 2. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
library(scales)
library(Hmisc)
filldata <- activdata
filldata$steps <- impute(activdata$steps, fun=mean)
```
#### 3. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day

```r
filldatastep <- tapply(filldata$steps, filldata$date, sum)
qplot(filldatastep, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

```r
totnumstepMean <- mean(filldatastep)
totnumstepMedian <- median(filldatastep)
```

## Are there differences in activity patterns between weekdays and weekends?
####1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```r
filldata$dateType <-  ifelse(as.POSIXlt(filldata$date)$wday %in% c(0,6), 'weekend', 'weekday')
```
#### 2. Make a panel plot containing a time series plot

```r
avrfilldata <- aggregate(steps ~ interval + dateType, data=filldata, mean)
```

```r
ggplot(avrfilldata, aes(interval, steps)) +
geom_line() + 
facet_grid(dateType ~ .) +
xlab("5-minute interval") +
ylab("avarage number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

