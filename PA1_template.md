# Reproducible Research: Peer Assessment 1

Data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up.
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


The variables included in this dataset are:
* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Loading and preprocessing the data


1. reading in the dataset and/or processing the data


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# 
# https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
#if(!file.exists("data")) {dir.create("data")}
# 
download.file(fileUrl, destfile = file.path(getwd(), "repdata%2Fdata%2Factivity.zip"))
# 
unzip(zipfile=file.path(getwd(), "repdata%2Fdata%2Factivity.zip"),exdir=getwd())
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)

activity$date <- as.Date(activity$date, "%Y-%m-%d")

names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

2. Histogram of the total number of steps taken each day


```r
# Filter out the NA's
ccActivity <- activity[(complete.cases(activity)),]   

# sum up all the steps per day
totalStepByDate <- aggregate(ccActivity$steps, list(date=ccActivity$date), sum)
names(totalStepByDate)[2] <- "totalSteps"   # rename column "x" to "totalSteps"


library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
ggplot(totalStepByDate, aes(x=totalSteps)) + 
      geom_histogram(binwidth = 1000) +
      ggtitle("Total Activity Steps per day from Oct. and Nov. 2012")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## 2. Calculate and report the **mean** and **median** total number of steps taken per day

```r
(MeanStepByDate <- mean(totalStepByDate$totalSteps))
```

```
## [1] 10766.19
```

```r
(MedianStepByDate <- median(totalStepByDate$totalSteps))
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged
across all days (y-axis)

```r
avgDaily5MinInterval <- aggregate(ccActivity$steps, list(interval=ccActivity$interval), mean)
names(avgDaily5MinInterval)[2] <- "intervalAvgSteps"   # rename column "x" to 

plot(avgDaily5MinInterval$intervalAvgSteps ~ avgDaily5MinInterval$interval, type="l", main ="Average # of Steps/ 5-minute interval Oct. to Nov. 2012", xlab = "Interval", ylab = "Avg # of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgDaily5MinInterval[(avgDaily5MinInterval$intervalAvgSteps > as.integer(max(avgDaily5MinInterval$intervalAvgSteps))),]
```

```
##     interval intervalAvgSteps
## 104      835         206.1698
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
NAcases <- is.na(activity$steps)
sum(NAcases)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Using the mean for that 5-minute interval
MeanStepByInterval <- aggregate(ccActivity$steps, list(interval=ccActivity$interval), mean)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# New dataset - join by common column name "interval"
activityImputed <- merge(activity, MeanStepByInterval, all.x = TRUE )   ## like left join

# New column: imputedSteps - fill in with avg value (column name "x") if original interval "steps" value was NA.
activityImputed <- dplyr::mutate(activityImputed, imputedSteps = ifelse(is.na(steps), x, steps))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
totalStepByDate <- aggregate(activityImputed$imputedSteps, list(date=activityImputed$date), sum)
names(totalStepByDate)[2] <- "totalSteps"   # rename column "x" to 


ggplot(totalStepByDate, aes(x=totalSteps)) + geom_histogram(binwidth = 1000) +
      labs(title="Imputed Value Total # of Steps per day, Oct.-Nov. 2012")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Do these values differ from the estimates from the first part of the assignment? What is the impact of # imputing missing data on the estimates of the total daily number of steps?


```r
MeanImputedStepByDate <- aggregate(activityImputed$imputedSteps, list(date=activityImputed$date), mean)
names(MeanImputedStepByDate)[2] <- "AvgSteps"   # rename column "x" to 

MeanStepByDate <- aggregate(ccActivity$steps, list(date=ccActivity$date), mean)
names(MeanStepByDate)[2] <- "AvgSteps"   # rename column "x" to 

MeanImputedStepByDate$source <- c("Imputed Value")
#MeanImputedStepByDate

MeanStepByDate$source <- c("NAs removed")

#MeanStepByDate

MeanStepsCompare <- rbind(MeanStepByDate, MeanImputedStepByDate )

#MeanStepsCompare

ggplot(MeanStepsCompare, aes(x=date, y=AvgSteps)) +
      geom_line() +
      facet_wrap(~source, nrow=2) +
      ggtitle("Compare imputed interval Avg Steps by Date - Oct. to Nov. 2012")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


The result is not much difference except for the first and last date where there were no data. 

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -"weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activityImputed$weekCategory <- factor(ifelse(weekdays(activityImputed$date) %in% c("Saturday", "Sunday"), c("weekend"),c("weekday")))
```

2. Make a panel plot containing a time series plot (i.e. type="1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
MeanStepByIntervalByWeekCat <- aggregate(activityImputed$imputedSteps, by=list(WeekCat = activityImputed$weekCategory, interval = activityImputed$interval), mean)

names(MeanStepByIntervalByWeekCat)[3] <- c("intervalAvgSteps")

ggplot(MeanStepByIntervalByWeekCat, aes(x=interval, y=intervalAvgSteps)) +
      geom_line() +
      facet_wrap(~WeekCat, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->





