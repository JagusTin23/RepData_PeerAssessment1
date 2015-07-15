# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(lubridate)
file <- "activity.csv"
data <- read.csv(file, header = TRUE, stringsAsFactor = FALSE)
data$date <- ymd(data$date)
```

## What is mean total number of steps taken per day?

```r
steps_day <- with(data, aggregate(steps, by = list(date), FUN=sum))    
hist(steps_day[,2], main = "Steps Taken Per Day", col = "turquoise", xlab = "steps/day")
```

![](PA1_template_files/figure-html/mean_median-1.png) 

```r
mean(steps_day[,2], na.rm = TRUE)  
```

```
## [1] 10766.19
```

```r
median(steps_day[,2], na.rm = TRUE) 
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
by_interval <- 
    data %>%
    select(interval, steps) %>%
    group_by(interval) %>%
    summarize(avgStepsInt = mean(steps, na.rm = TRUE))
maxActiveInt <- as.numeric(by_interval[by_interval$avgStepsInt == max(by_interval$avgStepsInt),1])
maxSteps <-  round(max(by_interval$avgStepsInt))
```

```r
plot(by_interval$interval, by_interval$avgStepsInt, type = "l", xlab = "5-minute interval", ylab = "steps")
```

![](PA1_template_files/figure-html/dailyplot-1.png) 

The 5 minute interval with the maximun number of steps is interval **835** with an average of **206** of steps taken.  

## Imputing missing values


```r
countNA <- length(which(is.na(data$steps)))
data_ver <- data[which(is.na(data$steps)),]
verif <- 
    data_ver %>%
    group_by(date) %>% 
    summarize(count = n(),
              sum_int = sum(interval))
```
There is a total of **2304** rows with missing values.  

After careful evaluation of the data, it was clear that there were full days in which values for the steps variable were not available. To verify this, a data set was created by subsetting the original dataset with the rows with missing values counting the number of rows and the sum of the interval. See the table below:  

```
##         date count sum_int
## 1 2012-10-01   288  339120
## 2 2012-10-08   288  339120
## 3 2012-11-01   288  339120
## 4 2012-11-04   288  339120
## 5 2012-11-09   288  339120
## 6 2012-11-10   288  339120
## 7 2012-11-14   288  339120
## 8 2012-11-30   288  339120
```
The number of rows with missing values in each day is 288 which corresponds to the number of interval recorded per day. This evaluation was performed to ensure there were no days with both recorded and missing values.  

This information was used to devise a strategy for inputing missing values. The strategy consist of inputing the missing values with the mean steps per interval calculated in the previous step. A new dataset was created by subsetting the original dataset by each date corresponding to missing values and inputting the the mean values per interval. 


```r
data2 <- read.csv(file, header = TRUE, stringsAsFactor = FALSE)
data2[which(data2$date == "2012-10-01"),1] <- by_interval$avgStepsInt
data2[which(data2$date == "2012-10-08"),1] <- by_interval$avgStepsInt
data2[which(data2$date == "2012-11-01"),1] <- by_interval$avgStepsInt
data2[which(data2$date == "2012-11-04"),1] <- by_interval$avgStepsInt
data2[which(data2$date == "2012-11-09"),1] <- by_interval$avgStepsInt
data2[which(data2$date == "2012-11-10"),1] <- by_interval$avgStepsInt
data2[which(data2$date == "2012-11-14"),1] <- by_interval$avgStepsInt
data2[which(data2$date == "2012-11-30"),1] <- by_interval$avgStepsInt
data2$date <- ymd(data2$date)
by_date2 <- 
    data2 %>%
    select(date, steps) %>%
    group_by(date) %>%
    summarize(stepsDay2 = sum(steps))
Avg_steps2 <- round(mean(by_date2$stepsDay2))
Median_steps2 <- median(by_date2$stepsDay2)
hist(by_date2$stepsDay2, main = "Steps Taken Per Day", col = "steelblue1", xlab = "steps/day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

As can be seen, the data has been normalized by imputting missing values with the mean steps taken per 5 minute interval. The new **mean** and **median** are equal with a value of **10766.19**.  

The data was not affected drastically by imputing the missing values with the mean interval values. The median shifted to the mean and the total number of steps taken daily slightly increased as can be seen from the histogram.  

## Are there differences in activity patterns between weekdays and weekends?


```r
weekend <- c("Saturday", "Sunday")
data2$wkday <- factor((weekdays(data2$date) %in% weekend), levels=c(FALSE, TRUE), labels=c('weekday', 'weekend'))

wkdata <- aggregate(steps~interval+wkday, data2, FUN=mean)
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.3
```

```r
xyplot(steps~interval|wkday, wkdata, layout=c(1,2), type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
dev.off()
```

```
## null device 
##           1
```


