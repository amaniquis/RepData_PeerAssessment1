---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity_data <- read.csv("activity.csv", header = T, 
                          colClasses = c("integer", "character", "integer"))
Date <- strptime(activity_data$date,"%Y-%m-%d")
activity_data <- cbind(activity_data, Date)
```


## What is mean total number of steps taken per day?

```r
total_steps_per_day <- aggregate(steps ~ Date, data = activity_data, sum)
hist(total_steps_per_day$steps, xlab = "Total Steps per Day",
     main = "Histogram of Total Steps per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean_steps_per_day <- mean(total_steps_per_day$steps)
median_steps_per_day <- median(total_steps_per_day$steps)
```
Mean total number of steps taken per day: 1.0766189 &times; 10<sup>4</sup>  
Median total number of steps taken per day: 10765

## What is the average daily activity pattern?

```r
mean_steps_per_interval <- aggregate(steps ~ interval, data = activity_data,
                                     mean)
colnames(mean_steps_per_interval) <- c("interval", "mean_steps")
with(mean_steps_per_interval, plot(interval, mean_steps, type="l", 
                                   ylab = "Mean Number of Steps", 
                                   xlab = "5-Minute Time Interval"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
max_steps_interval <-
    mean_steps_per_interval[which.max(mean_steps_per_interval$mean_steps),1]
```
5-Minute Time Interval with Most Steps: 835

## Imputing missing values

```r
num_na_rows <- nrow(activity_data) - sum(complete.cases(activity_data))
```
Number of rows with NA values: 2304  

```r
new_activity_data <- merge(x = activity_data, y = mean_steps_per_interval,
                           by = "interval")
new_activity_data$steps[is.na(new_activity_data$steps)] <-
    new_activity_data$mean_steps[is.na(new_activity_data$steps)]

new_total_steps_per_day <- aggregate(steps ~ Date, data = new_activity_data,
                                     sum)
hist(new_total_steps_per_day$steps,
     xlab = "Total Steps per Day",
     main = "Histogram of Total Steps per Day (with Imputed Missing Values)")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
new_mean_steps_per_day <- mean(new_total_steps_per_day$steps)
new_median_steps_per_day <- median(new_total_steps_per_day$steps)
```
Mean total number of steps taken per day (with imputed missing values): 
1.0766189 &times; 10<sup>4</sup>  
Median total number of steps taken per day (with imputed missing values):
1.0766189 &times; 10<sup>4</sup>  


It would appear that imputing the missing step values with the mean of the 
corresponding 5-minute interval does not change the mean in any way. However, 
the median is no longer 10765, but now 10766.19 which is to be expected. In the 
original data set, there are a few days that have no step values recorded at 
all. When all the step values for these days are replaced with their 
corresponding intervals, their total will add up to the mean of 10766.19 which 
now also becomes the new median since it is also the center of the data.

## Are there differences in activity patterns between weekdays and weekends?

```r
day_names <- weekdays(new_activity_data$Date)
day_names[day_names %in% c("Sunday", "Saturday")] <- "weekend"
day_names[day_names != "weekend"] <- "weekday"
day_type <- day_names
new_activity_data <- cbind(new_activity_data, day_type)

library(lattice)
xyplot(steps ~ interval | day_type, data = new_activity_data,
       ylab = "Number of steps", xlab = "Interval", 
       layout=c(1,2), type = "l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

It appears that weekends have less total steps per 5-minute time interval
compared to weekdays and also that the activity window is shifted later by about
three hours. Each "column" in the graphs represents an hour and each spike
within the column represents a 5-minute interval.
