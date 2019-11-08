---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
* Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
* Process/transform the data (if necessary) into a format suitable for your analysis


```r
  library(dplyr)
  library(knitr)
  library(ggplot2)
  if (!"activity.zip" %in% dir()) {
    link <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(link, "activity.zip")
  }
  if (!"activity.csv" %in% dir()) {
    unzip("activity.zip")
  }
  activity_data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
* Calculate the total number of steps taken per day


```r
total_steps_per_day <- activity_data %>% group_by(date) %>% summarize(total = sum(steps, na.rm = TRUE))
```

* If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(total_steps_per_day$total, main = "Total steps per day", xlab = "steps", col="darkmagenta", density = 25, border = "black", angle = 135, breaks = 20)
```

![](figure/unnamed-chunk-3-1.png)<!-- -->
* Calculate and report the mean and median of the total number of steps taken per day


```r
mean_n_median <- total_steps_per_day %>% summarize(mean = mean(total, na.rm = TRUE), median = median(total, na.rm = TRUE))
kable(mean_n_median, caption = "Mean and median of total steps")
```



Table: Mean and median of total steps

    mean   median
--------  -------
 9354.23    10395

## What is the average daily activity pattern?
* Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
time_series <- activity_data %>% group_by(interval) %>% summarize(mean = mean(steps, na.rm = TRUE))
ggplot(time_series, aes(x = interval , y = mean)) + geom_line(color="red", size = 1) + labs(title = "Average daily activity", x = "Interval", y = "Average steps per day")
```

![](figure/unnamed-chunk-5-1.png)<!-- -->

```r
max_mean_interval <- time_series[time_series$mean == max(time_series$mean),]$interval
```
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? *835*


## Imputing missing values
* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with **NA**s)


```r
totalNAs <- activity_data %>% filter(is.na(steps)) %>% nrow
```
The total number of **NA**s is: *2304*

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
filledActivityData <- activity_data %>% left_join(time_series, by = "interval") %>% mutate(steps = ifelse(is.na(steps), mean, steps)) %>% select(c("steps", "date", "interval"))
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
write.csv(filledActivityData, "filled_activity_data.csv")
```

* Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_steps_per_day_filled <- filledActivityData %>% group_by(date) %>% summarize(total = sum(steps, na.rm = TRUE))
hist(total_steps_per_day_filled$total, main = "Total steps per day", xlab = "steps", col="darkmagenta", density = 25, border = "black", angle = 45, breaks = 20)
```

![](figure/unnamed-chunk-9-1.png)<!-- -->

```r
mean_n_median_filled <- total_steps_per_day_filled %>% summarize(mean = mean(total, na.rm = TRUE), median = median(total, na.rm = TRUE))
kable(mean_n_median_filled, caption = "Mean and median of total steps")
```



Table: Mean and median of total steps

     mean     median
---------  ---------
 10766.19   10766.19

```r
time_series_filled <- filledActivityData %>% group_by(interval) %>% summarize(mean = mean(steps, na.rm = TRUE))
ggplot(time_series_filled, aes(x = interval , y = mean)) + geom_line(color="red", size = 1) + labs(title = "Average daily activity", x = "Interval", y = "Average steps per day")
```

![](figure/unnamed-chunk-9-2.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
filledActivityData <- mutate(filledActivityData, day_type = ifelse(weekdays(as.Date(date)) %in% c("sábado", "domingo"), "weekend", "weekday"))
```

* Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
time_series_filled_day_type <- filledActivityData %>% group_by(interval, day_type) %>% summarize(mean = mean(steps, na.rm = TRUE))
ggplot(time_series_filled_day_type, aes(x = interval , y = mean)) + geom_line(color="red", size = 1) + labs(x = "Interval", y = "Number of steps") + facet_wrap(~ day_type, ncol = 1, nrow=2)
```

![](figure/unnamed-chunk-11-1.png)<!-- -->
