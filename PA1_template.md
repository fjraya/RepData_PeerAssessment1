---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

First, I check if file has unzipped. Then I read the data.


```r
if  (!file.exists("activity.csv")) {
    unzip("activity.zip")
}

data <- read.csv("activity.csv", colClasses=c("numeric", "Date", "numeric"))

head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?


### Remove na's

```r
data_without_na <- data[!is.na(data$steps), ]
```

### Calculate the total number of steps taken per day


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
total_steps_per_day <- data_without_na %>% group_by(date) %>% summarise(steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(total_steps_per_day)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <date>     <dbl>
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
### If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
library(ggplot2)
g <- ggplot(data=total_steps_per_day, aes(x=steps)) + geom_histogram(alpha = .5) + labs( title = "Total number of steps taken each day", x = "steps" , y = "frequency") + theme_bw()
g
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
### Calculate and report the mean and median of the total number of steps taken per day


```r
summary(total_steps_per_day)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

Median is 10765 and mean is 10766

## What is the average daily activity pattern?

### Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
average_interval_steps <- data_without_na %>% group_by(interval) %>% summarise(steps = mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(average_interval_steps)
```

```
## # A tibble: 6 x 2
##   interval  steps
##      <dbl>  <dbl>
## 1        0 1.72  
## 2        5 0.340 
## 3       10 0.132 
## 4       15 0.151 
## 5       20 0.0755
## 6       25 2.09
```


```r
g <- ggplot(average_interval_steps, aes(interval, steps)) + geom_line() + theme_bw() + labs( title = "Average number of steps")
g
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_steps<- which.max(average_interval_steps$steps)
max_interval <- average_interval_steps[max_steps, 1]
max_interval
```

```
## # A tibble: 1 x 1
##   interval
##      <dbl>
## 1      835
```

Interval with maximum number of steps is ``835``



## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
summary(data)
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

There are 2304 missing values

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I fill NA values with the mean of steps of the same interval.



```r
data_with_mean_na <- data
for (i in 1:nrow(data_with_mean_na)){
    if (is.na(data_with_mean_na$steps[i])){
        data_with_mean_na$steps[i] <- mean(data_with_mean_na$steps[data_with_mean_na$interval == data_with_mean_na$interval[i]], na.rm=TRUE)
    }
}

head(data_with_mean_na)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```


### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_number_steps_by_day <- data_with_mean_na %>% group_by(date) %>% summarise(steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
g <- ggplot(data=total_number_steps_by_day, aes(x=steps)) + geom_histogram(alpha = .5) + labs( title = "Total number of steps taken each day", x = "steps" , y = "frequency") + theme_bw()
g
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


```r
summary(total_number_steps_by_day)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

I have similars median and mean. I fill NA values with the mean for interval. Perhaps for this reason, the values are very close to previously estimated without NA's


## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data_with_mean_na$type_day <- as.factor((weekdays(data_with_mean_na$date) == "sábado" | weekdays(data_with_mean_na$date) == "domingo"))

levels(data_with_mean_na$type_day)<- c("weekday", "weekend")

head(data_with_mean_na)
```

```
##       steps       date interval type_day
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```

### Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(dplyr)
grouped_by_interval_day <- data_with_mean_na %>% group_by(interval, type_day) %>% summarise(steps = mean(steps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
head(grouped_by_interval_day)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval type_day  steps
##      <dbl> <fct>     <dbl>
## 1        0 weekday  2.25  
## 2        0 weekend  0.215 
## 3        5 weekday  0.445 
## 4        5 weekend  0.0425
## 5       10 weekday  0.173 
## 6       10 weekend  0.0165
```

```r
g <- ggplot(grouped_by_interval_day, aes(interval, steps)) + geom_line(col="blue") + facet_wrap(.~type_day, ncol=1) + labs( y = "Number of steps" ) + theme(strip.background = element_rect(colour="red", fill="#CCCCFF"))
g
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
