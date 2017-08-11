# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The first step of the analysis is the read of the data. We can do it by first unzipping the file and reding the csv


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.1
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
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.1
```

```r
unzip(zipfile = "activity.zip")
data      <- read.csv("activity.csv")
variables <- names(data)
```

## What is mean total number of steps taken per day?
To understand the mean total number of steps taken per day we group the data by date, in order to get the sum of steps in all intervals. We can easily compute the mean and median of the value, to have them handy once we need to plot them. 

```r
by_day       <- data %>% group_by(date)
daily_steps  <- summarise(by_day,sum(steps,na.rm = TRUE))
daily_sum    <- as.matrix(daily_steps[,2])
daily_mean   <- mean(daily_sum)
daily_median <- median(daily_sum)

hist(daily_sum, breaks = 20)
abline(v=daily_mean, col = 2)   # red color
abline(v=daily_median, col = 4) # blue color
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## What is the average daily activity pattern?
By applying a similar procedure, but grouping by interval, we can obtain the average value of steps in each interval. 

```r
by_interval  <- data %>% group_by(interval)
interv_steps <- summarise(by_interval,mean(steps,na.rm = TRUE))
names(interv_steps)[2] <- "Mean steps"
plot(interv_steps,type = "l")

steps <- interv_steps[,2]
max_value <- which.max(as.matrix(steps))
points(interv_steps[max_value,], pch = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## Imputing missing values
We decide to input missing value. In order to do this, we first transform the steps values in a matrix, and then replace NA values with the average of the interval, across all days.
We can then show the histogram and compute the differences versus the previous case.

```r
data2          <- data
steps_row      <- data2[,1]
dim(steps_row) <- c(61, 288)
steps_mat_na  <- t(steps_row)

steps_mat <- steps_mat_na

for(i in 1:ncol(steps_mat)){
  steps_mat[is.na(steps_mat[,i]), i] <- mean(steps_mat[,i], na.rm = TRUE)
}

steps_mat_row      <- steps_mat
dim(steps_mat_row) <- c(1,61*288)
data2[,1]          <- t(steps_mat_row)

by_day            <- data2 %>% group_by(date)
daily_steps_nona  <- summarise(by_day,sum(steps,na.rm = TRUE))
daily_sum_nona    <- as.matrix(daily_steps_nona[,2])
daily_mean_nona   <- mean(daily_sum_nona)
daily_median_nona <- median(daily_sum_nona)

hist(daily_sum_nona, breaks = 20)
abline(v=daily_mean_nona, col = 2)   # red color
abline(v=daily_median_nona, col = 4) # blue color
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean_diff   <- daily_mean   - daily_mean_nona
median_diff <- daily_median - daily_median_nona
daily_diff  <- daily_sum    - daily_sum_nona
```


## Are there differences in activity patterns between weekdays and weekends?
Finally, we verify if there are differences of activty between weekdays and weekends.

```r
data2['weekday'] <- weekdays(as.Date(data2$date))
'%!in%' <- function(x,y)!('%in%'(x,y))

data2$daytype[data2$weekday  %in% c('sabato','domenica') ] <- "weekend"
data2$daytype[data2$weekday %!in% c('sabato','domenica')] <- "weekday"
steps_daytype <- aggregate(steps ~ interval + daytype, data = data2, mean)
names(steps_daytype)[3] <- "steps"

averages <- aggregate(steps ~ interval + daytype, data = data2, mean)
ggplot(steps_daytype, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
