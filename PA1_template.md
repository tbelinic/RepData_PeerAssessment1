# Reproducible Research: Peer Assessment 1

#Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data.

```r
data<- read.csv("activity.csv", header = TRUE) 
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
data$date <- as.Date(as.character(data$date), "%Y-%m-%d") 
```

##What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day. Make a histogram of the total number of steps taken each day.

```r
total_steps <- aggregate(steps ~ date, data, sum)
hist(total_steps$steps, 
     breaks = 10, 
     xlab = "Total number of steps per day",
     ylab = "Count",
     main = "Histogram of total steps per day", 
     col = "darkgreen")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

2. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(total_steps$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?
1. Make a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
average_steps <- aggregate(steps ~ interval, data, mean)
plot(average_steps$interval,
     average_steps$steps, 
     xlab = "Time inteval (5-min step)",
     ylab = "Step count",
     main= " Average number of steps taken throughout the day",
     type = "l", 
     col="darkgreen")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average_steps$interval[which.max(average_steps$steps)]
```

```
## [1] 835
```

##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as ????????). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)

```r
sum(is.na(data))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
for (i in which(sapply(data$steps, is.na))){
    data$steps[i] <- mean(data$steps[data$interval==data$interval[i]], na.rm = TRUE)
}
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total_steps_fill <- aggregate(steps ~ date, data, sum)
hist(total_steps_fill$steps, 
     breaks = 50, 
     xlab = "Total number of steps per day",
     ylab = "Count",
     main = "Histogram of total steps per day with replaced missing values", 
     col = "darkgreen")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(total_steps_fill$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_fill$steps)
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?
Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels weekday and weekend, indicating whether a given date is a weekday or weekend day.

```r
data$type <-  ifelse(as.POSIXlt(data$date)$wday %in% c(0,6), 'weekend', 'weekday')
average_steps_fill<- aggregate(steps ~ interval + type, data, mean)
```
2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(ggplot2)
ggplot() + 
    geom_line(data=average_steps_fill, aes(x=interval, y=steps, col=type)) + 
    facet_grid(type ~ .) +
    labs(x="Time inteval (5-min step)", y="Step count") + 
    theme(legend.position="none") 
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
