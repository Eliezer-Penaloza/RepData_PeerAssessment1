# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("C:/Users/usuario/Documents/Tarea Reprocible research/activity.csv", header = TRUE)
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.3
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
library(lattice)
library(grDevices)
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day.

```r
steps.day <- data %>% group_by(date) %>% summarise(total = sum(steps, na.rm= TRUE))
steps.day$date <- as.Date(steps.day$date)
```

### If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
steps.day$date <- as.Date(steps.day$date)
png(filename = "figure1.png", height = 480, width = 480)
with(steps.day, plot(date,total, type= "h", lwd = 4, col = 'red', xlab = 'Date', ylab = 'Total Steps', main = "Number of Steps Taken Each Day"))
dev.off()
```

```
## png 
##   2
```

```r
with(steps.day, plot(date,total, type= "h", lwd = 4, col = 'red', xlab = 'Date', ylab = 'Total Steps', main = "Number of Steps Taken Each Day"))
```

![](Peer_Assignament_1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Calculate and report the mean and median of the total number of steps taken per day


```r
mean.steps.day <- sum(steps.day$total)/61
median.steps.day <- as.numeric(median(steps.day$total))

print(paste('The Mean is: ', mean.steps.day))
```

```
## [1] "The Mean is:  9354.22950819672"
```

```r
print(paste('The Median is: ',median.steps.day))
```

```
## [1] "The Median is:  10395"
```

## What is the average daily activity pattern?.

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
steps.mean.day <- data %>% group_by(interval) %>% summarise(total = mean(steps, na.rm= TRUE))
png(filename = "figure2.png", height = 480, width = 480)
with(steps.mean.day, plot(interval,total, type = 'l', col = "pink", xlab = 'Interval', ylab = 'Average', main = 'Average Number of Steps Taken Across all Days'))
dev.off()
```

```
## png 
##   2
```

```r
with(steps.mean.day, plot(interval,total, type = 'l', col = "pink", xlab = 'Interval', ylab = 'Average', main = 'Average Number of Steps Taken Across all Days'))
```

![](Peer_Assignament_1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? Imputing missing values



```r
data2 <- na.omit(steps.mean.day)
m <- steps.mean.day[steps.mean.day$total == max(steps.mean.day$total, na.rm = TRUE),]

print(paste('The maximum value of number of steps on average is: ', m[1,2], ', and the interval that contains this value is: ', m[1,1]))
```

```
## [1] "The maximum value of number of steps on average is:  206.169811320755 , and the interval that contains this value is:  835"
```

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missing <- nrow(data[ is.na(data$steps), ])
print(paste('The number of missing values is: ', missing))
```

```
## [1] "The number of missing values is:  2304"
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
fill <- function(x,y){
  for(i in 1:nrow(x)){
   if(is.na(x[i,1]) == TRUE){
     for(j in 1:nrow(y)){
       if(x[i,3] == y[j,1])
         x[i,1] <- y[j,2]
     }
   }
  }
  x
}
```


### reate a new dataset that is equal to the original dataset but with the missing data filled in.



```r
data2 <- fill(data, steps.mean.day)
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps.day2 <- data2 %>% group_by(date) %>% summarise(total = sum(steps, na.rm= TRUE))
steps.day2$date <- as.Date(steps.day2$date)
png(filename = "figure3.png", height = 480, width = 480)
with(steps.day2, plot(date,total, type= "h", lwd = 4, col = 'green', xlab = 'Date', ylab = 'Total Steps', main = 'The Total Number of Steps Taken Each Day'))
dev.off()
```

```
## png 
##   2
```

```r
with(steps.day2, plot(date,total, type= "h", lwd = 4, col = 'green', xlab = 'Date', ylab = 'Total Steps', main = 'The Total Number of Steps Taken Each Day'))
```

![](Peer_Assignament_1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
mean.steps.day2 <- sum(steps.day2$total)/61
median.steps.day2 <- as.numeric(median(steps.day2$total))
print(paste('The Mean is: ', mean.steps.day2))
```

```
## [1] "The Mean is:  10766.1886792453"
```

```r
print(paste('The Median is: ', median.steps.day2))
```

```
## [1] "The Median is:  10766.1886792453"
```

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data$weekday <- weekdays(data$date)

worwend <- function(x){
  for(i in 1:nrow(x)){
    if(x[i,4] == 'sábado' | x[i,4] == 'domingo'){
      x[i,4] <- "weekend"
      
    }else{
      x[i,4] <- "weekday"
    }
  }
  x
}

data <- worwend(data)
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using 
simulated data.


```r
data3 <- data %>% group_by(weekday,interval) %>% summarise(average = mean(steps, na.rm = TRUE))
png(filename = "figure4.png", height = 480, width = 480)
xyplot(average ~ interval | weekday, data = data3, type = 'l', layout = c(1,2), ylab = 'Average of Number of Steps', xlab = 'Interval')
dev.off()
```

```
## png 
##   2
```

```r
xyplot(average ~ interval | weekday, data = data3, type = 'l', layout = c(1,2), ylab = 'Average of Number of Steps', xlab = 'Interval')
```

![](Peer_Assignament_1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
