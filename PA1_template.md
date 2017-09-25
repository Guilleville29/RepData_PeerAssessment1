Reproducible Research Assignment 1
==================================

##Guillermo S Romero P
##Sept 24, 2017

###1.Loading and preprocessing the data "Activiting monitoring data"

```r
#Load the data
actividad <- read.csv("~/DATA SCIENCE/CURSO5 Reproducible Research/Semana2/activity/activity.csv")
#Preview of data
head(actividad)
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
Resume of data

```r
str(actividad)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Process the data into a format suitable for the analysis

```r
actividad$date <- as.Date(actividad$date, format = "%Y-%m-%d")
```

###2. What is mean total number of steps taken per day?
* Calculate the total number of steps taken per day

```r
total_pasos <- aggregate(steps ~ date, data = actividad, sum, na.rm = TRUE)
head(total_pasos)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
* View the histogram of steps taken per day

```r
par(mfrow = c(1, 1))
#Use base plotting system
hist(total_pasos$steps, breaks = 20,
     main = "Total number of steps taken each day",
     col = "blue", border = "white", xlab = "Step", axes = FALSE)
axis(1)
axis(2, las = 1)
```

![plot of chunk unnamed-chunk-78](figure/unnamed-chunk-78-1.png)
* Calculate and report the mean and median total number of steps taken per day

```r
mean(total_pasos$steps)
```

```
## [1] 10766.19
```

```r
median(total_pasos$steps)
```

```
## [1] 10765
```

###What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
  the average number of steps taken, averaged across all days (y-axis)

```r
prom_pasos <- aggregate(steps ~ interval, data = actividad, mean, na.rm = TRUE)

plot(prom_pasos$interval, prom_pasos$steps, type = "l", lwd = 2, col = "navy",
     main = "Time Series: average number of steps taken",
     xlab = "5-minute interval", ylab = "Average number of steps")
axis(1)
```

![plot of chunk unnamed-chunk-80](figure/unnamed-chunk-80-1.png)

* Which 5-minute interval, on average across all the days in the dataset, 
  contains the maximum number of steps?

```r
prom_pasos$interval[which.max(prom_pasos$steps)]
```

```
## [1] 835
```
The 835-th 5-minute interval contains the maximum number of steps

### Imputing missing values

* Calculate and report the total number of missing values in the dataset 
(i.e. the total number of rows with NAs)

```r
sum(is.na(actividad))
```

```
## [1] 2304
```
There are 2304 missing values in the dataset

* Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could use 
the mean/median for that day, or the mean for that 5-minute interval, etc.

Here I use the mean of 5-minute interval to fill in the values of the missing values

* Create a new dataset that is equal to the original dataset but with the missing
data filled in

```r
imputar <- actividad #new dataset "imputar"
for(i in prom_pasos$interval){
  imputar[imputar$interval == i & is.na(imputar$steps), ]$steps <- prom_pasos$steps[
          prom_pasos$interval == i]
}

head(imputar) # no NAs
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
Verify the imputing missing values in data

```r
sum(is.na(imputar)) # result 0
```

```
## [1] 0
```

* Make a histogram of the total number of steps taken each day and Calculate and
report the mean and median total number of steps taken per day. Do these values 
differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily 
number of steps?

```r
total_paso_imput <- aggregate(steps ~ date, data = imputar, sum, na.rm = TRUE)

hist(total_paso_imput$steps, breaks = 20,
     main = "Total number of steps taken each day (imputed)",
     col = "purple1", border = "white", xlab = "Step", axes = FALSE)
axis(1)
axis(2, las = 1)
```

![plot of chunk unnamed-chunk-85](figure/unnamed-chunk-85-1.png)


```r
mean(total_paso_imput$steps)
```

```
## [1] 10766.19
```

```r
median(total_paso_imput$steps)
```

```
## [1] 10766.19
```
The main is the same as the mean from the first part of the assignment,
but the median is not, although their voalues are close.
Since many data points have the same values as the mean, the median is 
much likely to be the same as the mean as well.


###Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and 
"weekend" indicating whether a given date is a weekday or weekend day.

```r
imputar$date <- as.Date(imputar$date, format = "%Y-%m-%d")
imputar$dia <- weekdays(imputar$date)
imputar$semana <- ""
imputar$semana <- ifelse(imputar$dia == "sábado" | imputar$dia == "domingo",
                         "weekend", "weekday")
imputar$semana <- factor(imputar$semana)
```
* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```r
prom_paso_imput <- aggregate(steps ~ interval + semana, data = imputar, mean)
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.3.3
```

```r
xyplot(steps ~ interval | semana, data = prom_paso_imput, type = "l", lwd = 2,
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps",
       main = "Average Number of Steps Taken (across all weekday days or weekend days)")
```

![plot of chunk unnamed-chunk-88](figure/unnamed-chunk-88-1.png)







