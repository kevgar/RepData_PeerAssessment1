# Reproducible Research: Peer Assessment 1

**Setup environment**

```r
rm(list=ls())
library("data.table"); library("dplyr"); library("timeDate"); library("R.devices")
```

```
## Warning: package 'R.devices' was built under R version 3.2.3
```

```r
setwd('~/GitHub/05_ReproducableResearch/RepData_PeerAssessment1')
```
**Load data**

```r
f <- file.path(getwd(),"activity.zip") %>% unzip()
DT_activity<-data.table(read.csv(file.path(getwd(),"activity.csv")))
```
Make `interval` an index from `1:288` to number the 5-minute intervals in a day.

```r
DT_activity[,interval:=c(1:288)]
```
## What is mean total number of steps taken per day?
Calculate total steps per day

```r
DT_activity <- group_by(DT_activity,date) #group data
DT_totalSteps <- summarise(DT_activity, sum(steps)) %>% setnames(c("date","totalSteps")) 
summarise(ungroup(DT_activity), sum(steps)) #ungroup data
```

```
## Source: local data table [1 x 1]
## 
##   sum(steps)
##        (int)
## 1         NA
```
Generate histogram of the total number of steps taken per day

```r
with(DT_totalSteps,
        hist(totalSteps, 
                main="Histogram of Steps Taken Per Day",
                xlab="Number of steps taken per day"
        )
)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)\
Calculate and report the mean and median of the total number of steps taken per day

```r
summarise(DT_totalSteps, mean(totalSteps,na.rm=T), median(totalSteps,na.rm=T)) %>% 
  setnames(c("mean","median")) %>% data.table()
```

```
##        mean median
## 1: 10766.19  10765
```
## What is the average daily activity pattern?
Calculate the mean steps taken by interval

```r
DT_activity <- group_by(DT_activity,interval) #group data
summarise(DT_activity, mean(steps)) %>% setnames(c("interval","averageSteps")) %>% head(3)
```

```
##   interval averageSteps
## 1        1           NA
## 2        2           NA
## 3        3           NA
```
Note the values for `mean(steps)` are all NA. This tells us there is at least one incomplete case within each interval. To fix this we specify `na.rm=TRUE` within the call to `mean()`.

```r
DT_activityPattern <- summarise(DT_activity, mean(steps,na.rm=TRUE)) %>% 
  setnames(c("interval","averageSteps"))
head(DT_activityPattern, 3)
```

```
##   interval averageSteps
## 1        1    1.7169811
## 2        2    0.3396226
## 3        3    0.1320755
```

```r
summarise(ungroup(DT_activity), mean(steps)) #ungroup data
```

```
## Source: local data table [1 x 1]
## 
##   mean(steps)
##         (dbl)
## 1          NA
```
Plot a time series with average steps taken (y-axis) for each 5-minute intervals (x-axis). 

```r
with(DT_activityPattern,
        plot(interval, averageSteps,
                main="Average Daily Activity Pattern",
                xlab="Interval",
                ylab="Average steps",
                type="l"
        )
) 
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)\
**Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**  
Calculate the max of `averageSteps` and the interval at which it occurs

```r
summarize(DT_activityPattern, which.max(averageSteps), max(averageSteps)) %>% 
  setnames(c("interval","max")) %>% data.table()
```

```
##    interval      max
## 1:      104 206.1698
```
## Imputing missing values
Count the number of rows containing NA values

```r
summarize(DT_activity, sum(is.na(steps))) %>% setnames(c("NAs")) %>% data.table()
```

```
##     NAs
## 1: 2304
```
**First approach**
Fill NA values using the mean

```r
for (col in c("steps")) DT_activity[is.na(get(col)), (col) := mean(DT_activity$steps,na.rm=TRUE)]
summarize(DT_activity, sum(is.na(steps))) %>% setnames(c("NAs")) %>% data.table()
```

```
##    NAs
## 1:   0
```
Calculate the total number of steps per day using filled data

```r
DT_activity <- group_by(DT_activity,date) #group data
DT_totalSteps <- summarise(DT_activity, sum(steps)) %>% setnames(c("date","totalSteps")) 
summarise(ungroup(DT_activity), sum(steps)) #ungroup data
```

```
## Source: local data table [1 x 1]
## 
##   sum(steps)
##        (int)
## 1     655856
```
Generate histogram of the total number of steps taken per day

```r
with(DT_totalSteps,
        hist(totalSteps, 
                main="Histogram of Steps Taken Per Day",
                xlab="Number of steps taken per day"
        )
)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)\
Calculate and report the mean and median of the total number of steps taken per day

```r
summarise(DT_totalSteps, mean(totalSteps),median(totalSteps)) %>% 
  setnames(c("mean","median")) %>% data.table()
```

```
##        mean median
## 1: 10751.74  10656
```
**Do these values differ from the estimates from the first part of the assignment?**
Yes. The mean and median decreased by `14.45` steps and `109` steps, respectively. 

## Are there differences in activity patterns between weekdays and weekends?

Calculate the mean steps taken by interval for weekdays and weedends

```r
DT_activity <- DT_activity[,wday:=isWeekday(DT_totalSteps$date, wday=1:5)] #create wday variable
DT_wdays <- DT_activity[wday==TRUE] %>% #filter rows where wday==TRUE
  group_by(interval) %>% #group data
  summarise(mean(steps)) %>% #calculate mean steps by interval
  setnames(c("interval","averageSteps")) %>%
  setorder(interval)
DT_wends <- DT_activity[wday==FALSE] %>% #filter rows where wday==FALSE
  group_by(interval) %>% #group data
  summarise(mean(steps)) %>% #calculate mean steps by interval
  setnames(c("interval","averageSteps")) %>%
  setorder(interval)
```
Create a panel plot comparing the average steps taken per 5-minute interval across weekdays and weekends.

```r
with(DT_wdays,
     plot(interval, averageSteps,main="Average Daily Activity Pattern - Weekdays",
          xlab="Interval", ylab="Average steps",
          type="l", col="red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)\

```r
with(DT_wends,
     plot(interval, averageSteps,main="Average Daily Activity Pattern - Weekends",
          xlab="Interval", ylab="Average steps",
          type="l", col="blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-17-2.png)\