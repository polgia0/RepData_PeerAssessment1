---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())


```r
df <- read.table(unzip("activity.zip", "activity.csv"),header=T, quote="\"",na.strings = "NA", sep=",")
```
Process/transform the data (if necessary) into a format suitable for your analysis


```r
df$date<-as.Date(df$date, format="%Y-%m-%d")
head(df)
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

Calculate the total number of steps taken per day


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
df_full<-df[!is.na(df$steps),]
dfsteps<-df_full%>% group_by(date) %>% summarise(Tot.Step=sum(steps))
head(dfsteps)
```

```
## # A tibble: 6 x 2
##   date       Tot.Step
##   <date>        <int>
## 1 2012-10-02      126
## 2 2012-10-03    11352
## 3 2012-10-04    12116
## 4 2012-10-05    13294
## 5 2012-10-06    15420
## 6 2012-10-07    11015
```

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(dfsteps$Tot.Step,xlab="Step per day", main="Histogram of Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day


```r
mean(dfsteps$Tot.Step)
```

```
## [1] 10766.19
```

```r
median(dfsteps$Tot.Step)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
dfinterval<-with(df_full,tapply(steps,interval,mean))
plot(dfinterval,type="l",xlab='Interval',ylab='Average Step per Interval', main='Number of Steps vs. Interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(dfinterval)[which.max(dfinterval)]
```

```
## [1] "835"
```


## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
table(df[is.na(df$step),"date"])
```

```
## 
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 2012-11-14 
##        288        288        288        288        288        288        288 
## 2012-11-30 
##        288
```

```r
df[is.na(df$step),"steps"]<-mean(dfsteps$Tot.Step)/288
dfsteps<-df %>% group_by(date) %>% summarise(Tot.Step=sum(steps))
```


```r
hist(dfsteps$Tot.Step,xlab="Step per day", main="Histogram of Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
mean(dfsteps$Tot.Step)
```

```
## [1] 10766.19
```

```r
median(dfsteps$Tot.Step)
```

```
## [1] 10766.19
```
**Effect: simmetry of the distribution is increased.**

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
fday<-function(x) if ((x=='sabato')|(x=='domenica')) return('weekend') else return('weekday')
df$we<-sapply(weekdays(df$date),fday)
df$we<-factor(df$we)
```

Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
dfwe<-subset(df,we=='weekend')
dfwd<-subset(df,we=='weekday')
dfweint<-with(dfwe,tapply(steps,interval,mean))
dfwdint<-with(dfwd,tapply(steps,interval,mean))
dfweint<-data.frame(interval=as.numeric(names(dfweint)),steps=dfweint,day='weekend')
dfwdint<-data.frame(interval=as.numeric(names(dfwdint)),steps=dfwdint,day='weekday')
df1<-rbind(dfweint,dfwdint)
library(lattice)
xyplot(steps ~ interval|day,data=df1,layout=c(1,2),type='l',xlab="Interval",ylab="Number of steps",main= "Steps per Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

**Answer: it seems that in the week end the number of steps are more costant. Probably, people move more at day hour since they do not work.**
