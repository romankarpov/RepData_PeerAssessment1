# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr)
library(ggplot2)
activityData <- read.csv(file = unz("activity.zip", "activity.csv"))

sm_by_date <- activityData %>% group_by(date) %>% 
  summarise(total_steps = sum(steps, na.rm = TRUE))

sm_by_interval <- activityData %>% group_by(interval) %>%
  summarise(avg = mean(steps, na.rm = TRUE))
```


## What is mean total number of steps taken per day?
Here is the histogram of total steps

```r
library(ggplot2)
ggplot(data = sm_by_date, aes(sm_by_date$total_steps)) + geom_histogram(binwidth = 5000)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Mean value of total steps

```r
mean(sm_by_date$total_steps)
```

```
## [1] 9354.23
```
 
Median value of total steps:
 

```r
 median(sm_by_date$total_steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
ggplot(data=sm_by_interval, aes(interval, avg)) +
  geom_line(size = 1) 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Let's find five minute interval with maximum steps


```r
max_number_of_steps <- max(sm_by_interval$avg)

print(max_number_of_steps)
```

```
## [1] 206.1698
```

```r
sm_by_interval %>% filter(avg == max_number_of_steps)
```

```
## # A tibble: 1 x 2
##   interval      avg
##      <int>    <dbl>
## 1      835 206.1698
```

## Imputing missing values

Total number of observations with missing vaues

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

Imputing missing values with mean value for a specific interval 

```r
imputed_activity_data <- left_join(activityData, sm_by_interval, by = c("interval", "interval"))
imputed_activity_data <- mutate(imputed_activity_data, imputed_steps = ifelse(is.na(steps), round(avg), steps))
head(imputed_activity_data)
```

```
##   steps       date interval       avg imputed_steps
## 1    NA 2012-10-01        0 1.7169811             2
## 2    NA 2012-10-01        5 0.3396226             0
## 3    NA 2012-10-01       10 0.1320755             0
## 4    NA 2012-10-01       15 0.1509434             0
## 5    NA 2012-10-01       20 0.0754717             0
## 6    NA 2012-10-01       25 2.0943396             2
```

Now make it look like original data set


```r
imputed_activity_data <- select(imputed_activity_data, imputed_steps, date, interval)
colnames(imputed_activity_data)[1]<-"steps"
head(imputed_activity_data)
```

```
##   steps       date interval
## 1     2 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```

Here is the histogram of total steps with imputed data


```r
 library(ggplot2)
 sm_by_date <- imputed_activity_data %>% group_by(date) %>% 
  summarise(total_steps = sum(steps, na.rm = TRUE))
 
ggplot(data = sm_by_date, aes(sm_by_date$total_steps)) + geom_histogram(binwidth = 5000)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Mean value of total steps

```r
mean(sm_by_date$total_steps)
```

```
## [1] 10765.64
```
 
Median value of total steps:
 

```r
 median(sm_by_date$total_steps)
```

```
## [1] 10762
```

You can see that imputing the missing data with averages by time period has changed the distribution and sample mean and median. Both mean and median for imputed data set is larger then original 

## Are there differences in activity patterns between weekdays and weekends?

Let's add a week day / weekend indicator to the impuded data set. 


```r
imputed_activity_data <- mutate(imputed_activity_data,
                                weekday = as.factor(ifelse(weekdays(as.Date(imputed_activity_data$date)) %in% c("Sunday", "Saturday"), "Weekend", "Weekday")))

sm_by_interval_wd <- imputed_activity_data %>% group_by(weekday, interval) %>% summarise(avg = mean(steps))
```

Now let's plot daily activity by for weekdays and weekends


```r
ggplot(data=sm_by_interval_wd, aes(interval, avg)) +
  geom_line(size = 1) + facet_grid(weekday ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
