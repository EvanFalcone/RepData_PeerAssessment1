---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

``` {r loaddata}
setwd("/Users/estevanfalcone/CODING PRACTICE PROJECTS/R Projects/")
library("dplyr")
library("lubridate")
library("ggplot2")

### Create the data frame as 'df':
df <- read.csv("activity.csv")
df$date <- lubridate::ymd(df$date)
```

## What is mean total number of steps taken per day?

``` {r meansteps}
# aggregate:
dfGroup <- df %>% group_by(date)
stepsPerDay <- summarise(dfGroup, sum(steps, na.rm = T))

# 1. plot histogram:
qplot(stepsPerDay$`sum(steps, na.rm = T)`, binwidth = 1000,
      xlab = "Total Number of Steps taken per Day")

# 2. mean & medium:
mean(stepsPerDay$`sum(steps, na.rm = T)`, na.rm = T)
median(stepsPerDay$`sum(steps, na.rm = T)`, na.rm = T)
```

## What is the average daily activity pattern?

``` {r dailyactiv}
# aggregate:
dfGroupAvg <- df %>% group_by(interval)
avgSteps <- summarise(dfGroupAvg, mean(steps, na.rm = T))

# 1. plot time series:
ggplot(data=avgSteps, aes(x=interval, y=avgSteps$`mean(steps, na.rm = T)`)) +
  geom_line() +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps taken")

# 2. max number of steps:
avgSteps[which.max(avgSteps$`mean(steps, na.rm = T)`),]
```

## Imputing missing values

``` {r inputMissing}
# 1. Assuming we're looking for NAs in all columns of 'df':
nrow(subset(df, is.na(df$steps)|is.na(df$date)|is.na(df$interval)))

# 2. & 3. Create new dataset with NAs filled by mean of corresponding 5-min interval:
dfFilled <- df %>%
  group_by(interval) %>% 
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = T), steps))

# 4. plot histogram + mean & medium updated:

# aggregate:
dfGroupNa <- dfFilled %>% group_by(date)
stepsPerDayNa <- summarise(dfGroupNa, sum(steps, na.rm = T))

qplot(stepsPerDayNa$`sum(steps, na.rm = T)`, binwidth = 1000,
      xlab = "Total Number of Steps taken per Day")

mean(stepsPerDayNa$`sum(steps, na.rm = T)`, na.rm = T)
median(stepsPerDayNa$`sum(steps, na.rm = T)`, na.rm = T)
```

Mean and median values are higher after filling the missing data. The reason is,
in the original data, there are some days with `steps` values `NA` for 
a given `interval`. The total # of steps in such days are set to 0 by
default. However, after replacing missing `steps` values with the mean `steps`
of the corresponding `interval`, these 0 values are removed from the total number
of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?

``` {r weekdaysEnds}
# 1. Add weekdays and weeknights column to dfFilled:

wDayVsWEnd <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop(NA)
}

dfFilled[,"day"] <- sapply(dfFilled$date, FUN = wDayVsWEnd)

# 2. Plot average steps taken on weekdays and weekends respective vs 5 min intervals:

# aggregate:
dfGroupAvgFill <- dfFilled %>% group_by(interval, day)
avgStepsFill <- summarise(dfGroupAvgFill, mean(steps))

# plot time series weekday vs weekend:
ggplot(data=avgStepsFill, aes(interval, `mean(steps)`))  +
  geom_line() +
  facet_grid(day ~ .) +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps taken Filled")
  ```
