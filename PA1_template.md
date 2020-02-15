---
title: '"Reproducible Research: Peer Assessment 1"'
author: '"Dr-A-Soni'
date: "15/02/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## **Loading and Processing the Data:**
### *1. Read data into memory via read.csv*
### *2. Change Date from String to Date(IF necessary)*
```{r}
Task <- read.csv("C://Users/chinks/Downloads/coursera/repdata_data_activity/activity.csv")
dim(Task)

## Loading and Processing the Data:
head(Task$date)
str(Task)
summary(Task)

```
## **What is the mean of the total number of steps taken per day?**
###  *a) Calculate the total number of steps taken per day?*
```{r}
library(ggplot2)
library(dplyr)
## i)Calulating the total number steps taken each day:
Steps_daily <- Task %>% group_by(Task$date) %>% summarize(tSteps = sum(steps, na.rm = TRUE))
head(Steps_daily) 
summary(Steps_daily)
```
###  *b) Make a histogram of the total number of steps taken each day?*
```{r}
hist(Steps_daily$tSteps, xlab = "Steps of the day",
     main = "histogram of total steps taken on each day", ylim = c(0,30),
     col = "red")
```

###  *c) Calculate and report the mean and median of the total number of steps taken per day?*
```{r}
Task_mean_preNA <- mean(Steps_daily$tSteps)            
Task_median_PreNA <- median(Steps_daily$tSteps)
Task_mean_preNA           
Task_median_PreNA
```
## **What is the average daily activity pattern?**
###  *Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days (y-axis)?*
```{r}
##Time series plot :  
Steps_Interval <- Task %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm=TRUE))
head(Steps_Interval)
```
### *Creating two different plots showing same result:*
```{r}
ggplot(Steps_Interval, aes(x= interval, y = steps)) + geom_line() + 
    ggtitle("Time series plot for Average steps taken each day")

```
## **The 5-minute interval that, on average, contains the maximum number of steps**
### *On average across all the days in the dataset, the 5-minute interval contains the maximum number of steps?*

```{r}
Steps_Interval[which(Steps_Interval$steps == max(Steps_Interval$steps)), ]
summary(Steps_Interval)
```
## *Creating two different plots using both the plotting system showing similar results:*
``` {r}
ggplot(Steps_Interval, aes(x= interval, y = steps), col = "red") + 
    geom_line() + xlab("5-mintue Interval") +  ylab ("Average number of Intervals") + ggtitle("Steps by Time Interval")
```
##   **Imputing missing values**
###  *a)Calculate and report the total number of missing values in the dataset.*
```{r}
Missing_Value <- sum(is.na(Task$steps))
head(Missing_Value)
summary(Missing_Value)
## 1. Calculate and report the total number of missing values in the dataset:
## The Total number of NA's in the Task dataframe are:
print(paste("Total number of rows with NA is :", sum(is.na(Task))))
## calculating the mean :
Mean_Steps <- mean(Steps_Interval$steps, na.rm = TRUE)
Mean_Steps
```
###  *b)Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.* 
```{r}
## Checking the data with NA's value before inputing the missing values:
head(Task, 10)
## NA's Strategy: for inputing the missing value:
Task_NA <- Task 
for(i in 1:nrow(Task)){
    if(is.na(Task$steps[i])){
        Task_NA$steps[i] <- Steps_Interval$steps[Task_NA$interval[i] == Steps_Interval$interval]
    }
}

```
###  *c) Checking the data with the missing data filled in.*
```{r}
head(Task_NA)
summary(Task_NA)
str(Task_NA)
```
###  *d) Make a histogram of the total number of steps taken each day.*
```{r}
## i) Firstly calcultaing the daily steps taken with missing values inputed.
Step_NA_Daily <- Task_NA %>% group_by(date) %>% summarise(Tsteps = sum(steps))
head(Step_NA_Daily)
## ii) ploting the histogram with the missing values.

hist(Step_NA_Daily$Tsteps, xlab= "Steps", col = "yellow", main = "Histogram of daily steps", ylim= c(0,40))
```
###   *e) Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*
    
```{r}
## calculating the mean and median of the data with the inputed missign values.
Task_NA_mean <- mean(Step_NA_Daily$Tsteps)
Task_NA_median <- median(Step_NA_Daily$Tsteps)
## Pre and Post results of adding missing value and finding the difference in the mean and median data:
NA_Compare <- data.frame(mean= c(Task_mean_preNA, Task_NA_mean), median = c(Task_median_PreNA, Task_NA_median))
rownames(NA_Compare) <- c("Pre Transformation", "Post transformation")
print(NA_Compare)
```
##  **Are there differences in activity patterns between weekdays and weekends?**
###   *1) Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.*
```{r}
Task_DoW <- Task_NA
Task_DoW$date <- as.Date(Task_DoW$date)
Task_DoW$day <- ifelse(weekdays(Task_DoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
Task_DoW$day <- as.factor(Task_DoW$day)
head(Task_DoW$day)
str(Task_DoW$day)
summary(Task_DoW$day)
Task_Weekday <- filter(Task_DoW, Task_DoW$day == "weekday")
Task_Weekend <- filter(Task_DoW, Task_DoW$day == "weekend")
Task_WKDay <- Task_Weekday %>% group_by(interval) %>% summarise(Steps = mean(steps))
Task_WKDay$day <- "Weekday"
Task_WKEnd <- Task_Weekend %>% group_by(interval) %>% summarise(Steps = mean(steps))
Task_WKEnd$day <- "WeekEnd"

## combining both the rows 
WholeWeek <- rbind(Task_WKDay, Task_WKEnd)
WholeWeek$day <- as.factor(WholeWeek$day)
dim(WholeWeek)
summary(WholeWeek)
```

###  *2) Make a panel plot containing a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*
```{r}
ggplot(WholeWeek, aes(interval,Steps)) + geom_line()+ facet_grid(day~.) + 
    ggtitle("Average Number of Steps - Weekday vs. Weekend") +labs(y = "Number of Steps") + labs(x = "Interval")
```
