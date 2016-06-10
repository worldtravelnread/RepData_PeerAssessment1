## Reproducible Research - Project 1

## Installing packages and loading libraries
if(!require(dplyr, quietly = TRUE)) install.packages("dplyr")
library(dplyr)

if(!require(lubridate, quietly = TRUE)) install.packages("lubridate")
library(lubridate)

if(!require(ggplot2, quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

##workingDirChar <- "C:/Users/b1050549/Documents/RResearch_project1"
workingDirChar <- "~/Documents/DataScience_JH/ReproducibleResearch/project1/RepData_PeerAssessment1"
setwd(workingDirChar)

## clear the environment
rm(list = ls())

## Loading and preprocessing the data

## 1. Load the data
## fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
## download.file(fileUrl, destfile = "activity.zip", method = "curl")
## dateDownloaded <- now()

unzip("activity.zip")
activityOrigDf <- read.csv("activity.csv", header = TRUE)

## 2. Process/transform the data (if necessary) into a format suitable 
## for your analysis.
activityOrigDfTbl <- tbl_df(activityOrigDf)
print(activityOrigDfTbl)
summary(activityOrigDfTbl)

## What is mean total number of steps taken per day?

## 1. Calculate the total number of steps taken per day.
## 3. Calculate and report the mean and median of the total number of steps 
## taken per day.
totalStepsByDay <- activityOrigDfTbl %>%
        group_by(date) %>%
        summarize(totalSteps = sum(steps, na.rm = TRUE),
                  mean = mean(steps, na.rm = TRUE),
                  median = median(steps, na.rm = TRUE))
print(totalStepsByDay)

## 2. Make a histogram of the total number of steps taken each day.
plot1 <- hist(totalStepsByDay$totalSteps, xlab = "Total Steps Each Day",
     main = "Plot 1: Frequency Distribution of Total Steps Taken Each Day")



## What is the average daily activity pattern?
## Make a time series plot of the 5-minute interval (x-axis) and the 
## average number of steps taken, averaged across all days (y-axis).
avgStepsByInterval <- activityOrigDfTbl %>%
        group_by(interval) %>%
        summarize(avgSteps = mean(steps, na.rm = TRUE), totalSteps = 
                          sum(steps, na.rm = TRUE))
print(avgStepsByInterval)

plot2 <- with(avgStepsByInterval, plot(interval, avgSteps, type = "l", 
        xlab = "Time Interval", ylab = "Average Number of Steps",
        main = "Plot 2: Average Number of Steps by Time Interval"))

summary(avgStepsByInterval)
maxInterval <- avgStepsByInterval %>%
        filter(avgSteps == max(avgSteps))
print(maxInterval)

## Imputing missing values
## 1. Calculate and report the total number of missing values in the dataset 
## (i.e., the total number of rows with **NA**s).
cntNAs <- length(which(is.na(activityOrigDfTbl$steps)))

## get the dates where steps are NA
daysNA <- totalStepsByDay %>%
        filter(is.nan(mean))
daysNA

## 2. Devise a strategy for filling in all of the missing values in the 
## dataset. The strategy does not need to be sophisticated. For example, you 
## could use the mean/median for that day, or the mean for that 5-minute 
## interval, etc.

## create an empty vector to store the returned values
imputedVectNum <- vector(mode = "numeric", length = 0)
stepTypeVect <- character(length = 0)
## get the total number of observations for the for loop
totalObservations <- as.integer(count(activityOrigDfTbl))
## loop through all the observations in the data set
for(i in 1: totalObservations) {
        ## get the value of the steps column
        test <- activityOrigDfTbl[i, "steps"]
        ## If the value of steps is NA
        if(is.na(test)) {
                ## get the value of the interval column as an integer
                y <- as.integer(activityOrigDfTbl[i, "interval"])
                ## get the row in avgStepsByInterval for the interval
                x <- filter(avgStepsByInterval, interval == y)
                ## get the avgSteps for that interval as a number
                z <- as.numeric(x$avgSteps)
                ## append the avgSteps to imputedVectNum
                imputedVectNum <- c(imputedVectNum, z)
                ## assign the stepType as "Imputed"
                l <- "Imputed"
                ## append the stepType to the vector
                stepTypeVect <- c(stepTypeVect, l)
        } else {
                ## get value of the steps columns as a number
                z <- as.numeric(activityOrigDfTbl[i, "steps"])
                ## append the number of steps to imputedVectNum
                imputedVectNum <- c(imputedVectNum, z)
                ## assign the stepType as "Original"
                l <- "Original"
                ## append the stepType to the vector
                stepTypeVect <- c(stepTypeVect, l)
        }
} 

imputedStepsVect <- imputedVectNum
head(imputedStepsVect)
head(stepTypeVect)

## 3. Create a new dataset that is equal to the original 
## dataset but with the missing data filled in.

## add the imputedSteps and stepType vectors to the activity data
activityImputedDfTbl <- activityOrigDfTbl %>%
        mutate(imputedSteps = imputedStepsVect, stepType = stepTypeVect)
head(activityImputedDfTbl)
summary(activityImputedDfTbl)


## 4. Make a histogram of the total number of steps taken each 
## day, and calculate and report the **mean** and **median** 
## total number of steps taken per day.

## summarize imputedSteps by date
imputedByDate <- activityImputedDfTbl %>%
        group_by(date) %>%
        summarize(totalImputedSteps = sum(imputedSteps), avgImputedSteps 
                = mean(imputedSteps), medianImputedSteps = 
                median(imputedSteps), totalOrigSteps = 
                sum(steps, na.rm = TRUE), avgOrigSteps = 
                mean(steps, na.rm = TRUE), medianOrigSteps =
                median(steps, na.rm = TRUE)) %>%
        mutate(deltaTotal = totalImputedSteps - totalOrigSteps, 
                deltaAvg = avgImputedSteps - avgOrigSteps, 
                deltaMedian = medianImputedSteps - medianOrigSteps)
head(imputedByDate)

## create histogram
plot3 <-hist(imputedByDate$totalImputedSteps, xlab = "Total Imputed Steps Each Day", 
     main = "Plot 3: Frequency Distribution of Total Imputed Steps Taken Each Day")



## Do these values differ from the estimates from the first 
## part of the assignment? 

## get the differences calcultaed in imputedByDate for the days with NAs.
differencesDfTbl <- imputedByDate %>%
        filter(is.nan(deltaAvg)) %>%
        select(date, deltaTotal, deltaAvg, deltaMedian)
print(differencesDfTbl)


## What is the impact of imputing 
## missing data on the estimates of the total daily number 
## of steps?
                  
## byIntervalByStepType <- activityImputedDfTbl %>%
##        group_by(interval, stepType) %>%
##        summarize(totalOrigSteps = sum(steps, na.rm = TRUE), 
##                  totalImputedSteps = sum(imputedSteps))

## head(byIntervalByStepType)

## plot4 <- ggplot(byIntervalByStepType, aes(interval, totalImputedSteps))
## plot4 <- plot4 + geom_line(aes(color = stepType)) + labs(x = 
##                "Interval", y = "Total Steps",
##                title = "Comparison of Total Original and Imputed Steps ")
## print(plot4)

## byDateByStepType <- activityImputedDfTbl %>%
##        group_by(date, stepType) %>%
##        summarize(totalOrigSteps = sum(steps, na.rm = TRUE), 
##                  totalImputedSteps = sum(imputedSteps))
## head(byDateByStepType)



## plot4a <- qplot(totalImputedSteps, data = byDateByStepType, fill = stepType,
##                binwidth = 150)
## print(plot4a)

totalStepsByInterval <- activityImputedDfTbl %>%
        group_by(interval) %>%
        summarize(totalOrigSteps = sum(steps, na.rm = TRUE), 
                  totalImputedSteps = sum(imputedSteps))
head(totalStepsByInterval)

## plot4 <- with(totalStepsByInterval, plot(interval, totalOrigSteps, pch = 19,
##        xlab = "Time Interval", ylab = "Total Steps",
##        main = "Plot 4: Comparison of Total Number of Original and Imputed Steps"),
##        col = rgb(0, 0, 0, 0.15))
## plot4 <- with(totalStepsByInterval, points(interval, totalImputedSteps, pch = 19,
##        col = rgb(0, 0, 1, 0.50)))
## plot4 <- legend(x=2000, y = 10000, pch = 19, cex = 1, col = c("black", "blue"), 
##        legend = c("Original", "Imputed"), bty = "n")


plot4 <- ggplot(totalStepsByInterval, aes(interval, totalImputedSteps))
plot4 <- plot4 + geom_line(aes(color = "Imputed"))  + geom_line(aes(interval, 
        totalOrigSteps, color = "Original")) + labs(x = "Interval", 
        y = "Total Steps",
        title = "Plot 4: Comparison of Total Original and Imputed Steps ") +
        theme(legend.position = "right")
print(plot4)



## Are there differences in activity patterns between weekdays and weekends?

## 1.Create a new factor variable in the dataset with two levels 
## -- "weekday" and "weekend" indicating whether a given date is 
## a weekday or weekend day.


## create a table with the weekday
wkdayActivityImputedDfTbl <- activityImputedDfTbl %>%
        mutate(weekday = weekdays(as_date(date)))
head(wkdayActivityImputedDfTbl)
               

## assign an empty vector
dayTypeVect <- vector("character", length = 0)
## get the total number of observations for the loop
totalObservations <- as.integer(count(wkdayActivityImputedDfTbl))
## loop through all the days
for(j in 1:totalObservations) {
        ## get the weekday
        day <- wkdayActivityImputedDfTbl[j, "weekday"]
        ## test if the day is Saturday or Sunday
        if(day == "Saturday" | day == "Sunday") {
                a <- "Weekend"
                dayTypeVect <- c(dayTypeVect, a)
        } else {
                a <- "Weekday"
                dayTypeVect <- c(dayTypeVect, a)
        }
        dayTypeVect
}

## add dayTypeVect as a column in data frame
dayTypeActivityImputedDfTbl <- wkdayActivityImputedDfTbl %>%
        mutate(dayType = dayTypeVect)
head(dayTypeActivityImputedDfTbl)

## 2.Make a panel plot containing a time series plot (i.e.  
## type = "l" ) of the 5-minute interval (x-axis) and the 
## average number of steps taken, averaged across all weekday 
## days or weekend days (y-axis). 

byIntervalByDayType <- dayTypeActivityImputedDfTbl %>%
        group_by(interval, dayType) %>%
        summarize(avgDayTypeImputedStep = mean(imputedSteps))

plot5 <- ggplot(byIntervalByDayType, aes(interval, avgDayTypeImputedStep))
plot5 <- plot5 + geom_line() + facet_grid(dayType ~ .) + labs(title = 
                "Plot 5: Average Number of Imputed Steps by Time Interval", 
                x = " Time Interval", y = "Number of Imputed Steps")
print(plot5)




