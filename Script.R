setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(scales)
library(Hmisc)
activity_data <- read.csv("activity.csv")


tot_number_steps_day <- sum(activity_data$steps, na.rm = TRUE)/length(unique(activity_data$date))
print(tot_number_steps_day)


library(dplyr)
by_date <- group_by(activity_data, date)




StepsPerDay <- summarise(by_date, sum(steps))

hist(StepsPerDay$`sum(steps)`,
     breaks = 10,
     main = "Number of Steps per Day",
     xlab = "Steps per day",
     ylab = "Frequency")


# mean number of steps

print(StepsPerDay)

mean_stepsPerDay <- mean(StepsPerDay$`sum(steps)`, na.rm= TRUE)

print(mean_stepsPerDay)

median_stepsPerDay <- median(StepsPerDay$`sum(steps)`, na.rm = TRUE)

print(median_stepsPerDay)

## Average daily activity pattern


stepsPerInterval <- group_by(activity_data, interval)
print(stepsPerInterval)

average_stepsPerInterval <- summarise(stepsPerInterval, mean(steps, na.rm = TRUE))
print(average_stepsPerInterval)

plot(average_stepsPerInterval, type="l", main="Average number of steps per each Interval",
     xlab="Intervals",
     ylab="Avg number of steps")


maximum_interval_i <- which(average_stepsPerInterval[,2] == max(average_stepsPerInterval[,2]))
maximum_interval <- average_stepsPerInterval[maximum_interval_i, 1]
print((maximum_interval))


## Imputing missing values

# As it turns out, NA's only occur on a daily basis or not at all. 
total_missing <- sum(is.na(activity_data$steps))
print(total_missing)


## For a given day, it is either NA or not. It cannot contain intervals with NA and some without for the same day.

## Therefore, replacing it with average values can be done
num_naPerDay <- summarise(by_date, sum(is.na(activity_data$steps)))
unique(num_naPerDay[,2])


sum(is.na(StepsPerDay[,2]))



## Replace NA by average

activity_data_imputed <- activity_data
activity_data_imputed$steps <- impute(activity_data$steps, fun=mean)

##histogram
steps_perDayImputed <- group_by(activity_data_imputed, date)
stepsPerDayImputed <- summarise(steps_perDayImputed, sum(steps))

hist(stepsPerDayImputed$`sum(steps)`, breaks=10, main="Total number of steps (Replacing NA's)",
     xlab="Number of steps per day",
     ylab="Frequency")


mean(stepsPerDayImputed$`sum(steps)`)

median(stepsPerDayImputed$`sum(steps)`)


##
#The mean does not differ from the estimate from the first part of the assignment, because adding the average of a vector any times does not change the average of the resulting verctor.

#The median has changed, because the number of days was changing.

#Replacing missing data by average values increases the total daily number of steps and corrects it: The calculation in part (1) was incorrect, bacause all 61 days of October and November contributed the denominator, while for 8 of them the device has not been used.

#The total daily number of steps is now equal to the average of the total number of steps taken per day (4.1).



#Differences in activity patterns between weekdays and weekends


activity_data_imputed$dateType <-  ifelse(as.POSIXlt(stepsPerDayImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')

activity_data_imputed


# ow histograms for weekends and weekdays

weekday_interval <- group_by(activity_data_imputed, interval, dateType)

average_stepsPerIntervalWeekday <- summarise(weekday_interval, mean(steps))
average_stepsPerIntervalWeekday


par(mfrow = c(1, 2), mar = c(4, 4, 6, 1))
plot(subset(average_stepsPerIntervalWeekday, dateType == "weekday")$interval,
     subset(average_stepsPerIntervalWeekday, dateType == "weekday")$`mean(steps)`,
     type = "l",
     main = "Weekdays",
     xlab = "Interval",
     ylab = "Average Number of Steps")
plot(subset(average_stepsPerIntervalWeekday, dateType == "weekend")$interval,
     subset(average_stepsPerIntervalWeekday, dateType == "weekend")$`mean(steps)`,
     type = "l",
     main = "Weekends",
     xlab = "Interval",
     ylab = "Average Number of Steps")
title(main="Average Number of Steps taken in Interval", outer = T)