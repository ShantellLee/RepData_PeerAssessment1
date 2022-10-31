## 1. Loading and processing data
unzip("./activity.zip")
activityData <- read.csv("./activity.csv")
summary(activityData)

    #Delete the NA observations
raw_data <- read.csv("activity.csv", header = TRUE)
main_data <- na.omit(raw_data)
summary(main_data) ##check NA observations were deleted

## 2. What is the mean total number of steps taken per day
steps_per_day <- aggregate(main_data$steps, 
                           by = list(Step.Data = main_data$date), FUN = "sum")

      #Creating a histogram
hist(steps_per_day$x, col = "green",
     breaks = 20, 
     main = "Total number of steps each day",
     xlab = "Number of steps per day")

    #Calculating the mean and median for the total steps per day
mean_steps <- mean(steps_per_day[,2])
print (mean_steps)

median_steps <- median(steps_per_day[,2])
print (median_steps)

## 3. What is the average daily activity pattern?

steps_per_interval<-aggregate(steps~interval, data=activityData, mean, 
                              na.rm=TRUE)
plot(steps~interval, data=steps_per_interval, type="l", 
     main = "Average daily activity pattern", 
     ylab = "Avarage number of steps taken", 
     xlab = "5-min intervals")

    #interval with the maximum number of steps
interval_with_max_steps <- steps_per_interval[which.max(steps_per_interval$steps),
                                           ]$interval
interval_with_max_steps

## Imputing missing values 
    #Calculating total NA
total_values_missings <- sum(is.na(activityData$steps))
total_values_missings

    # Strategy for filling in all missing values
get_mean_steps_per_interval<-function(interval){
  steps_per_interval[steps_per_interval$interval==interval,]$steps
}

  # Create new data set
activityDataNoNA<-activityData
for(i in 1:nrow(activityDataNoNA)){
  if(is.na(activityDataNoNA[i,]$steps)){
   activityDataNoNA[i,]$steps <- get_mean_steps_per_interval(activityDataNoNA[i,]$interval)
  }
  }

    # New dataset with no missing values 
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps,
     main = "Total Steps Per Day:with no missing values")

    #Calculating the mean and median
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)

## 4. Are there differences in activity patterns between weekends and weekdays?
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
  if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
    activityDataNoNA[i,]$day<-"weekend"
  }
  else{
    activityDataNoNA[i,]$day<-"weekday"
  }
}
steps_by_day <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval 
                        + activityDataNoNA$day, activityDataNoNA, mean)

names(steps_by_day) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, steps_by_day, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
