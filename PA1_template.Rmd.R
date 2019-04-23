#read in the data, first line is header, separate by comma, interpret NA as NA
setwd("~/Documents/coursera/coursera")
x <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")

#convert date variable to date class
x$date <- as.Date(x$date, format = "%Y-%m-%d")

#convert interval variable into a factor
x$interval <- factor(x$interval)

#get rid of all NA values
bad <- is.na(as.character(x$steps))

#print out vector with non na values
good_data <- x[!bad, ]

#split the data into steps and date and calculate the sum
steps <- aggregate(steps ~ date, data = good_data, sum)

#add column names to the data frame that was created
colnames(steps) <- c("date", "steps")

#create a histogram of steps
hist(as.numeric(steps$steps), col = "Red", breaks = 20, 
     main = "Histogram of the total number of steps taken each day",
     xlab = "Number of Steps")

#calculate the mean total number of steps taken per day
mean(steps$steps)

#calculating average daily activity pattern
steps_interval <- aggregate(good_data$steps, by = list(interval = good_data$interval),
                            FUN = mean)

#add column names to the data set
colnames(steps_interval) <- c("interval", "average_steps")

#plot the average daily activity pattern
plot(as.integer(levels(steps_interval$interval)), steps_interval$average_steps, type = "l",
     ylab = "Average Number of Steps", xlab = "Interval",
     main = "Average Daily Activity Pattern", col = "blue")

#find the maximum number of steps in 5 minute interval
max_steps <- max(steps_interval$average_steps)

#output
max_steps

#5 minute interval that contains the max_steps
#which.max gives the one with the max
#call subset of interval and average
#find in steps_interval in intervals
interval_max_steps <- steps_interval[which.max(steps_interval$average_steps), ]$interval

#output
interval_max_steps

#calculate and report the total number of missing values in the dataset
#change into character to add up
sum(is.na(as.character(x$steps)))

#find the sum of na values for the date variable
sum(is.na(as.character(x$date)))

#find the sum of na values for the interval variable
sum(is.na(as.character(x$interval)))

#finding the indices of missing values
na_index <- which(is.na(as.character(x$steps)))
complete_data <- x

#input missing values using the mean for that 5 minute interval
complete_data[na_index, ]$steps <- unlist(lapply(na_index, FUN = function(na_index) {
        steps_interval[x[na_index, ]$interval == steps_interval$interval, ]$average_steps
}))

#make a data frame with the steps taken for each day
steps_each_day <- aggregate(steps ~ date, data = complete_data, sum)

#add the colnames for the new data frame
colnames(steps_each_day) <- c("date", "steps")

#make a histogram of total number of steps taken each day
hist(as.numeric(steps_each_day$steps), breaks = 20, 
     main = "Histogram of the total number of steps taken each day",
     xlab = "Number of Steps",
     col = "red")

#report the mean and median total number of steps taken per day
mean(steps_each_day$steps)
median(steps_each_day$steps)

#create a new factor variable in the dataset to store the day of the week
complete_data$day <- as.factor(weekdays(complete_data$date))
complete_data$weekday <- ifelse(!(complete_data$day %in% c("Saturday", "Sunday")), TRUE, FALSE)

#calculate the average number of steps for the week of the day
weekday <- complete_data[complete_data$weekday, ]
steps_per_int_weekday <- aggregate(weekday$steps, 
                                   by = list(interval = weekday$interval), FUN = mean)
weekend <- complete_data[!complete_data$weekday, ]
steps_per_int_weekends <- aggregate(weekend$steps, 
                                    by = list(interval = weekend$interval), FUN = mean)

#add the column names
colnames(steps_per_int_weekday) <- c("interval", "average_steps")
colnames(steps_per_int_weekends) <- c("interval", "average_steps")

#add a column the state the day
steps_per_int_weekday$day <- "Weekday"
steps_per_int_weekends$day <- "Weekend"

#merge the two together with rbind
week_data <- rbind(steps_per_int_weekends, steps_per_int_weekday)
week_data$day <- as.factor(week_data$day)

library(lattice)
#plot the data averaged across all weekday days or weekend days
xyplot(average_steps ~ interval | day, data = week_data, 
       type = "l", ylab = "Number of Steps", 
       layout = c(1,2))
