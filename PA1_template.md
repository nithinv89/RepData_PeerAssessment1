
# Loading and Reprocessing the data

###Load the data (i.e. read.csv()
read <- read.csv(file = "activity.csv", header = TRUE, sep = ",")
View(read)
summary(read)
dim(read)



# What is mean total number of steps taken per day?

### Sum steps by date
steps_date <- aggregate(steps ~ date, read, sum)
### Create Histogram
hist(steps_date$steps, main = "Total Number of Steps Each Day", col = "green", xlab = "Number of Steps")
### Calculate Mean and Median
Mean <- mean(steps_date$steps)
Median <- median(steps_date$steps)

![alt tag](https://github.com/nithinv89/RepData_PeerAssessment1/blob/master/Diagrams/Plot%201.png)


# What is the Average Daily Activity Pattern

### Calculate the average steps for each interval
steps_interval <- aggregate(steps ~ interval, read, mean)
### Plot the Average Number of Steps per day with respect to interval
plot(steps_interval$interval, steps_interval$steps, type = "l", main = "Average Steps / Day by Interval", xlab = "Interval", ylab = "Number of Steps")
### Interval with most average steps
max_int <- steps_interval[which.max(steps_interval$steps), 1]
### Highest mean of the number of steps per 5-minute interval

![alt tag](https://github.com/nithinv89/RepData_PeerAssessment1/blob/master/Diagrams/Plot%202.png)


# Imputing missing values

### Calculate and report the total number of missing values in the dataset 
any(is.na(read$steps))
sum(is.na(read$steps))

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc
### Create a new dataset that is equal to the original dataset but with the missing data filled in
fil <- function(steps, interval) {
  filler <- NA
  if (!is.na(steps))
    filler <- c(steps)
  else
    filler <- (steps_interval[steps_interval$interval==interval, "steps"])
  return(filler)
}
filler.new <- read
filler.new$steps <- mapply(fil, filler.new$steps, filler.new$interval)

### Histogram showing the number of steps taken each day
Tsteps <- tapply(filler.new$steps, filler.new$date, FUN = sum)
qplot(Tsteps, binwidth = 2000, xlab = "Total number of steps taken each day", main = "Histogram showing the number of steps taken each day")
mean(Tsteps)
median(Tsteps)

![alt tag](https://github.com/nithinv89/RepData_PeerAssessment1/blob/master/Diagrams/Plot%203.png)


# Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels indicating whether a given date is a weekday or weekend day
filler.new$daytype <- ifelse(as.POSIXlt(filler.new$date)$wday %in% c(0,6), 'weekend', 'weekday')
### Make a panel plot containing a time series plot
final <- aggregate(steps ~ interval + daytype, filler.new, mean)
ggplot(final, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .)

![alt tag](https://github.com/nithinv89/RepData_PeerAssessment1/blob/master/Diagrams/Plot%204.png)
