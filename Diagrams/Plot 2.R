
library(ggplot2)
read <- read.csv(file = "activity.csv")
steps_interval <- aggregate(steps ~ interval, read, mean)
png("Plot 2.png")
plot(steps_interval$interval, steps_interval$steps, type = "l", main = "Average Steps / Day by Interval", xlab = "Interval", ylab = "Number of Steps")
dev.off