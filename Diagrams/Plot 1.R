
library(ggplot2)
read <- read.csv(file = "activity.csv")
steps_date <- aggregate(steps ~ date, read, sum)
png("Plot 1.png")
hist(steps_date$steps, main = "Total Number of Steps Each Day", col = "green", xlab = "Number of Steps")
dev.off()
