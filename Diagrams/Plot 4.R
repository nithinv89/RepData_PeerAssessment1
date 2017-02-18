library(ggplot2)
read <- read.csv(file = "activity.csv")

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
Tsteps <- tapply(filler.new$steps, filler.new$date, FUN = sum)

filler.new$daytype <- ifelse(as.POSIXlt(filler.new$date)$wday %in% c(0,6), 'weekend', 'weekday')
final <- aggregate(steps ~ interval + daytype, filler.new, mean)

png("Plot 4.png")
ggplot(final, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .)

dev.off()


