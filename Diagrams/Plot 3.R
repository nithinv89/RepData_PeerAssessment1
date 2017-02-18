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

png("Plot 3.png")
qplot(Tsteps, binwidth = 2000, xlab = "Total number of steps taken each day", main = "Histogram showing the number of steps taken each day")

dev.off()
