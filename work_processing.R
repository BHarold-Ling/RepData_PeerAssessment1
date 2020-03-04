## Work script to gather code

library(dplyr)
library(ggplot2)

# Unzip

unzip("activity.zip")

activity <- read.csv("activity.csv", col.names = c("Steps", "Date", "Interval"))
str(activity)
summary(activity)


daily <- activity %>%
    group_by(Date) %>%
    summarize(Daily_Steps = sum(Steps))


interval <- activity %>%
    group_by(Interval) %>%
    summarize(Average_Steps = mean(Steps, na.rm = TRUE))


plot1 <- ggplot(daily, aes(Daily_Steps)) + geom_histogram(na.rm = TRUE)
print(plot1)

print(sprintf("Mean steps per day:   %d", mean(daily$Daily_Steps, na.rm = TRUE)))
print(sprintf("Median steps per day: %d", median(daily$Daily_Steps, na.rm = TRUE)))



ggplot(interval, aes(Interval, Average_Steps)) + geom_line(na.rm = TRUE)

maxintv <- which.max(interval$Average_Steps)
interval$Average_Steps[maxintv]
interval$Interval[maxintv]

nrow(activity)
nrow(filter(activity, is.na(Steps)))
nrow(filter(activity, is.na(Date)))
nrow(filter(activity, is.na(Interval)))

x <- activity$Steps[!complete.cases(activity)]
length(x)

mean(daily2$Daily_Steps, na.rm = TRUE)
median(daily2$Daily_Steps, na.rm = TRUE)


dim(activity)[1]

activity_f <- activity %>%
    mutate()

a1 <- inner_join(activity, interval, by = "Interval")
a1$Steps[is.na(a1$Steps)] <- a1$Average_Steps[is.na(a1$Steps)]


daily2 <- a1 %>%
    group_by(Date) %>%
    summarize(Daily_Steps = sum(Steps))


plot2 <- ggplot(daily2, aes(Daily_Steps)) + geom_histogram(binwidth = 2000, na.rm = TRUE)
print(plot2)

a2 <- activity[is.na(activity$Steps), ]
table(a2$Date)
table(activity$Date)


weekend <- c("Sat", "Sun")

weektime <- mutate(a1, Weekpart = weekdays(as.Date(Date), abbreviate = TRUE),
                   Weekpart = if_else(Weekpart %in% weekend, "weekend", "weekday"),
                   Weekpart = as.factor(Weekpart))

interval_wt <- weektime %>%
    group_by(Weekpart, Interval) %>%
    summarize(Average_Steps = mean(Steps, na.rm = TRUE))


ggplot(interval_wt, aes(Interval, Average_Steps)) + geom_line(na.rm = TRUE) + facet_grid(Weekpart ~ .)
